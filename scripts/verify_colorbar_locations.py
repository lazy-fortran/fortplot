#!/usr/bin/env python3
"""Compare actual PNG/PDF colorbar rectangles and Matplotlib placement sides."""
import argparse
import json
import re
import subprocess
import xml.etree.ElementTree as ET
import zlib
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image

NUMBER = r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)"
EVENT = re.compile(rf"(?P<color>{NUMBER}\s+{NUMBER}\s+{NUMBER}\s+RG)"
                   rf"|(?P<line>{NUMBER}\s+{NUMBER}\s+m\s+{NUMBER}\s+{NUMBER}\s+l\s+S)"
                   rf"|(?P<rect>{NUMBER}\s+{NUMBER}\s+{NUMBER}\s+{NUMBER}\s+re\s+S)")
LEVELS = np.array([1., 3., 5., 8., 11.])


def pdf_rectangles(path, canvas):
    data = path.read_bytes()
    page = tuple(map(float, re.search(rb"/MediaBox\s*\[([^]]+)\]", data)[1].split()))
    obj = re.search(rb"/Contents\s+(\d+)\s+0\s+R", data)[1]
    start = re.search(rb"\b"+obj+rb" 0 obj\s*<<.*?stream\r?\n", data, re.S)
    length = int(re.search(rb"/Length\s+(\d+)",start[0])[1])
    stream = data[start.end():start.end()+length]
    content = zlib.decompress(stream).decode("latin1")
    color, rectangles, horizontal, vertical = (0., 0., 0.), [], [], set()
    for event in EVENT.finditer(content):
        values = tuple(map(float, re.findall(NUMBER, event[0])))
        if event.lastgroup == "color":
            color = values
        elif color == (0., 0., 0.):
            if event.lastgroup == "rect":
                rectangles.append(values)
            else:
                x1, y1, x2, y2 = values
                if y1 == y2 and abs(x2-x1) > 3:
                    horizontal.append((min(x1,x2), max(x1,x2), y1))
                if x1 == x2 and abs(y2-y1) > 3:
                    vertical.add((x1, min(y1,y2), max(y1,y2)))
    main = max(rectangles, key=lambda r: r[2]*r[3])
    bars = []
    for left, right, y0 in horizontal:
        for lo, hi, y1 in horizontal:
            if lo == left and hi == right and y1 > y0:
                if (left,y0,y1) in vertical and (right,y0,y1) in vertical:
                    bars.append((left,y0,right-left,y1-y0))
    assert len(bars) == 1, (path, "Expected one actual colorbar frame", bars)
    sx, sy = canvas[0]/page[2], canvas[1]/page[3]
    assert abs(sx-sy) < 1e-10, "PDF physical canvas aspect mismatch"
    def image_rectangle(rect):
        x, y, width, height = rect
        return np.array([x*sx, (page[3]-y-height)*sy,
                         (x+width)*sx, (page[3]-y)*sy])
    return image_rectangle(main), image_rectangle(bars[0])


def image_rectangle(rgb, expected_size):
    """Detect a black frame anywhere in the image; dimensions only guide search."""
    dark = rgb.max(axis=2) < 60
    width, height = expected_size
    found = []
    for top in range(dark.shape[0]):
        row = np.r_[False, dark[top], False].astype(int)
        starts = np.flatnonzero(np.diff(row) == 1)
        ends = np.flatnonzero(np.diff(row) == -1)-1
        for start, end in zip(starts, ends):
            # Outward axis ticks extend a frame's horizontal run at its corners.
            if abs(end-start-width) > 12:
                continue
            for bottom in range(max(top+1, round(top+height)-4),
                                min(dark.shape[0], round(top+height)+5)):
                lefts = [x for x in range(start,min(end,start+9)+1)
                         if dark[top:bottom+1,x].mean() >= .9]
                rights = [x for x in range(max(start,end-8),end+1)
                          if dark[top:bottom+1,x].mean() >= .9]
                for left in lefts:
                    for right in rights:
                        if abs(right-left-width) <= 4:
                            if dark[bottom,left:right+1].mean() >= .9:
                                found.append((left,top,right,bottom))
    assert found, "Missing independently detectable black rectangle"
    return np.array(max(found, key=lambda r: (r[2]-r[0])*(r[3]-r[1])),dtype=float)


def side(main, bar):
    if bar[3] < main[1]:
        return "top"
    if bar[1] > main[3]:
        return "bottom"
    if bar[2] < main[0]:
        return "left"
    if bar[0] > main[2]:
        return "right"
    return "overlap"


def reference(canvas, style, location, shrink):
    with plt.rc_context(matplotlib.rcParamsDefault):
        fig, ax = plt.subplots(figsize=(canvas[0]/100,canvas[1]/100),dpi=100)
        x, y = np.linspace(0,2,31), np.linspace(0,1,19)
        z = x[None,:]+10*y[:,None]
        if style == "mesh":
            artist = ax.pcolormesh(x,y,z,cmap="viridis")
        elif style == "filled":
            artist = ax.contourf(x,y,z,levels=LEVELS,cmap="viridis")
        else:
            artist = ax.contour(x,y,z,levels=LEVELS,cmap="viridis")
        cb = fig.colorbar(artist,ax=ax,location=location,fraction=.15,pad=.05,shrink=shrink)
        ax.set(xlim=(0,2),ylim=(0,1))
        fig.canvas.draw()
        def rectangle(axes):
            x0,y0,x1,y1 = axes.get_window_extent().extents
            return np.array([x0,canvas[1]-y1,x1,canvas[1]-y0])
        main, bar = rectangle(ax), rectangle(cb.ax)
        plt.close(fig)
    assert side(main,bar) == location
    return main, bar


def check_label(pdf, rgb, main, bar):
    root = ET.fromstring(subprocess.check_output(["pdftotext","-bbox",str(pdf),"-"]))
    page = next(node for node in root.iter() if node.tag.endswith("page"))
    word = next(node for node in root.iter() if node.tag.endswith("word") and node.text == "SCALAR")
    expected = np.array([(float(word.attrib["xMin"])+float(word.attrib["xMax"]))/2,
                         (float(word.attrib["yMin"])+float(word.attrib["yMax"]))/2])
    expected *= np.array([rgb.shape[1]/float(page.attrib["width"]),
                          rgb.shape[0]/float(page.attrib["height"])])
    mask = rgb.max(axis=2) < 60
    for rect in [main,bar]:
        left,top,right,bottom = np.rint(rect).astype(int)
        mask[max(0,top-1):bottom+2,max(0,left-1):right+2] = False
    x0,x1 = round(expected[0])-45,round(expected[0])+46
    mask[:,:max(0,x0)] = False
    mask[:,min(mask.shape[1],x1):] = False
    rows = np.flatnonzero(mask.sum(axis=1) >= 3)
    assert len(rows) >= 4, "Missing horizontal colorbar label ink"
    yy,xx = np.where(mask[rows])
    assert xx.max()-xx.min() >= 18, "Horizontal label requires a word, not a tick"
    center = np.array([.5*(xx.min()+xx.max()),.5*(rows.min()+rows.max())])
    assert abs(center[0]-expected[0]) <= 4 and abs(center[1]-expected[1]) <= 4, \
        ("PNG label detached from PDF label position",center.tolist(),expected.tolist())
    assert center[1] > bar[3], "Native horizontal label must follow lower bar edge"
    return {"png_center":center.tolist(),"pdf_center":expected.tolist()}


def verify(artifacts, output):
    report = {"matplotlib":matplotlib.__version__,"cases":{},"failures":[]}
    names = [(f"{style}_{location}",style,location,1.)
             for location in ["bottom","top","left","right"]
             for style in ["line","filled","mesh"]]
    names += [(f"label_{style}_{location}",style,location,.63)
              for location in ["bottom","top"] for style in ["line","filled"]]
    for name,style,location,shrink in names:
        try:
            rgb = np.asarray(Image.open(artifacts/f"{name}.png").convert("RGB"))
            canvas = (rgb.shape[1],rgb.shape[0])
            pdf = artifacts/f"{name}.pdf"
            pm,pb = pdf_rectangles(pdf,canvas)
            main = image_rectangle(rgb,pm[2:]-pm[:2])
            bar = image_rectangle(rgb,pb[2:]-pb[:2])
            rm,rb = reference(canvas,style,location,shrink)
            assert side(main,bar) == side(rm,rb), ("Wrong PNG colorbar side",side(main,bar),location)
            assert side(pm,pb) == location, "Wrong PDF colorbar side"
            assert np.max(abs(main-pm)) <= 2 and np.max(abs(bar-pb)) <= 2, \
                ("PNG/PDF rectangle mismatch",main.tolist(),pm.tolist(),bar.tolist(),pb.tolist())
            result = {"png_axes":main.tolist(),"png_bar":bar.tolist(),
                      "pdf_axes":pm.tolist(),"pdf_bar":pb.tolist(),
                      "matplotlib_axes":rm.tolist(),"matplotlib_bar":rb.tolist()}
            if name.startswith("label_"):
                result["label"] = check_label(pdf,rgb,main,bar)
            report["cases"][name] = result
        except (AssertionError,StopIteration) as error:
            report["failures"].append({"name":name,"diagnostic":str(error)})
        output.parent.mkdir(parents=True,exist_ok=True)
        output.write_text(json.dumps(report,indent=2)+"\n")
    assert not report["failures"], report["failures"]
    print(f"PASS: {len(report['cases'])} actual PNG/PDF colorbar location cases")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifacts",type=Path,default=Path("build/test/output/fortplot_test_colorbar_location_contract"))
    parser.add_argument("--output",type=Path,default=Path("output/visual-audit/colorbar-locations/report.json"))
    arguments = parser.parse_args()
    verify(arguments.artifacts,arguments.output)
