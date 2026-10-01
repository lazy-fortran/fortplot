#!/usr/bin/env python3
"""Independent rendered contour geometry and Matplotlib line-colorbar oracle."""
from __future__ import annotations

import argparse
import json
from pathlib import Path
import subprocess
import xml.etree.ElementTree as ET

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image

LEVELS = np.array([1., 3., 5., 8., 11.])


def longest_run(mask):
    padded = np.r_[False, mask, False].astype(int)
    starts = np.flatnonzero(np.diff(padded) == 1)
    ends = np.flatnonzero(np.diff(padded) == -1) - 1
    if not len(starts):
        return 0, 0, 0
    index = np.argmax(ends - starts)
    return int(ends[index] - starts[index] + 1), int(starts[index]), int(ends[index])


def axes_frame(rgb):
    dark = rgb.max(axis=2) < 60
    resolution = rgb.shape[1]/640
    candidates = []
    for x in range(rgb.shape[1]):
        length, top, bottom = longest_run(dark[:, x])
        if length < 150*resolution:
            continue
        width, left, right = longest_run(dark[top])
        if width > 150*resolution and left <= x <= left + 8*resolution:
            # Axis ticks can extend a frame's black run beyond its corners.
            frame_rows = []
            for row in range(top, bottom+1):
                row_width, row_left, row_right = longest_run(dark[row])
                if row_width >= width-10*resolution and abs(row_right-right) <= 2*resolution:
                    frame_rows.append(row)
            if frame_rows:
                bottom = max(frame_rows)
                candidates.append(((bottom-top)*width, x, top, right, bottom))
    assert candidates, "No independently detectable axes frame"
    return max(candidates)[1:]


def reference(levels=LEVELS, custom_ticks=None, linewidth=1.5, offset=0., scale=1.):
    x, y = np.linspace(0, 2, 31), np.linspace(0, 1, 19)
    fig, ax = plt.subplots()
    cs = ax.contour(x, y, offset + scale*(x[None, :] + 10*y[:, None]), levels=levels,
                    cmap="viridis", linewidths=linewidth)
    cb = fig.colorbar(cs, ticks=custom_ticks)
    fig.canvas.draw()
    ticks = cb.get_ticks()
    positions = cb.ax.transData.transform(np.c_[np.zeros(len(ticks)), ticks])[:, 1]
    bounds = cb.ax.get_window_extent()
    result = {"levels": cs.levels, "colors": cs.to_rgba(cs.cvalues)[:, :3] * 255,
              "ticks": ticks, "fractions": (positions-bounds.y0)/bounds.height,
              "linewidth": float(cs.get_linewidths()[0]), "offset": offset, "scale": scale}
    plt.close(fig)
    return result


def colored_mask(rgb):
    return np.ptp(rgb.astype(float), axis=2) > 35


def check_geometry(rgb, ref):
    left, top, right, bottom = axes_frame(rgb)
    resolution = rgb.shape[1]/640
    padding = max(1, round(resolution))
    crop = rgb[top+padding:bottom-padding+1, left+padding:right-padding+1]
    yy, xx = np.indices(crop.shape[:2])
    field = 2*(xx+padding)/(right-left) + 10*(bottom-yy-top-padding)/(bottom-top)
    distances = np.linalg.norm(crop[..., None, :]-ref["colors"], axis=3)
    nearest = distances.argmin(axis=2)
    colored = colored_mask(crop) & (distances.min(axis=2) < 5)
    tolerance = 2.5*resolution*(2/(right-left)+10/(bottom-top))
    offset, scale = ref.get("offset", 0.), ref.get("scale", 1.)
    errors = []
    for i, level in enumerate(ref["levels"]):
        # Endpoint-only and out-of-domain contours have no finite visible span.
        physical_level = (level-offset)/scale
        if not 0 < physical_level < 12:
            continue
        selected = colored & (nearest == i)
        lo = max(2*padding/(right-left), physical_level-10*(1-padding/(bottom-top)))
        hi = min(2-2*padding/(right-left), physical_level-10*padding/(bottom-top))
        expected_span = max(0., hi-lo)*(right-left)/2
        assert selected.sum() >= max(2, .2*expected_span), f"Missing visible contour level {level}"
        error = float(np.quantile(np.abs(field[selected]-physical_level), .99))
        assert error <= tolerance, f"Contour {level}: physical error {error} > {tolerance}"
        observed = 2*(xx[selected]+padding)/(right-left)
        assert observed.min() <= lo+tolerance and observed.max() >= hi-tolerance, \
            f"Contour {level} does not cover its analytic visible domain"
        errors.append(error)
    return {"axes": [left, top, right, bottom], "max_physical_error": max(errors),
            "tolerance": tolerance}


def bar_frame(rgb, horizontal=False):
    left, top, right, bottom = axes_frame(rgb)
    dark = rgb.max(axis=2) < 60
    resolution = rgb.shape[1]/640
    if not horizontal:
        columns = np.flatnonzero(dark[top:bottom+1].sum(axis=0) > .8*(bottom-top))
        columns = columns[columns > right+8*resolution]
        assert len(columns) >= 2, "Missing line colorbar frame"
        return int(columns.min()), top, int(columns.max()), bottom
    rows = np.flatnonzero(dark[:, left:right+1].sum(axis=1) > .8*(right-left))
    rows = rows[(rows < top-8*resolution) | (rows > bottom+8*resolution)]
    assert len(rows) >= 2, "Missing horizontal colorbar frame"
    return left, int(rows.min()), right, int(rows.max())


def check_bar(rgb, ref, horizontal=False):
    left, top, right, bottom = bar_frame(rgb, horizontal)
    resolution = rgb.shape[1]/640
    padding = round(2*resolution)
    if horizontal:
        samples = rgb[(top+bottom)//2, left-padding:right+padding+1]
    else:
        samples = rgb[top-padding:bottom+padding+1, (left+right)//2][::-1]
    mask = colored_mask(samples[None, ...])[0]
    groups = []
    padded = np.r_[False, mask, False].astype(int)
    starts = np.flatnonzero(np.diff(padded) == 1)
    ends = np.flatnonzero(np.diff(padded) == -1) - 1
    merged = []
    for start, end in zip(starts, ends):
        if merged and start-merged[-1][1] <= 3*resolution:
            merged[-1][1] = end
        else:
            merged.append([start, end])
    for start, end in merged:
        visible = samples[start:end+1][mask[start:end+1]]
        groups.append((start, end, np.median(visible, axis=0)))
    assert len(groups) == len(ref["levels"]), "Wrong stroke count or continuous bar"
    length = len(samples)-2*padding-1
    positions = []
    for i, (start, end, color) in enumerate(groups):
        expected = i/max(1, len(groups)-1) if len(groups) > 1 else .5
        position = (.5*(start+end)-padding)/length
        assert abs(position-expected)*length <= 2*resolution, "Wrong ordinal level spacing"
        # Stroke/border antialiasing mixes the level color with white and black.
        mixing = np.c_[ref["colors"][i], np.full(3, 255.)]
        coefficients = np.linalg.lstsq(mixing, color, rcond=None)[0]
        error = np.linalg.norm(mixing @ coefficients-color)
        assert error < 8 and coefficients[0] > .15 and coefficients[1] >= -.02 \
            and coefficients.sum() <= 1.03, "Wrong level-range color"
        expected_width = ref["linewidth"]*100/72*resolution
        assert end-start+1 <= expected_width+2*resolution, "Wrong contour stroke width"
        if 0 < i < len(groups)-1:
            assert end-start+1 >= max(1, expected_width-2*resolution), "Contour stroke too thin"
        positions.append(position)
    assert mask.mean() < .35, "Line colorbar must preserve white gaps"
    return {"frame": [left, top, right, bottom], "stroke_fractions": positions,
            "colored_fraction": float(mask.mean())}


def check_pdf_ticks(pdf, rgb, ref, labels=None, horizontal=False):
    xml = subprocess.check_output(["pdftotext", "-bbox", str(pdf), "-"])
    root = ET.fromstring(xml)
    page = next(item for item in root.iter() if item.tag.endswith("page"))
    sx, sy = rgb.shape[1]/float(page.attrib["width"]), rgb.shape[0]/float(page.attrib["height"])
    words = []
    for word in page:
        if not word.tag.endswith("word"):
            continue
        words.append((word.text, float(word.attrib["xMin"])*sx,
                      .5*(float(word.attrib["xMin"])+float(word.attrib["xMax"]))*sx,
                      .5*(float(word.attrib["yMin"])+float(word.attrib["yMax"]))*sy))
    left, top, right, bottom = bar_frame(rgb, horizontal)
    resolution = rgb.shape[1]/640
    numeric_labels = labels is None
    observed_labels = []
    labels = labels or [f"{tick:g}" for tick in ref["ticks"]]
    for label, fraction, tick in zip(labels, ref["fractions"], ref["ticks"]):
        def matches(text):
            if not numeric_labels:
                return text == label
            try:
                differences = np.diff(np.unique(ref["ticks"]))
                tolerance = abs(np.spacing(tick))*8
                if len(differences):
                    tolerance = max(tolerance, float(differences.min())*.01)
                else:
                    tolerance = max(tolerance, abs(tick)*1.e-10)
                return abs(float(text.replace("−", "-"))-tick) <= tolerance
            except ValueError:
                return False
        if horizontal:
            expected = left+fraction*(right-left)
            matching = [word for word in words if matches(word[0]) and
                        top-18*resolution <= word[3] <= bottom+25*resolution and abs(word[2]-expected) <= 14*resolution]
        else:
            expected = bottom-fraction*(bottom-top)
            matching = [word for word in words if matches(word[0]) and
                        word[1] >= right-2*resolution and abs(word[3]-expected) <= 14*resolution]
        assert matching, f"Missing or misplaced level tick label {label}"
        observed_labels.append(matching[0][0])
    return observed_labels


def verify(artifacts, output):
    output.mkdir(parents=True, exist_ok=True)
    explicit = reference()
    cases = {name: (explicit, None, False) for name in
             ["square_bar", "rect_yx_bar", "rect_xy_bar"]}
    cases["rect_default_bar"] = (reference(levels=None), None, False)
    cases["dense_bar"] = (reference(levels=np.arange(-1., 14.)), None, False)
    cases["wide_bar"] = (reference(linewidth=3.), None, False)
    cases["custom_bar"] = (reference(custom_ticks=[1, 5, 11]), ["LOW", "MID", "HIGH"], False)
    cases["interpolated_bar"] = (reference(custom_ticks=[2, 6.5, 9.5]), ["A", "B", "C"], False)
    cases["bottom_bar"] = (explicit, None, True)
    cases["unsorted_bar"] = (explicit, None, False)
    cases["fallback_bar"] = (reference(levels=None), None, False)
    cases["offset_bar"] = (reference(levels=None, offset=100.13, scale=.527), None, False)
    cases["tiny_bar"] = (reference(levels=1.e-7*LEVELS, scale=1.e-7), None, False)
    cases["large_bar"] = (reference(levels=1.e12+np.arange(3), offset=1.e12), None, False)
    cases["fraction_offset_bar"] = (reference(levels=[100.8, 101.8, 102.8], offset=100., scale=.5), None, False)
    report = {}
    for name, (ref, labels, horizontal) in cases.items():
        prefix = output / f"{name}_pdf"
        subprocess.run(["pdftoppm", "-png", "-r", "300", "-singlefile",
                        str(artifacts/f"{name}.pdf"), str(prefix)], check=True)
        pdf_rgb = np.asarray(Image.open(prefix.with_suffix(".png")).convert("RGB"), dtype=float)
        for kind in ["png", "pdf"]:
            path = artifacts / f"{name}.png"
            if kind == "pdf":
                path = prefix.with_suffix(".png")
            rgb = np.asarray(Image.open(path).convert("RGB"), dtype=float)
            try:
                result = check_geometry(rgb, ref)
                result.update(check_bar(rgb, ref, horizontal))
                result["tick_labels"] = check_pdf_ticks(artifacts/f"{name}.pdf", pdf_rgb, ref, labels, horizontal)
            except AssertionError as error:
                raise AssertionError(f"{name}_{kind}: {error}") from error
            report[f"{name}_{kind}"] = result
            (output/"report.json").write_text(json.dumps(report, indent=2)+"\n")
    # Matplotlib rejects singleton contour colorbars. Fortplot keeps a finite,
    # truthful native fallback: one midpoint-colored stroke and its actual value.
    single = {"levels": np.array([5.]), "colors": np.array([plt.colormaps["viridis"](.5)[:3]])*255,
              "ticks": np.array([5.]), "fractions": np.array([.5]), "linewidth": 1.5}
    for name in ["single_bar", "constant_bar", "single_fraction_bar"]:
        single["levels"] = np.array([5.25 if name == "single_fraction_bar" else 5.])
        single["ticks"] = single["levels"]
        prefix = output/f"{name}_pdf"
        subprocess.run(["pdftoppm", "-png", "-r", "300", "-singlefile",
                        str(artifacts/f"{name}.pdf"), str(prefix)], check=True)
        pdf_rgb = np.asarray(Image.open(prefix.with_suffix(".png")).convert("RGB"), dtype=float)
        for kind, path in [("png", artifacts/f"{name}.png"), ("pdf", prefix.with_suffix(".png"))]:
            rgb = np.asarray(Image.open(path).convert("RGB"), dtype=float)
            result = check_bar(rgb, single)
            if name != "constant_bar":
                result.update(check_geometry(rgb, single))
            result["tick_labels"] = check_pdf_ticks(artifacts/f"{name}.pdf", pdf_rgb, single,
                                                    ["REAL"] if name == "single_bar" else None)
            report[f"{name}_{kind}"] = result
        text = subprocess.check_output(["pdftotext", str(artifacts/f"{name}.pdf"), "-"], text=True)
        assert "FALSE" not in text, "Artificial singleton normalization accepted a false tick"
    for name in ["filled", "mesh"]:
        bar = np.asarray(Image.open(artifacts/f"{name}_bar.png").convert("RGB"), dtype=float)
        no_bar = np.asarray(Image.open(artifacts/f"{name}_no_bar.png").convert("RGB"), dtype=float)
        left, top, right, bottom = bar_frame(bar)
        assert colored_mask(bar[top+4:bottom-3, left+3:right-2]).mean() > .8, "Existing scalar bar lost its fill"
        assert not np.array_equal(bar, no_bar), "Existing explicit scalar colorbar missing"
    empty = np.asarray(Image.open(artifacts/"empty_bar.png").convert("RGB"), dtype=float)
    assert not colored_mask(empty).any(), "Empty contour levels emitted a false scalar bar"
    ordinary = Image.open(artifacts/"ordinary_bar.png")
    assert np.array_equal(ordinary, Image.open(artifacts/"ordinary_no_bar.png")), "Ordinary line acquired an unmapped bar"
    assert not np.array_equal(Image.open(artifacts/"square_bar.png"), Image.open(artifacts/"square_no_bar.png")), "Explicit linebar missing"
    assert np.array_equal(Image.open(artifacts/"rect_yx_bar.png"), Image.open(artifacts/"rect_xy_bar.png")), "Legacy rectangular shape changed physical rendering"
    (output/"report.json").write_text(json.dumps(report, indent=2)+"\n")
    print(f"PASS: {len(report)} rendered contour/colorbar cases and preservation controls")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifacts", type=Path, default=Path("build/test/output/fortplot_test_contour_line_contract"))
    parser.add_argument("--output", type=Path, default=Path("output/visual-audit/contour-line-colorbar"))
    args = parser.parse_args()
    verify(args.artifacts, args.output)
