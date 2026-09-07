#!/usr/bin/env python3
"""Check each supported marker against real Matplotlib PNG and PDF output.

PDF is rasterized at 300 dpi to avoid Poppler snapping a one-point stroke to
one versus two whole pixels at 100 dpi. Measurements retain physical sizes
in 100-dpi pixel units; neither image is aligned or resized.
"""

import argparse
import json
from pathlib import Path
import subprocess

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image

STYLES = ".osDdx+*^v<>pho"
WIDTH, HEIGHT = 780, 300


def reference(output):
    with matplotlib.rc_context(matplotlib.rcParamsDefault):
        fig, ax = plt.subplots(figsize=(7.8, 3), dpi=100)
        ax.set(xlim=(0, 16), ylim=(0, 3))
        for row in (1, 2):
            for index, style in enumerate(STYLES, 1):
                area = 36*row*row if index < len(STYLES) else 0
                ax.scatter([index], [row], s=area, marker=style,
                           color="#1f77b4", linewidths=1)
        for suffix in ("png", "pdf"):
            fig.savefig(output / f"matplotlib_markers.{suffix}")
        plt.close(fig)


def raster(path, output, dpi=300):
    target = output / (path.stem + "_pdf.png")
    subprocess.run(["pdftoppm", "-r", str(dpi), "-singlefile", "-png",
                    str(path), str(target.with_suffix(""))], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    return target


def measure(path, scale=1):
    image = np.asarray(Image.open(path).convert("RGB")).astype(float)
    # Blue-minus-red isolates marker ink, independent of black axis labels.
    ink = np.clip((image[:, :, 2] - image[:, :, 0])/149, 0, 1)
    markers = []
    for row in (1, 2):
        cy = round(scale*HEIGHT*(0.89 - 0.77*row/3))
        for index, style in enumerate(STYLES, 1):
            cx = round(scale*WIDTH*(0.125 + 0.775*index/16))
            crop = ink[cy-17*scale:cy+18*scale, cx-17*scale:cx+18*scale]
            yy, xx = np.nonzero(crop > 0.5)
            bbox = [float(xx.min())/scale, float(yy.min())/scale,
                    float(xx.max())/scale, float(yy.max())/scale] if len(xx) else None
            markers.append({"style": style, "row": row, "area": float(crop.sum())/scale**2,
                            "bbox": bbox})
    return image.shape, markers


def compare(actual_path, reference_path, scale=1):
    actual_shape, actual = measure(actual_path, scale)
    reference_shape, expected = measure(reference_path, scale)
    results = []
    for observed, oracle in zip(actual, expected):
        if oracle["bbox"] is None:
            passed = observed["area"] == 0
        elif observed["bbox"] is None:
            passed = False
        else:
            bounds_error = max(abs(a-b) for a, b in zip(observed["bbox"], oracle["bbox"]))
            # At six-point sizes a single antialiased pixel can change ink area
            # appreciably; bounds still constrain every individual marker.
            passed = bounds_error <= 2 and abs(observed["area"]-oracle["area"]) <= max(6, 0.20*oracle["area"])
        results.append({"actual": observed, "matplotlib": oracle, "pass": passed})
    return {"actual_shape": actual_shape, "reference_shape": reference_shape,
            "markers": results, "pass": actual_shape == reference_shape and
            all(item["pass"] for item in results)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifacts", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    reference(args.output)
    report = {"matplotlib": matplotlib.__version__, "results": {}}
    for suffix in ("png", "pdf"):
        actual = args.artifacts / f"marker_profile.{suffix}"
        expected = args.output / f"matplotlib_markers.{suffix}"
        if suffix == "pdf":
            actual, expected = raster(actual, args.output), raster(expected, args.output)
        report["results"][suffix] = compare(actual, expected, 3 if suffix == "pdf" else 1)
    (args.output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    passed = all(item["pass"] for item in report["results"].values())
    print("PASS" if passed else "FAIL", "Matplotlib marker geometry (PNG and PDF)")
    raise SystemExit(0 if passed else 1)


if __name__ == "__main__":
    main()
