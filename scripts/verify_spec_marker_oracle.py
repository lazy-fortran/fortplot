#!/usr/bin/env python3
"""Check native Vega and portable Matplotlib marker paths against real renderers.

Run after fo test test_spec_point_shapes. Dependencies: vl-convert-python,
Matplotlib, Pillow, NumPy, and pdftoppm. The original fixture JSON is rendered
unchanged by Vega-Lite; custom triangle/plus/cross paths also have actual
Matplotlib marker oracles. This primitive gate compares shape after translating
its bounding-box center by an integer pixel offset, and records that offset.
It does not certify mark placement: complete figure layout and labels belong
to compare_matplotlib.py, which never aligns images.
"""

import argparse
import io
import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image
import vl_convert

from compare_matplotlib import diagnostic, load_rgb, mask_metrics, rasterize_pdf


def red(image):
    return (image[:, :, 0] > 200) & (image[:, :, 1] < 80) & (image[:, :, 2] < 80)


def shape_metrics(reference, actual):
    ref_mask, actual_mask = red(reference), red(actual)
    if not ref_mask.any() or not actual_mask.any():
        return mask_metrics(ref_mask, actual_mask)
    ry, rx = np.nonzero(ref_mask)
    ay, ax = np.nonzero(actual_mask)
    dx = round(float(rx.min()+rx.max()-ax.min()-ax.max()) / 2)
    dy = round(float(ry.min()+ry.max()-ay.min()-ay.max()) / 2)
    actual_mask = np.roll(actual_mask, (dy, dx), axis=(0, 1))
    result = mask_metrics(ref_mask, actual_mask)
    # Integrated chroma measures filled/stroked area without a hard threshold's
    # dependence on whether a two-pixel stroke falls on a pixel or between two.
    ref_area = float(np.maximum(reference[:, :, 0].astype(float) - reference[:, :, 1], 0).sum()/255)
    actual_area = float(np.maximum(actual[:, :, 0].astype(float) - actual[:, :, 1], 0).sum()/255)
    coverage_ratio = actual_area/ref_area
    result["coverage_area_ratio"] = coverage_ratio
    result["position_offset_px"] = [-dx, -dy]
    size_error = max(abs(int(rx.max()-rx.min())-int(ax.max()-ax.min())),
                     abs(int(ry.max()-ry.min())-int(ay.max()-ay.min())))
    result["extent_error_px"] = size_error
    # One pixel at either edge permits two pixels of full-span raster variation.
    result["pass"] = (min(result["recall"], result["precision"]) >= 0.95 and
                      max(result["largest_missing_component"], result["largest_extra_component"]) <= 16 and
                      0.90 <= coverage_ratio <= 1.10 and size_error <= 2)
    return result


def matplotlib_reference(name):
    markers = {"mpl-triangle": "^", "mpl-plus": "+", "mpl-cross": "x"}
    with matplotlib.rc_context(matplotlib.rcParamsDefault):
        fig = plt.figure(figsize=(4, 3), dpi=100)
        ax = fig.add_axes([0, 0, 1, 1], xlim=(-1, 1), ylim=(-1, 1))
        ax.scatter([0], [0], s=400*(72/100)**2, marker=markers[name], color="red",
                   linewidths=0 if name == "mpl-triangle" else 2*72/100)
        fig.canvas.draw()
        result = np.asarray(fig.canvas.buffer_rgba())[:, :, :3].copy()
        plt.close(fig)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fixtures", type=Path, default=Path("build/test/output/spec_point_shapes"))
    parser.add_argument("--output", type=Path, default=Path("output/visual-audit/spec-marker-oracle"))
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    results = {}
    files = sorted(args.fixtures.glob("*.json"))
    if not files:
        raise SystemExit(f"No fixtures in {args.fixtures}; run fo test test_spec_point_shapes")
    for path in files:
        name = path.stem
        spec = json.loads(path.read_text())
        buffer = vl_convert.vegalite_to_png(spec)
        reference = np.asarray(Image.open(io.BytesIO(buffer)).convert("RGB"))
        Image.fromarray(reference).save(args.output / f"vega_{name}.png")
        for suffix in ("png", "pdf"):
            actual_path = args.fixtures / f"{name}.{suffix}"
            if suffix == "pdf":
                actual_path = args.output / f"native_{name}_pdf.png"
                rasterize_pdf(args.fixtures / f"{name}.pdf", actual_path, 100)
            actual = load_rgb(actual_path)
            oracles = {"vega": reference}
            if name.startswith("mpl-"):
                oracles["matplotlib"] = matplotlib_reference(name)
            for oracle, expected in oracles.items():
                key = f"{name}.{suffix}.{oracle}"
                if actual.shape != expected.shape:
                    result = {"pass": False, "error": "canvas dimensions differ"}
                else:
                    result = shape_metrics(expected, actual)
                results[key] = result
                print(f"{'PASS' if result['pass'] else 'FAIL'} {key}: {result}")
                diagnostic(expected, actual, args.output / f"{key}.png")
    report = {"matplotlib": matplotlib.__version__, "vl_convert": vl_convert.__version__,
              "results": results, "pass": all(result["pass"] for result in results.values())}
    (args.output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    raise SystemExit(0 if report["pass"] else 1)


if __name__ == "__main__":
    main()
