#!/usr/bin/env python3
"""Actual Vega-Lite oracle for native JSON stroke width and line opacity.

Run after fo test test_spec_line_style. No image alignment is performed. The
gate checks endpoints, vertical stroke center, integrated chromatic coverage,
transparent-line absence, and alpha ratios against actual Vega-Lite renders.
"""

import argparse
import io
import json
from pathlib import Path

import numpy as np
from PIL import Image
import vl_convert

from compare_matplotlib import diagnostic, load_rgb, rasterize_pdf


def coverage(image):
    difference = image[:, :, 0].astype(float)-image[:, :, 1]
    # Ignore the one-LSB channel rounding found on nominally gray raster text.
    return np.where(difference > 2, difference/255, 0.0)


def stroke_metrics(image):
    ink = coverage(image)
    profile = ink[:, 350:451].mean(axis=1)
    area = float(profile.sum())
    if area <= 1e-12:
        return {"coverage": 0.0, "total_ink": float(ink.sum()), "endpoints": None,
                "center_y": None}
    columns = ink.sum(axis=0)
    columns_present = np.flatnonzero(columns > columns.max()*0.01)
    return {"coverage": area, "total_ink": float(ink.sum()),
            "endpoints": [int(columns_present[0]), int(columns_present[-1])],
            "center_y": float(np.dot(np.arange(len(profile)), profile)/area)}


def compare_strokes(reference, actual):
    ref, got = stroke_metrics(reference), stroke_metrics(actual)
    result = {"reference": ref, "actual": got}
    if reference.shape != actual.shape:
        return {**result, "pass": False, "error": "canvas dimensions differ"}
    if ref["coverage"] == 0:
        return {**result, "pass": got["total_ink"] <= 1e-6}
    if got["coverage"] == 0:
        return {**result, "pass": False, "error": "missing visible line"}
    ratio = got["coverage"]/ref["coverage"]
    endpoint_error = max(abs(a-b) for a, b in zip(ref["endpoints"], got["endpoints"]))
    center_error = abs(ref["center_y"]-got["center_y"])
    result.update({"coverage_ratio": ratio, "endpoint_error_px": endpoint_error,
                   "center_error_px": center_error,
                   "pass": 0.90 <= ratio <= 1.10 and endpoint_error <= 2 and center_error <= 1.5})
    return result


def check_alpha_ratios(results):
    checks = {}
    for source in (1, 2):
        for width in (1, 2):
            prefix = f"source{source}_w{width}"
            for suffix in ("png", "pdf"):
                faint = results[f"{prefix}_a2.{suffix}"]
                opaque = results[f"{prefix}_a3.{suffix}"]
                denom = opaque["actual"]["coverage"]
                ratio = faint["actual"]["coverage"]/denom if denom else 0
                ref_ratio = faint["reference"]["coverage"]/opaque["reference"]["coverage"]
                checks[f"{prefix}.{suffix}"] = {
                    "reference": ref_ratio, "actual": ratio,
                    "pass": abs(ratio-ref_ratio) <= 0.015,
                }
    return checks


def checked_downsample(image, reference_shape, renderer):
    size = (reference_shape[1], reference_shape[0])
    expected = tuple(4*dimension for dimension in size)
    if image.size != expected:
        raise ValueError(f"{renderer} supersampled canvas is {image.size}; expected {expected}")
    return np.asarray(image.convert("RGB").resize(size, Image.Resampling.BOX))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fixtures", type=Path, default=Path("build/test/output/spec_line_style"))
    parser.add_argument("--output", type=Path, default=Path("output/visual-audit/spec-line-style"))
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    results = {}
    for path in sorted(args.fixtures.glob("*.json")):
        spec = json.loads(path.read_text())
        reference = np.asarray(Image.open(io.BytesIO(vl_convert.vegalite_to_png(spec))).convert("RGB"))
        Image.fromarray(reference).save(args.output / f"vega_{path.stem}.png")
        for suffix in ("png", "pdf"):
            actual_path = args.fixtures / f"{path.stem}.{suffix}"
            expected = reference
            if suffix == "pdf":
                actual_path = args.output / f"native_{path.stem}_pdf.png"
                # Supersample both renderers: 100dpi PDF rasterization can round
                # a 2.083px vector stroke to three opaque rows depending on phase.
                rasterize_pdf(args.fixtures / f"{path.stem}.pdf", actual_path, 400)
                with Image.open(actual_path) as raster:
                    reduced = checked_downsample(raster, reference.shape, "native PDF")
                Image.fromarray(reduced).save(actual_path)
                high = vl_convert.vegalite_to_png(spec, scale=4)
                with Image.open(io.BytesIO(high)) as raster:
                    expected = checked_downsample(raster, reference.shape, "Vega")
            actual = load_rgb(actual_path)
            key = f"{path.stem}.{suffix}"
            result = compare_strokes(expected, actual)
            results[key] = result
            print(f"{'PASS' if result['pass'] else 'FAIL'} {key}: {result}")
            diagnostic(expected, actual, args.output / f"{key}.png")
    if not results:
        raise SystemExit(f"No fixtures in {args.fixtures}; run fo test test_spec_line_style")
    alpha = check_alpha_ratios(results)
    report = {"vl_convert": vl_convert.__version__, "results": results, "alpha_checks": alpha,
              "pass": all(item["pass"] for item in (*results.values(), *alpha.values()))}
    (args.output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    raise SystemExit(0 if report["pass"] else 1)


if __name__ == "__main__":
    main()
