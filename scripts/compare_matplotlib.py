#!/usr/bin/env python3
"""Compare fortplot PNG and PDF renders with actual Matplotlib defaults.

The gate permits one pixel of antialiasing displacement for geometry, and three
pixels of text-bound displacement for font metrics. It never aligns, rescales,
recolors, masks data regions, or learns thresholds from fortplot output. Each
label must be present; math fraction/radical strokes receive additional checks.
Reports and a compact side-by-side/difference image are saved for every case.
"""

import argparse
import json
from pathlib import Path
import subprocess

import numpy as np
from PIL import Image, ImageFilter


GEOMETRY_RADIUS = 1
GEOMETRY_COVERAGE = 0.95
GEOMETRY_AREA_RATIO = (0.80, 1.25)
MAX_UNMATCHED_COMPONENT = 16
TEXT_BOUND_TOLERANCE = 3
TEXT_AREA_RATIO = (0.55, 1.80)
TEXT_DIMENSION_RATIO = (0.80, 1.25)


def dilate(mask, radius):
    return np.asarray(Image.fromarray(mask).filter(ImageFilter.MaxFilter(2*radius+1)))


def largest_component(mask):
    """Eight-connected error area prevents a missing small mark being averaged away."""
    remaining = set(np.flatnonzero(mask).tolist())
    width = mask.shape[1]
    largest = 0
    while remaining:
        pending = [remaining.pop()]
        count = 0
        while pending:
            current = pending.pop()
            count += 1
            y, x = divmod(current, width)
            for dy in (-1, 0, 1):
                for dx in (-1, 0, 1):
                    if not 0 <= x+dx < width or not 0 <= y+dy < mask.shape[0]:
                        continue
                    neighbor = current + dy*width + dx
                    if neighbor in remaining:
                        remaining.remove(neighbor)
                        pending.append(neighbor)
        largest = max(largest, count)
    return largest


def mask_metrics(reference, actual, radius=GEOMETRY_RADIUS):
    ref_count, actual_count = int(reference.sum()), int(actual.sum())
    missing = reference & ~dilate(actual, radius)
    extra = actual & ~dilate(reference, radius)
    recall = 1.0 - float(missing.sum() / max(ref_count, 1))
    precision = 1.0 - float(extra.sum() / max(actual_count, 1))
    largest_missing, largest_extra = largest_component(missing), largest_component(extra)
    if ref_count == actual_count == 0:
        recall = precision = 1.0
    ratio = actual_count / max(ref_count, 1)
    passed = (min(recall, precision) >= GEOMETRY_COVERAGE and
              GEOMETRY_AREA_RATIO[0] <= ratio <= GEOMETRY_AREA_RATIO[1] and
              max(largest_missing, largest_extra) <= MAX_UNMATCHED_COMPONENT)
    if ref_count == actual_count == 0:
        passed = True
    return {"reference_pixels": ref_count, "actual_pixels": actual_count,
            "recall": recall, "precision": precision, "area_ratio": ratio,
            "largest_missing_component": largest_missing, "largest_extra_component": largest_extra,
            "pass": bool(passed)}


def ink(image):
    values = image.astype(np.int16)
    return (values.max(2) < 210) & (np.ptp(values, axis=2) < 25)


def colored(image):
    return (np.ptp(image.astype(np.int16), axis=2) > 35) & (image.min(2) < 210)


def bounds(mask):
    yy, xx = np.nonzero(mask)
    if not len(xx):
        return None
    return [int(xx.min()), int(yy.min()), int(xx.max()+1), int(yy.max()+1)]


def region(box, shape, padding=TEXT_BOUND_TOLERANCE + 1):
    x0, y0, x1, y1 = box
    height, width = shape[:2]
    pad_x = max(padding, int((x1-x0) * 0.125))
    pad_y = max(padding, int((y1-y0) * 0.125))
    return (slice(max(0, int(np.floor(y0))-pad_y), min(height, int(np.ceil(y1))+pad_y)),
            slice(max(0, int(np.floor(x0))-pad_x), min(width, int(np.ceil(x1))+pad_x)))


def horizontal_strokes(mask, minimum=8):
    strokes = np.zeros_like(mask)
    for y, row in enumerate(mask):
        edges = np.diff(np.r_[False, row, False].astype(np.int8))
        for start, end in zip(np.flatnonzero(edges == 1), np.flatnonzero(edges == -1)):
            if end-start >= minimum:
                strokes[y, start:end] = True
    return strokes


def text_metrics(reference, actual, label):
    crop = region(label["bbox"], reference.shape)
    ref_mask, actual_mask = ink(reference[crop]), ink(actual[crop])
    ref_box, actual_box = bounds(ref_mask), bounds(actual_mask)
    result = {"text": label["text"], "math": label["math"],
              "reference_bbox": ref_box, "actual_bbox": actual_box}
    if ref_box is None:
        result.update({"pass": False, "error": "reference label has no visible ink"})
        return result
    if actual_box is None:
        result.update({"pass": False, "error": "missing label"})
        return result
    displacement = max(abs(a-b) for a, b in zip(ref_box, actual_box))
    center_error = max(abs((ref_box[i]+ref_box[i+2]) - (actual_box[i]+actual_box[i+2])) / 2
                       for i in (0, 1))
    dimensions = [(actual_box[i+2]-actual_box[i])/(ref_box[i+2]-ref_box[i]) for i in (0, 1)]
    ratio = float(actual_mask.sum() / ref_mask.sum())
    passed = (center_error <= TEXT_BOUND_TOLERANCE and TEXT_AREA_RATIO[0] <= ratio <= TEXT_AREA_RATIO[1]
              and all(TEXT_DIMENSION_RATIO[0] <= value <= TEXT_DIMENSION_RATIO[1] for value in dimensions))
    result.update({"max_bound_error_px": displacement, "center_error_px": center_error,
                   "dimension_ratios": dimensions, "ink_ratio": ratio})
    if label["math"]:
        strokes = horizontal_strokes(ref_mask)
        if strokes.any():
            stroke_recall = float((strokes & dilate(horizontal_strokes(actual_mask), 2)).sum() / strokes.sum())
            result["math_stroke_recall"] = stroke_recall
            passed = passed and stroke_recall >= 0.90
    result["pass"] = bool(passed)
    return result


def compare_images(reference, actual, metadata, colors):
    if reference.shape != actual.shape:
        return {"pass": False, "error": "image dimensions differ",
                "reference_shape": list(reference.shape), "actual_shape": list(actual.shape)}
    checks = {"colored_geometry": mask_metrics(colored(reference), colored(actual))}
    # Individual series cannot disappear or swap colors behind a global average.
    for color in colors:
        rgb = np.array([int(color[i:i+2], 16) for i in (1, 3, 5)])
        if np.ptp(rgb) <= 35:
            continue  # Gray is checked below, after font-dependent text is separated.
        ref_mask = np.linalg.norm(reference.astype(float)-rgb, axis=2) < 35
        actual_mask = np.linalg.norm(actual.astype(float)-rgb, axis=2) < 35
        if ref_mask.sum() >= 12 or actual_mask.sum() >= 12:
            checks[color] = mask_metrics(ref_mask, actual_mask)
    ref_dark, actual_dark = ink(reference), ink(actual)
    labels = []
    for label in metadata["labels"]:
        labels.append(text_metrics(reference, actual, label))
        crop = region(label["bbox"], reference.shape)
        ref_dark[crop] = False
        actual_dark[crop] = False
    # Remaining black/gray marks include axes, ticks, grids, and unlabelled text.
    checks["axes_and_monochrome_geometry"] = mask_metrics(ref_dark, actual_dark)
    return {"pass": all(check["pass"] for check in checks.values()) and
                    all(label["pass"] for label in labels),
            "geometry": checks, "labels": labels}


def rasterize_pdf(source, target, dpi):
    subprocess.run(["pdftoppm", "-f", "1", "-singlefile", "-r", str(dpi),
                    "-png", str(source), str(target.with_suffix(""))],
                   check=True, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)


def load_rgb(path):
    with Image.open(path) as image:
        return np.asarray(image.convert("RGB"))


def diagnostic(reference, actual, path):
    ref_image, actual_image = Image.fromarray(reference), Image.fromarray(actual)
    width, height = ref_image.size
    canvas = Image.new("RGB", (width*3, height), "white")
    canvas.paste(ref_image, (0, 0))
    canvas.paste(actual_image, (width, 0))
    if actual.shape == reference.shape:
        delta = np.abs(actual.astype(np.int16)-reference.astype(np.int16))
        difference = 255-np.clip(delta*4, 0, 255).astype(np.uint8)
        canvas.paste(Image.fromarray(difference), (width*2, 0))
    canvas.save(path)


def compare_suite(reference_dir, actual_dir, output_dir, formats=("png", "pdf"), cases=None):
    reference_dir, actual_dir, output_dir = map(Path, (reference_dir, actual_dir, output_dir))
    output_dir.mkdir(parents=True, exist_ok=True)
    manifest = json.loads((reference_dir / "manifest.json").read_text())
    selected = cases or list(manifest["cases"])
    report = {"matplotlib": manifest["matplotlib"], "thresholds": {
        "geometry_radius_px": GEOMETRY_RADIUS, "geometry_coverage": GEOMETRY_COVERAGE,
        "max_unmatched_component_pixels": MAX_UNMATCHED_COMPONENT,
        "geometry_area_ratio": GEOMETRY_AREA_RATIO, "text_bound_tolerance_px": TEXT_BOUND_TOLERANCE,
        "text_ink_ratio": TEXT_AREA_RATIO, "text_dimension_ratio": TEXT_DIMENSION_RATIO,
        "math_stroke_coverage": 0.90}, "results": {}}
    for name in selected:
        metadata = manifest["cases"][name]
        for suffix in formats:
            key = f"{name}.{suffix}"
            reference = reference_dir / f"mpl_{key}"
            actual = actual_dir / f"fp_{key}"
            if not reference.is_file() or not actual.is_file():
                report["results"][key] = {"pass": False, "error": "missing artifact"}
                continue
            if suffix == "pdf":
                ref_raster = output_dir / f"mpl_{name}_pdf.png"
                actual_raster = output_dir / f"fp_{name}_pdf.png"
                rasterize_pdf(reference, ref_raster, metadata["dpi"])
                rasterize_pdf(actual, actual_raster, metadata["dpi"])
                reference, actual = ref_raster, actual_raster
            ref_image, actual_image = load_rgb(reference), load_rgb(actual)
            result = compare_images(ref_image, actual_image, metadata, manifest["colors"])
            report["results"][key] = result
            diagnostic(ref_image, actual_image, output_dir / f"{name}_{suffix}_comparison.png")
            failures = [label["text"] for label in result.get("labels", []) if not label["pass"]]
            geometry = [kind for kind, check in result.get("geometry", {}).items() if not check["pass"]]
            error = result.get("error", "")
            print(f"{'PASS' if result['pass'] else 'FAIL'} {key}: geometry={geometry}, labels={failures} {error}")
    report["pass"] = bool(report["results"]) and all(result["pass"] for result in report["results"].values())
    (output_dir / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reference", type=Path, required=True)
    parser.add_argument("--actual", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--format", choices=("png", "pdf"), action="append")
    parser.add_argument("--case", action="append")
    args = parser.parse_args()
    report = compare_suite(args.reference, args.actual, args.output,
                           args.format or ("png", "pdf"), args.case)
    raise SystemExit(0 if report["pass"] else 1)


if __name__ == "__main__":
    main()
