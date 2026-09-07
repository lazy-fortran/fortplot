#!/usr/bin/env python3
"""Compare actual PNG/PDF legend placement and extents with default Matplotlib.

First run `fo test test_legend_matplotlib`. No Fortplot output influences the
reference artist options, automatic placement, or expected legend bounds.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path
import subprocess

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image

LOCATIONS = ["upper right", "upper left", "lower left", "lower right", "right",
             "center left", "center right", "lower center", "upper center", "center"]


def reference(name: str, output: Path, font_family: str | None = None) -> list[float]:
    with matplotlib.rc_context(matplotlib.rcParamsDefault):
        if font_family is not None:
            matplotlib.rcParams["font.family"] = [font_family]
        fig, ax = plt.subplots(figsize=(6.4, 4.8), dpi=100)
        if name == "best":
            x = np.arange(100) / 5
            ax.plot(x, np.sin(x), label="sin(x)")
            ax.plot(x, np.cos(x), label="cos(x)")
            leg = ax.legend()
        elif name == "crossing":
            ax.plot([0, 10], [9.5, 9.5], label="crossing")
            ax.set(xlim=(0, 10), ylim=(0, 10))
            leg = ax.legend()
        elif name == "bar":
            ax.bar([8], [10], width=4, label="bar")
            ax.set(xlim=(0, 10), ylim=(0, 10))
            leg = ax.legend()
        else:
            ax.plot([0, 1], [0, 1], label="series")
            ax.set(xlim=(0, 1), ylim=(0, 1))
            leg = ax.legend(loc=LOCATIONS[int(name[-2:]) - 1])
        fig.canvas.draw()
        bbox = leg.get_window_extent(fig.canvas.get_renderer())
        expected = [float(bbox.x0), 480.0 - float(bbox.y1),
                    float(bbox.width), float(bbox.height)]
        for extension in ("png", "pdf"):
            suffix = "_liberation" if font_family is not None else ""
            fig.savefig(output / f"matplotlib_{name}{suffix}.{extension}")
        plt.close(fig)
        return expected


def longest_run(row: np.ndarray) -> tuple[int, int]:
    switches = np.diff(np.r_[False, row, False].astype(np.int8))
    starts, stops = np.flatnonzero(switches == 1), np.flatnonzero(switches == -1)
    if not len(starts):
        return 0, 0
    runs = []
    left, right = int(starts[0]), int(stops[0])
    for start, stop in zip(starts[1:], stops[1:]):
        # A colored artist can cross the partially transparent frame. Bridge
        # only narrow interruptions, never a missing edge or a shifted box.
        if start - right <= 6:
            right = int(stop)
        else:
            runs.append((left, right))
            left, right = int(start), int(stop)
    runs.append((left, right))
    return max(runs, key=lambda pair: pair[1] - pair[0])


def rendered_bounds(path: Path, expected_width: float) -> list[float]:
    rgb = np.asarray(Image.open(path).convert("RGB"), dtype=np.int16)
    # Only the pale, gray legend frame has long achromatic runs inside the axes.
    gray = (rgb.max(axis=2) - rgb.min(axis=2) < 3)
    gray &= (rgb.min(axis=2) >= 170) & (rgb.max(axis=2) <= 245)
    gray[:60] = False
    gray[425:] = False
    gray[:, :82] = False
    gray[:, 575:] = False
    edges = [(y, *longest_run(row)) for y, row in enumerate(gray)]
    edges = [(y, left, right) for y, left, right in edges
             if right - left > expected_width * 0.55]
    if len(edges) < 2:
        raise AssertionError(f"legend border not detected in {path}")
    left = min(item[1] for item in edges)
    right = max(item[2] for item in edges)
    top, bottom = edges[0][0], edges[-1][0]
    return [float(left), float(top), float(right - left), float(bottom - top + 1)]


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifacts", type=Path)
    parser.add_argument("--output", type=Path,
                        default=Path("output/visual-audit/legend-parity"))
    args = parser.parse_args()
    if args.artifacts is None:
        candidates = list(Path("build/test/output").rglob("legend_best.png"))
        if not candidates:
            parser.error("run fo test test_legend_matplotlib first")
        args.artifacts = max(candidates, key=lambda p: p.stat().st_mtime).parent
    args.output.mkdir(parents=True, exist_ok=True)
    report = {"matplotlib": matplotlib.__version__, "cases": {}}
    failed = []
    for name in ["best", "crossing", "bar", *(f"location_{i:02}" for i in range(1, 11))]:
        default_expected = reference(name, args.output)
        expected = default_expected
        font_pair = None
        if name == "best":
            # Font family alone changes Matplotlib's best-position tie-break.
            # Fortplot's Linux default is Liberation Sans/Helvetica; render an
            # independent Matplotlib companion with that family. Keep the true
            # default-font reference and its discrepancy in the report as well.
            font_pair = "Liberation Sans"
            expected = reference(name, args.output, font_pair)
        for extension in ("png", "pdf"):
            path = args.artifacts / f"legend_{name}.{extension}"
            if extension == "pdf":
                prefix = args.output / f"fortplot_{name}_pdf"
                subprocess.run(["pdftoppm", "-r", "100", "-singlefile", "-png",
                                str(path), str(prefix)], check=True, capture_output=True)
                path = prefix.with_suffix(".png")
            actual = rendered_bounds(path, expected[2])
            error = np.abs(np.asarray(actual) - expected)
            # The allowed default-font difference changes the text width by a
            # few pixels; the anchor, point-based padding and row height still
            # must agree. A corner-versus-center regression fails decisively.
            passed = bool(np.all(error <= [5, 4, 7, 4]))
            key = f"{name}_{extension}"
            report["cases"][key] = {
                "expected_xywh": expected, "actual_xywh": actual,
                "absolute_error": error.tolist(), "pass": passed,
                "reference_font_family": font_pair or "Matplotlib default",
                "matplotlib_default_xywh": default_expected,
                "default_font_absolute_error":
                    np.abs(np.asarray(actual) - default_expected).tolist(),
            }
            if not passed:
                failed.append(key)
    report["failures"] = failed
    (args.output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    total = len(report["cases"])
    print(f"{total - len(failed)}/{total} legend comparisons passed")
    if failed:
        raise SystemExit("Failed: " + ", ".join(failed))


if __name__ == "__main__":
    main()
