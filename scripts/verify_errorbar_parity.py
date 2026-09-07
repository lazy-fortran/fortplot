#!/usr/bin/env python3
"""Compare rendered errorbar geometry against the installed Matplotlib renderer.

Run `fo test test_errorbar_geometry` first. Both PNGs and PDF rasterizations are
checked; label and font differences do not affect the colored-primitive oracle.
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


def color_mask(path: Path, channel: int) -> np.ndarray:
    rgb = np.asarray(Image.open(path).convert("RGB")) / 255.0
    others = [index for index in range(3) if index != channel]
    return (rgb[:, :, channel] > 0.8) & (rgb[:, :, others].max(axis=2) < 0.2)


def rasterize(path: Path, target: Path) -> Path:
    subprocess.run(
        ["pdftoppm", "-r", "100", "-singlefile", "-png", str(path), str(target)],
        check=True, capture_output=True,
    )
    return target.with_suffix(".png")


def measurements(path: Path, case: str) -> dict[str, float]:
    red = color_mask(path, 0)
    if case == "caps_y":
        return {"cap_span_px": int(red.sum(axis=1).max())}
    if case == "caps_x":
        return {"cap_span_px": int(red.sum(axis=0).max())}
    column = red[:, red.sum(axis=0).argmax()]
    rows = np.flatnonzero(column)
    blue_count = int(color_mask(path, 2).sum())
    if not len(rows):
        return {"stem_continuity": 0.0, "blue_line_pixels": blue_count}
    return {
        "stem_continuity": float(column.sum() / (rows[-1] - rows[0] + 1)),
        "blue_line_pixels": blue_count,
    }


def reference(case: str, output: Path) -> None:
    with matplotlib.rc_context(matplotlib.rcParamsDefault):
        fig, ax = plt.subplots(figsize=(6.4, 4.8), dpi=100)
        ax.set(xlim=(0, 1), ylim=(0, 1))
        if case == "independent_style":
            ax.errorbar([0.25, 0.75], [0.5, 0.5], yerr=[0.25, 0.25],
                        capsize=8, color="blue", ecolor="red", linestyle="--")
        else:
            errors = {"yerr" if case == "caps_y" else "xerr": [0.25]}
            ax.errorbar([0.5], [0.5], capsize=8, color="red", linestyle="none", **errors)
        for extension in ("png", "pdf"):
            fig.savefig(output / f"matplotlib_{case}.{extension}")
        plt.close(fig)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifacts", type=Path)
    parser.add_argument("--output", type=Path, default=Path("output/visual-audit/errorbar-parity"))
    args = parser.parse_args()
    if args.artifacts is None:
        candidates = list(Path("build/test/output").rglob("errorbar_caps_y.png"))
        if not candidates:
            parser.error("run fo test test_errorbar_geometry to generate fixtures")
        args.artifacts = max(candidates, key=lambda path: path.stat().st_mtime).parent
    args.output.mkdir(parents=True, exist_ok=True)
    results = {"matplotlib": matplotlib.__version__, "artifacts": str(args.artifacts), "cases": {}}
    failed = []
    for case in ("caps_y", "caps_x", "independent_style"):
        reference(case, args.output)
        for backend in ("png", "pdf"):
            actual = args.artifacts / f"errorbar_{case}.{backend}"
            expected = args.output / f"matplotlib_{case}.{backend}"
            if backend == "pdf":
                actual = rasterize(actual, args.output / f"fortplot_{case}_pdf")
                expected = rasterize(expected, args.output / f"matplotlib_{case}_pdf")
            observed, oracle = measurements(actual, case), measurements(expected, case)
            if case.startswith("caps"):
                passed = abs(observed["cap_span_px"] - oracle["cap_span_px"]) <= 2
            else:
                passed = observed["stem_continuity"] >= oracle["stem_continuity"] - 0.03
                passed = passed and observed["blue_line_pixels"] > 100
            name = f"{case}.{backend}"
            results["cases"][name] = {"fortplot": observed, "matplotlib": oracle, "passed": passed}
            if not passed:
                failed.append(name)
    (args.output / "results.json").write_text(json.dumps(results, indent=2) + "\n")
    print(json.dumps(results, indent=2))
    if failed:
        raise SystemExit("errorbar parity failed: " + ", ".join(failed))


if __name__ == "__main__":
    main()
