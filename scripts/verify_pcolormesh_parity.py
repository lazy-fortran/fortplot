#!/usr/bin/env python3
"""Check every mesh cell's color and geometry against actual Matplotlib.

Generate fixtures with `fo test test_pcolormesh_cell_geometry`, then pass the
reported artifact directory. PNG and rasterized PDF use independent references.
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


def rasterize(source: Path, destination: Path) -> Path:
    subprocess.run(["pdftoppm", "-r", "100", "-singlefile", "-png",
                    str(source), str(destination)], check=True, capture_output=True)
    return destination.with_suffix(".png")


def cell_geometry(path: Path, colors: np.ndarray, interior: tuple) -> list[dict]:
    pixels = np.asarray(Image.open(path).convert("RGB"), dtype=float)
    left, top, right, bottom = interior
    # Spine antialiasing/paint order belongs to the axes oracle. This cell oracle
    # measures shared interior pixels and every internal color boundary.
    pixels = pixels[top:bottom, left:right]
    result = []
    for color in colors:
        rows, columns = np.where(np.max(np.abs(pixels - color), axis=2) <= 2)
        if not len(rows):
            result.append({"pixels": 0, "bbox": None})
        else:
            result.append({"pixels": int(len(rows)),
                           "bbox": [int(columns.min()), int(rows.min()),
                                    int(columns.max()), int(rows.max())]})
    return result


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifacts", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    values = np.array([[0, 1, 2], [3, 4, 5]], dtype=float)
    cases = {
        "asymmetric": ([0, 1, 2, 3], [0, 1, 2], values),
        "nonuniform": ([0, .5, 2, 3], [0, .5, 2], values),
        "descending": ([3, 2, 1, 0], [2, 1, 0], values),
        "singleton": ([0, 3], [0, 2], np.array([[7.0]])),
    }
    report = {"matplotlib": matplotlib.__version__,
              "artifacts": str(args.artifacts), "cases": {}}
    failed = []
    with matplotlib.rc_context(matplotlib.rcParamsDefault):
        for case, (x, y, z) in cases.items():
            fig, ax = plt.subplots(figsize=(6.4, 4.8), dpi=100)
            mesh = ax.pcolormesh(x, y, z)
            ax.set(xlim=(0, 3), ylim=(0, 2))
            left, bottom, right, top = ax.get_window_extent().extents
            interior = (int(left) + 4, 480 - int(top) + 4,
                        int(right) - 4, 480 - int(bottom) - 4)
            colors = np.rint(255 * mesh.cmap(mesh.norm(np.unique(z)))[:, :3])
            for backend in ("png", "pdf"):
                actual = args.artifacts / f"pcolormesh_{case}.{backend}"
                reference = args.output / f"matplotlib_{case}.{backend}"
                fig.savefig(reference)
                if backend == "pdf":
                    actual = rasterize(actual, args.output / f"fortplot_{case}_pdf")
                    reference = rasterize(reference, args.output / f"matplotlib_{case}_pdf")
                observed = cell_geometry(actual, colors, interior)
                expected = cell_geometry(reference, colors, interior)
                checks = []
                for got, want in zip(observed, expected):
                    passed = got["bbox"] is not None and want["bbox"] is not None
                    if passed:
                        delta = np.abs(np.array(got["bbox"]) - want["bbox"])
                        passed = bool(delta.max() <= 2)
                        passed = passed and abs(got["pixels"] / want["pixels"] - 1) <= .06
                    checks.append(passed)
                name = f"{case}.{backend}"
                report["cases"][name] = {"fortplot": observed, "matplotlib": expected,
                                          "passed": all(checks)}
                if not all(checks):
                    failed.append(name)
            plt.close(fig)
    (args.output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({"passed": not failed, "failed": failed,
                      "report": str(args.output / "report.json")}, indent=2))
    if failed:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
