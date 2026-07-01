#!/usr/bin/env python3
"""Matplotlib mplot3d oracle for 3D tick-label placement (refs #2055).

Records the window-extent bounding box of every visible 3D tick label for the
``3d_helix`` and ``parametric_curve`` fixtures used by
``example/fortran/3d_plotting/3d_plotting.f90``. The figure size, dpi, title,
and default view (elev=30, azim=-60) match the fortplot example so the reported
boxes are a like-for-like reference for the Fortran tick-label layout.

Usage:
    python3 scripts/oracle_3d_tick_labels.py [--check]

Without ``--check`` it prints one line per label:
    <fixture> <axis> <text> x0 y0 x1 y1
With ``--check`` it additionally reports any pair of tick-label boxes on the
same fixture that overlap (which mplot3d avoids for these views).
"""
from __future__ import annotations

import argparse
import math
import sys


def helix():
    x = [math.cos(i * 0.1) for i in range(100)]
    y = [math.sin(i * 0.1) for i in range(100)]
    z = [i * 0.05 for i in range(100)]
    return x, y, z, "3D Line Plot - Helix"


def parametric_curve():
    n = 200
    x, y, z = [], [], []
    for i in range(n):
        theta = 6.0 * math.pi * i / (n - 1)
        x.append(math.cos(theta) * math.exp(-theta * 0.1))
        y.append(math.sin(theta) * math.exp(-theta * 0.1))
        z.append(theta * 0.1)
    return x, y, z, "3D Parametric Curve"


def boxes_overlap(a, b):
    return not (a[2] <= b[0] or b[2] <= a[0] or a[3] <= b[1] or b[3] <= a[1])


def collect(fixture_name, data):
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    x, y, z, title = data
    fig = plt.figure(figsize=(8, 6), dpi=100)
    ax = fig.add_subplot(111, projection="3d")
    ax.plot(x, y, z)
    ax.set_title(title)
    ax.view_init(elev=30, azim=-60)
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()

    labels = []
    for axis_name, axis in (("x", ax.xaxis), ("y", ax.yaxis), ("z", ax.zaxis)):
        for label in axis.get_ticklabels():
            if label.get_visible() and label.get_text():
                bb = label.get_window_extent(renderer)
                labels.append((fixture_name, axis_name, label.get_text(),
                               bb.x0, bb.y0, bb.x1, bb.y1))
    plt.close(fig)
    return labels


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()

    fixtures = {
        "3d_helix": helix(),
        "parametric_curve": parametric_curve(),
    }

    overlaps = 0
    for name, data in fixtures.items():
        labels = collect(name, data)
        for lb in labels:
            print(f"{lb[0]} {lb[1]} {lb[2]} "
                  f"{lb[3]:.2f} {lb[4]:.2f} {lb[5]:.2f} {lb[6]:.2f}")
        if args.check:
            for i in range(len(labels)):
                for j in range(i + 1, len(labels)):
                    if boxes_overlap(labels[i][3:], labels[j][3:]):
                        overlaps += 1
                        print(f"OVERLAP {name} {labels[i][2]} {labels[j][2]}",
                              file=sys.stderr)

    if args.check and overlaps:
        print(f"{overlaps} overlapping tick-label boxes", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
