#!/usr/bin/env python3
"""Matplotlib oracle for subplot y-label spacing.

Reports, per subplot, the horizontal geometry that fortplot's raster backend
must match: the y tick-label bounding boxes and the y-axis label bounding box,
in device pixels at the same DPI fortplot renders the subplot demo.

The load-bearing number is the pad between the outer (left) edge of the y
tick labels and the right edge of the rotated y-axis label. That pad is
matplotlib's axes.labelpad (4.0 pt) plus tick-label metrics, and is what the
Fortran renderer reproduces via pt2px(AXIS_LABEL_PAD_PT).

Run: python3 scripts/oracle_subplot_ylabel_spacing.py
"""
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np


def report(fig, axs, titles):
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    for ax, name in zip(axs, titles):
        ylabel_box = ax.yaxis.label.get_window_extent(renderer)
        tick_boxes = [
            t.get_window_extent(renderer)
            for t in ax.get_yticklabels()
            if t.get_visible() and t.get_text()
        ]
        if not tick_boxes:
            continue
        tick_right = max(b.x1 for b in tick_boxes)
        tick_left = min(b.x0 for b in tick_boxes)
        span = tick_right - ylabel_box.x1
        pad = tick_left - ylabel_box.x1
        print(
            f"{name:20s} ylabel_x1={ylabel_box.x1:7.2f} "
            f"tick_left={tick_left:7.2f} tick_right={tick_right:7.2f} "
            f"span={span:7.2f} pad={pad:7.2f}"
        )


def demo_2x2():
    x = np.arange(0, 100) * 0.1
    x2 = np.arange(0, 50) * 0.2
    y2 = np.exp(-x2 * 0.5) * np.cos(2.0 * x2)
    fig, axs = plt.subplots(2, 2, figsize=(8, 6), dpi=100)
    fig.suptitle("Trigonometric and Polynomial Functions")
    axs[0, 0].plot(x, np.sin(x)); axs[0, 0].set_title("Sine Wave")
    axs[0, 0].set_xlabel("x"); axs[0, 0].set_ylabel("sin(x)")
    axs[0, 1].plot(x, np.cos(x)); axs[0, 1].set_title("Cosine Wave")
    axs[0, 1].set_xlabel("x"); axs[0, 1].set_ylabel("cos(x)")
    axs[1, 0].plot(x2, y2); axs[1, 0].set_title("Damped Oscillation")
    axs[1, 0].set_xlabel("x"); axs[1, 0].set_ylabel("y")
    axs[1, 1].plot(x2, x2 ** 2 / 50.0); axs[1, 1].set_title("Quadratic Function")
    axs[1, 1].set_xlabel("x"); axs[1, 1].set_ylabel("x^2/50")
    print("== 2x2 ==")
    report(fig, axs.flat, ["sin(x)", "cos(x)", "y", "x^2/50"])


def demo_1x3():
    x2 = np.arange(0, 50) * 0.2
    fig, axs = plt.subplots(1, 3, figsize=(9, 3), dpi=100)
    fig.suptitle("Polynomial Growth Comparison")
    axs[0].plot(x2, x2); axs[0].set_title("Linear")
    axs[0].set_xlabel("x"); axs[0].set_ylabel("x")
    axs[1].plot(x2, x2 ** 2); axs[1].set_title("Quadratic")
    axs[1].set_xlabel("x"); axs[1].set_ylabel("x^2")
    axs[2].plot(x2, x2 ** 3); axs[2].set_title("Cubic")
    axs[2].set_xlabel("x"); axs[2].set_ylabel("x^3")
    print("== 1x3 ==")
    report(fig, axs.flat, ["x", "x^2", "x^3"])


if __name__ == "__main__":
    demo_2x2()
    demo_1x3()
