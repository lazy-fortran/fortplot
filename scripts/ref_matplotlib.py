#!/usr/bin/env python3
"""Render the independent default-Matplotlib oracle for app/mpl_parity.f90.

No fortplot settings or pixels enter reference generation. The manifest records
Matplotlib's version, default rcParams, axis geometry and every visible label.
"""

import argparse
import json
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np


MATH_TITLES = {
    "math_scripts": r"$x_i^2 + y_{i+1}^{n-1} = e^{-x/3}$",
    "math_fraction": r"$\frac{1}{2} + \frac{x^2}{1+x}$",
    "math_radical": r"$\sqrt{x^2+y^2} = \alpha + \beta$",
}
CASES = (
    "line", "scatter", "bar", "hist", "errorbar", "logy", "markers",
    "grid", "fill_between", "subplots", *MATH_TITLES,
)


def create_case(name):
    """Use the same data and public options as the Fortran driver."""
    fig, ax = plt.subplots()
    ax.set(xlabel="x", ylabel="y")
    if name == "line":
        x = np.arange(100) / 5.0
        ax.plot(x, np.sin(x), label="sin(x)")
        ax.plot(x, np.cos(x), label="cos(x)")
        ax.set_title("Sine and Cosine Functions")
        ax.legend()
    elif name == "scatter":
        x = np.arange(1, 21, dtype=float)
        ax.scatter(x, x**2)
        ax.set_title("Scatter")
    elif name == "bar":
        ax.bar([1., 2., 3., 4.], [4.5, 5.8, 6.1, 6.7])
        ax.set(xlabel="category", ylabel="value", title="Bar")
    elif name == "hist":
        data = (np.arange(1, 201) * 13 + 7) % 100 / 10.0
        ax.hist(data, bins=10)
        ax.set(xlabel="value", ylabel="count", title="Histogram")
    elif name == "errorbar":
        ax.errorbar(np.arange(1., 6.), [2., 4., 5., 4.5, 6.], yerr=0.5)
        ax.set_title("Errorbar")
    elif name == "logy":
        x = np.arange(1., 51.)
        ax.plot(x, 10.0**(x / 12.0))
        ax.set(yscale="log", title="Log Y")
    elif name == "markers":
        x = np.arange(9, dtype=float)
        ax.plot(x, np.sin(x), linestyle="--", marker="o", label="circles")
        ax.plot(x, np.cos(x), linestyle=":", marker="s", label="squares")
        ax.set_title("Markers and Line Styles")
        ax.legend()
    elif name == "grid":
        x = np.arange(100) / 10.0
        ax.plot(x, np.sin(x))
        ax.grid(True)
        ax.set_title("Default Grid")
    elif name == "fill_between":
        x = np.arange(41) / 10.0
        ax.fill_between(x, np.sin(x), 0.5 * np.sin(x))
        ax.set_title("Filled Band")
    elif name == "subplots":
        plt.close(fig)
        fig, axes = plt.subplots(2, 2)
        x = np.arange(30) / 5.0
        for index, ax in enumerate(axes.flat, 1):
            ax.plot(x, np.sin(x + index))
            ax.set(xlabel="x", ylabel="y", title=f"Panel {index}")
    elif name in MATH_TITLES:
        x = np.arange(21) / 10.0
        ax.plot(x, x**2)
        ax.set(xlabel=r"$x_i$", ylabel=r"$y^2$", title=MATH_TITLES[name])
    else:
        raise ValueError(f"unknown reference case: {name}")
    return fig


def visible_labels(fig):
    yield from fig.texts
    for ax in fig.axes:
        yield from (ax.title, ax.xaxis.label, ax.yaxis.label)
        yield from ax.texts
        if ax.get_legend() is not None:
            yield from ax.get_legend().get_texts()
        for axis in (ax.xaxis, ax.yaxis):
            low, high = sorted(axis.get_view_interval())
            yield axis.get_offset_text()
            for tick in (*axis.get_major_ticks(), *axis.get_minor_ticks()):
                if low <= tick.get_loc() <= high:
                    yield from (tick.label1, tick.label2)


def geometry(fig):
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    width, height = fig.canvas.get_width_height()
    labels = []
    for item in visible_labels(fig):
        if not item.get_visible() or not item.get_text():
            continue
        box = item.get_window_extent(renderer)
        if box.x1 < 0 or box.y1 < 0 or box.x0 > width or box.y0 > height:
            continue
        if item.axes is not None and not item.axes.get_visible():
            continue
        labels.append({
            "text": item.get_text(),
            "bbox": [box.x0, height - box.y1, box.x1, height - box.y0],
            "math": "$" in item.get_text(),
        })
    return {
        "size": [width, height], "dpi": fig.dpi, "labels": labels,
        "axes": [{"bbox": list(ax.bbox.bounds), "xlim": list(ax.get_xlim()),
                  "ylim": list(ax.get_ylim()), "xticks": list(ax.get_xticks()),
                  "yticks": list(ax.get_yticks())} for ax in fig.axes],
    }


def generate(outdir, cases=CASES):
    outdir = Path(outdir)
    outdir.mkdir(parents=True, exist_ok=True)
    manifest = {"schema": 1, "matplotlib": matplotlib.__version__, "cases": {}}
    # Prevent a user's matplotlibrc/style from changing the independent oracle.
    with matplotlib.rc_context(matplotlib.rcParamsDefault):
        manifest["defaults"] = {key: matplotlib.rcParams[key] for key in (
            "figure.figsize", "figure.dpi", "font.size", "font.family",
            "lines.linewidth", "lines.markersize", "axes.linewidth",
            "axes.xmargin", "axes.ymargin", "mathtext.fontset",
            "xtick.major.size", "ytick.major.size", "xtick.direction",
            "ytick.direction", "axes.labelpad", "axes.titlesize",
        )}
        manifest["colors"] = [matplotlib.colors.to_hex(color) for color in
                              matplotlib.rcParams["axes.prop_cycle"].by_key()["color"]]
        for name in cases:
            fig = create_case(name)
            manifest["cases"][name] = geometry(fig)
            for suffix in ("png", "pdf"):
                fig.savefig(outdir / f"mpl_{name}.{suffix}")
            plt.close(fig)
    (outdir / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    return manifest


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("outdir", type=Path)
    parser.add_argument("--case", action="append", choices=CASES)
    args = parser.parse_args()
    generate(args.outdir, args.case or CASES)
