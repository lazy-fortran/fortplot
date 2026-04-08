#!/usr/bin/env python3
"""Verify Python JSON-pipe support for field-based plot types."""

from __future__ import annotations

import json
import os
import sys
import tempfile
from pathlib import Path

import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "python"))

import fortplot


def assert_nonempty(path: Path) -> None:
    if not path.exists():
        raise AssertionError(f"missing output: {path}")
    if path.stat().st_size <= 0:
        raise AssertionError(f"empty output: {path}")


def load_json(path: Path) -> dict:
    with open(path, encoding="utf-8") as handle:
        return json.load(handle)


def field_layer(spec: dict) -> dict:
    layers = spec.get("layer")
    if not layers:
        raise AssertionError("expected layered spec for field plot")
    layer = layers[0]
    if "fortplotField" not in layer:
        raise AssertionError("missing fortplotField metadata")
    return layer


def test_contour(tmpdir: Path) -> None:
    x = np.linspace(-3.0, 3.0, 16)
    y = np.linspace(-2.0, 2.0, 12)
    X, Y = np.meshgrid(x, y)
    Z = np.sin(X) * np.cos(Y)

    fortplot.figure()
    fortplot.contour(X, Y, Z, levels=[-0.75, 0.0, 0.75])
    fortplot.xlabel("x")
    fortplot.ylabel("y")
    fortplot.title("Contour")

    json_path = tmpdir / "contour.vl.json"
    png_path = tmpdir / "contour.png"
    svg_path = tmpdir / "contour.svg"
    fortplot.savefig(json_path)
    fortplot.savefig(png_path)
    fortplot.savefig(svg_path)

    spec = load_json(json_path)
    layer = field_layer(spec)
    assert layer["mark"]["type"] == "contour"
    assert layer["fortplotField"]["levels"] == [-0.75, 0.0, 0.75]
    assert_nonempty(png_path)
    assert_nonempty(svg_path)


def test_contourf(tmpdir: Path) -> None:
    x = np.linspace(-3.0, 3.0, 18)
    y = np.linspace(-3.0, 3.0, 18)
    X, Y = np.meshgrid(x, y)
    Z = np.exp(-(X**2 + Y**2))

    fortplot.figure()
    fortplot.contourf(X, Y, Z, cmap="plasma")

    json_path = tmpdir / "contourf.vl.json"
    pdf_path = tmpdir / "contourf.pdf"
    fortplot.savefig(json_path)
    fortplot.savefig(pdf_path)

    spec = load_json(json_path)
    layer = field_layer(spec)
    assert layer["mark"]["type"] == "contour_filled"
    assert layer["fortplotField"]["colormap"] == "plasma"
    assert_nonempty(pdf_path)


def test_pcolormesh(tmpdir: Path) -> None:
    x = np.linspace(0.0, 1.0, 11)
    y = np.linspace(0.0, 1.0, 9)
    X, Y = np.meshgrid(x, y)
    C = np.sin(2.0 * np.pi * X[:-1, :-1]) * np.cos(2.0 * np.pi * Y[:-1, :-1])

    fortplot.figure()
    fortplot.pcolormesh(x, y, C, cmap="coolwarm", vmin=-1.0, vmax=1.0)

    json_path = tmpdir / "pcolormesh.vl.json"
    png_path = tmpdir / "pcolormesh.png"
    fortplot.savefig(json_path)
    fortplot.savefig(png_path)

    spec = load_json(json_path)
    layer = field_layer(spec)
    assert layer["mark"]["type"] == "pcolormesh"
    assert layer["fortplotField"]["vmin"] == -1.0
    assert layer["fortplotField"]["vmax"] == 1.0
    assert_nonempty(png_path)


def test_streamplot(tmpdir: Path) -> None:
    x = np.linspace(-2.0, 2.0, 20)
    y = np.linspace(-2.0, 2.0, 16)
    X, Y = np.meshgrid(x, y)
    U = -Y
    V = X

    fortplot.figure()
    fortplot.streamplot(X, Y, U, V, density=1.2)

    json_path = tmpdir / "streamplot.vl.json"
    png_path = tmpdir / "streamplot.png"
    fortplot.savefig(json_path)
    fortplot.savefig(png_path)

    spec = load_json(json_path)
    layer = field_layer(spec)
    assert layer["mark"]["type"] == "streamplot"
    assert abs(layer["fortplotField"]["density"] - 1.2) < 1.0e-12
    assert_nonempty(png_path)


def main() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        tmpdir = Path(tmp)
        test_contour(tmpdir)
        test_contourf(tmpdir)
        test_pcolormesh(tmpdir)
        test_streamplot(tmpdir)
    print("Python field plot JSON-pipe verification passed")


if __name__ == "__main__":
    main()
