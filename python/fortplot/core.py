"""
fortplot.core - Core plotting functions
========================================

Core figure management and pyplot-compatible plotting utilities for the
fortplot Python interface.
"""

from __future__ import annotations

from collections.abc import Iterable
from typing import Any, Dict, Optional, Sequence, Tuple

import fortplot.fortplot_wrapper as _fortplot

DEFAULT_DPI = 100


class _FigurePlaceholder:
    """Lightweight matplotlib-compatible figure placeholder."""

    __slots__ = ("_figsize", "_dpi")

    def __init__(self, figsize: Sequence[float], dpi: float) -> None:
        self._figsize = (float(figsize[0]), float(figsize[1]))
        self._dpi = float(dpi)

    def get_size_inches(self) -> Tuple[float, float]:
        return self._figsize

    def set_size_inches(self, *size: Any, **kwargs: Any) -> Tuple[float, float]:
        if size:
            if isinstance(size[0], Iterable):
                dims = tuple(size[0])
            else:
                dims = size
        else:
            width = kwargs.get("w") or kwargs.get("width")
            height = kwargs.get("h") or kwargs.get("height")
            dims = (width, height)
        if len(dims) >= 2 and dims[0] is not None and dims[1] is not None:
            self._figsize = (float(dims[0]), float(dims[1]))
        return self._figsize

    def get_dpi(self) -> float:
        return self._dpi

    def set_dpi(self, dpi: float) -> None:
        self._dpi = float(dpi)

    def __repr__(self) -> str:  # pragma: no cover - debugging helper
        width, height = self._figsize
        return f"<fortplot.Figure size {width:.2f}x{height:.2f} at {self._dpi:.0f}dpi>"


class _Line2DPlaceholder:
    """Minimal matplotlib Line2D placeholder for compatibility."""

    __slots__ = ("_label",)

    def __init__(self, label: str = "") -> None:
        self._label = label

    def get_label(self) -> str:
        return self._label

    def set_label(self, label: str) -> None:
        self._label = label


def _ensure_array(obj: Any):
    """Convert input to a sequence suitable for plotting."""
    try:
        import numpy as np  # local import to avoid hard dependency

        if isinstance(obj, np.ndarray):
            return obj
        return np.array(list(obj) if isinstance(obj, range) else obj)
    except Exception:
        if isinstance(obj, (list, tuple, range)):
            return list(obj)
        if isinstance(obj, Iterable):
            return list(obj)
        return [obj]


def _resolve_data_argument(args: Tuple[Any, ...], data: Optional[Dict[str, Any]]):
    if data is None:
        return args
    resolved = []
    for value in args:
        if isinstance(value, str) and value in data:
            resolved.append(data[value])
        else:
            resolved.append(value)
    return tuple(resolved)


def figure(*args: Any, **kwargs: Any) -> _FigurePlaceholder:
    """Create a new figure with matplotlib-compatible signature."""

    figsize = kwargs.pop("figsize", None)
    dpi = kwargs.pop("dpi", DEFAULT_DPI)
    if figsize is None:
        for candidate in args:
            if isinstance(candidate, (list, tuple)) and len(candidate) == 2:
                figsize = candidate
                break
    if figsize is None:
        figsize = (6.4, 4.8)

    width = int(float(figsize[0]) * float(dpi))
    height = int(float(figsize[1]) * float(dpi))
    _fortplot.fortplot.figure(width, height)
    return _FigurePlaceholder(figsize, dpi)


def plot(*args: Any, **kwargs: Any):
    """Matplotlib-compatible plot wrapper."""

    if len(args) == 0:
        raise TypeError("plot() missing required data arguments")

    data = kwargs.pop("data", None)
    args = _resolve_data_argument(args, data)

    fmt = None
    if len(args) == 1:
        y = _ensure_array(args[0])
        x = _ensure_array(range(len(y)))
    else:
        x = _ensure_array(args[0])
        y = _ensure_array(args[1])
        if len(args) >= 3 and isinstance(args[2], str):
            fmt = args[2]

    label = kwargs.pop("label", "")
    linestyle = kwargs.pop("linestyle", kwargs.pop("ls", None))
    if linestyle is None and fmt is not None:
        if "--" in fmt:
            linestyle = "--"
        elif "-." in fmt:
            linestyle = "-."
        elif ":" in fmt:
            linestyle = ":"
        elif "-" in fmt:
            linestyle = "-"
        else:
            linestyle = "None"
    if linestyle is None:
        linestyle = "-"

    label_str = "" if label is None else str(label)
    _fortplot.fortplot.plot(x, y, label_str, linestyle)
    return [_Line2DPlaceholder(label_str)]


def savefig(fname: Any, *args: Any, **kwargs: Any) -> None:
    """Save figure to disk, mirroring matplotlib signature."""

    if fname is None and args:
        fname = args[0]
    if fname is None:
        raise TypeError("savefig() missing filename")
    filename = str(fname)
    _fortplot.fortplot.savefig(filename)


def show(*args: Any, **kwargs: Any) -> None:
    """Display figures with matplotlib-compatible parameters."""

    block = kwargs.pop("block", None)
    if block is None and args:
        block = args[0]
    if block is None:
        block = True
    _fortplot.fortplot.show_figure(bool(block))
