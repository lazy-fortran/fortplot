"""
fortplot.axes - Axes and label management
==========================================

Matplotlib-compatible axes helpers for the fortplot Python interface.
"""

from __future__ import annotations

from typing import Any, Optional, Tuple

import fortplot.fortplot_wrapper as _fortplot

_CURRENT_XLIM: Optional[Tuple[float, float]] = None
_CURRENT_YLIM: Optional[Tuple[float, float]] = None
_GRID_STATE: Optional[bool] = None


class _TextPlaceholder:
    """Minimal matplotlib Text placeholder."""

    __slots__ = ("_text",)

    def __init__(self, text: str) -> None:
        self._text = text

    def get_text(self) -> str:
        return self._text

    def set_text(self, text: str) -> None:
        self._text = text


def title(label: Any = None, *args: Any, **kwargs: Any) -> _TextPlaceholder:
    label_text = _coerce_label(label, kwargs)
    _fortplot.fortplot.title(label_text)
    return _TextPlaceholder(label_text)


def xlabel(label: Any = None, *args: Any, **kwargs: Any) -> _TextPlaceholder:
    label_text = _coerce_label(label, kwargs)
    _fortplot.fortplot.xlabel(label_text)
    return _TextPlaceholder(label_text)


def ylabel(label: Any = None, *args: Any, **kwargs: Any) -> _TextPlaceholder:
    label_text = _coerce_label(label, kwargs)
    _fortplot.fortplot.ylabel(label_text)
    return _TextPlaceholder(label_text)


def legend(*args: Any, **kwargs: Any) -> None:
    _ = args  # Consume positional handles for compatibility
    _ = kwargs  # Accept keyword arguments without affecting backend yet
    _fortplot.fortplot.legend()


def grid(b: Any = None, which: Optional[str] = None, axis: Optional[str] = None,
         **kwargs: Any) -> None:
    global _GRID_STATE

    linestyle = kwargs.get("linestyle", kwargs.get("ls"))
    alpha = kwargs.get("alpha")

    if b is None:
        if which is None and axis is None and alpha is None and linestyle is None:
            enabled = not _GRID_STATE if _GRID_STATE is not None else True
        else:
            enabled = True
    else:
        enabled = bool(b)

    _GRID_STATE = enabled
    _fortplot.fortplot.grid(enabled=enabled, which=which, axis=axis,
                            alpha=alpha, linestyle=linestyle)


def xscale(scale: str, *args: Any, **kwargs: Any) -> None:
    threshold = kwargs.get("linthresh", kwargs.get("linthreshx"))
    _fortplot.fortplot.set_xscale(scale, threshold)


def yscale(scale: str, *args: Any, **kwargs: Any) -> None:
    threshold = kwargs.get("linthresh", kwargs.get("linthreshy"))
    _fortplot.fortplot.set_yscale(scale, threshold)


def xlim(*args: Any, **kwargs: Any):
    global _CURRENT_XLIM
    left = kwargs.get("xmin", kwargs.get("left"))
    right = kwargs.get("xmax", kwargs.get("right"))

    if len(args) == 1:
        pair = args[0]
        if isinstance(pair, (list, tuple)) and len(pair) >= 2:
            left, right = pair[0], pair[1]
    elif len(args) >= 2:
        left, right = args[0], args[1]

    if left is None and right is None:
        return _CURRENT_XLIM

    if left is None and _CURRENT_XLIM is not None:
        left = _CURRENT_XLIM[0]
    if right is None and _CURRENT_XLIM is not None:
        right = _CURRENT_XLIM[1]

    if left is None or right is None:
        raise ValueError("xlim requires finite bounds")

    xmin = float(left)
    xmax = float(right)
    _fortplot.fortplot.xlim(xmin, xmax)
    _CURRENT_XLIM = (xmin, xmax)
    return _CURRENT_XLIM


def ylim(*args: Any, **kwargs: Any):
    global _CURRENT_YLIM
    bottom = kwargs.get("ymin", kwargs.get("bottom"))
    top = kwargs.get("ymax", kwargs.get("top"))

    if len(args) == 1:
        pair = args[0]
        if isinstance(pair, (list, tuple)) and len(pair) >= 2:
            bottom, top = pair[0], pair[1]
    elif len(args) >= 2:
        bottom, top = args[0], args[1]

    if bottom is None and top is None:
        return _CURRENT_YLIM

    if bottom is None and _CURRENT_YLIM is not None:
        bottom = _CURRENT_YLIM[0]
    if top is None and _CURRENT_YLIM is not None:
        top = _CURRENT_YLIM[1]

    if bottom is None or top is None:
        raise ValueError("ylim requires finite bounds")

    ymin = float(bottom)
    ymax = float(top)
    _fortplot.fortplot.ylim(ymin, ymax)
    _CURRENT_YLIM = (ymin, ymax)
    return _CURRENT_YLIM


def _coerce_label(label: Any, kwargs: Any) -> str:
    if label is None:
        label = kwargs.get("label", "")
    return "" if label is None else str(label)
