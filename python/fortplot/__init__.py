"""
Lightweight package initializer for fortplot.

Avoid importing heavy submodules (and third‑party deps) at import time.
Provides lazy access to symbols from `.fortplot` when actually used.
"""

from typing import Any
import importlib


def __getattr__(name: str) -> Any:  # PEP 562 lazy import
    _fortplot = importlib.import_module('.fortplot', __package__)
    return getattr(_fortplot, name)


__all__ = []
