#!/usr/bin/env python3
import os
import sys
import tempfile

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'python'))

import numpy as np
import pytest

try:
    import fortplot
    FORTPLOT_AVAILABLE = all(
        hasattr(fortplot, name) for name in ("figure", "scatter", "savefig")
    )
except Exception:
    FORTPLOT_AVAILABLE = False


pytestmark = pytest.mark.skipif(not FORTPLOT_AVAILABLE, reason="fortplot not available")


def test_scatter_accepts_matplotlib_kwargs(tmp_path):
    import fortplot

    x = np.linspace(0, 1, 10)
    y = np.linspace(1, 0, 10)

    # Should not raise for common matplotlib kwargs
    fortplot.figure()
    fortplot.scatter(
        x,
        y,
        marker='o',
        alpha=0.5,
        edgecolors='k',
        linewidths=2.0,
        label='pts',
    )

    out = tmp_path / "scatter_kwargs.png"
    fortplot.savefig(str(out))
    assert out.exists() and out.stat().st_size > 0
