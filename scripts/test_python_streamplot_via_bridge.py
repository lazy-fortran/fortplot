#!/usr/bin/env python3
import os
import sys
from pathlib import Path

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'python'))

import numpy as np


def main():
    import fortplot

    # Simple vector field: circular flow
    nx, ny = 20, 15
    x = np.linspace(-1.0, 1.0, nx)
    y = np.linspace(-1.0, 1.0, ny)
    xx, yy = np.meshgrid(x, y, indexing='ij')  # shape (nx, ny)
    u = -yy
    v = xx

    fortplot.figure()
    fortplot.streamplot(x, y, u, v, density=1.0)

    out_dir = Path(__file__).resolve().parent.parent / 'output'
    out_dir.mkdir(parents=True, exist_ok=True)
    out = out_dir / 'python_bridge_streamplot.png'
    fortplot.savefig(str(out))

    assert out.exists(), f"Output not created: {out}"
    assert out.stat().st_size > 1000, f"Output too small: {out.stat().st_size} bytes"
    print(f"OK: streamplot via bridge -> {out} ({out.stat().st_size} bytes)")


if __name__ == '__main__':
    main()

