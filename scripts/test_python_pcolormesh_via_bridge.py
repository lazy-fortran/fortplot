#!/usr/bin/env python3
import os
import sys
from pathlib import Path

# Add package path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'python'))

import numpy as np


def main():
    import fortplot

    # Simple regular grid
    x = np.linspace(0.0, 1.0, 11)  # 10 cells
    y = np.linspace(0.0, 1.0, 6)   # 5 cells
    xx, yy = np.meshgrid(x[:-1] + 0.05, y[:-1] + 0.1)
    c = np.sin(2*np.pi*xx) * np.cos(2*np.pi*yy)  # shape (5,10)

    fortplot.figure()
    fortplot.pcolormesh(x, y, c)

    out_dir = Path(__file__).resolve().parent.parent / 'output'
    out_dir.mkdir(parents=True, exist_ok=True)
    out = out_dir / 'python_bridge_pcolormesh.png'
    fortplot.savefig(str(out))

    assert out.exists(), f"Output not created: {out}"
    assert out.stat().st_size > 1000, f"Output too small: {out.stat().st_size} bytes"
    print(f"OK: pcolormesh via bridge -> {out} ({out.stat().st_size} bytes)")


if __name__ == '__main__':
    main()

