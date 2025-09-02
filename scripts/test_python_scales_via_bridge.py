#!/usr/bin/env python3
import os
import sys
from pathlib import Path

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'python'))

import numpy as np


def main():
    import fortplot

    x = np.logspace(-1, 1, 50)
    y = x**2

    fortplot.figure()
    fortplot.plot(x, y)
    # Set log scales via bridge
    fortplot.xscale('log')
    fortplot.yscale('log')

    out_dir = Path(__file__).resolve().parent.parent / 'output'
    out_dir.mkdir(parents=True, exist_ok=True)
    out = out_dir / 'python_bridge_scales_log.png'
    fortplot.savefig(str(out))

    assert out.exists(), f"Output not created: {out}"
    assert out.stat().st_size > 1000, f"Output too small: {out.stat().st_size} bytes"
    print(f"OK: scales via bridge -> {out} ({out.stat().st_size} bytes)")


if __name__ == '__main__':
    main()

