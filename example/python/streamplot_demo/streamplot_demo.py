#!/usr/bin/env python3
"""Streamplot demo - demonstrates streamline plotting"""

import numpy as np
import sys
from pathlib import Path
from typing import Optional

if "--matplotlib" in sys.argv:
    import matplotlib.pyplot as plt
    backend = "matplotlib"
else:
    import fortplot.fortplot as plt
    backend = "fortplot"

def _parse_outdir() -> Path:
    args = sys.argv[:]
    outdir_arg: Optional[str] = None
    for i, a in enumerate(list(args)):
        if a.startswith("--outdir="):
            outdir_arg = a.split("=", 1)[1]
            sys.argv.pop(i)
            break
        if a == "--outdir" and i + 1 < len(args):
            outdir_arg = args[i + 1]
            del sys.argv[i:i+2]
            break
    if outdir_arg:
        p = Path(outdir_arg).expanduser().resolve()
    else:
        repo_root = Path(__file__).resolve().parents[3]
        example_name = Path(__file__).resolve().parent.name
        backend_dir = "pyplot" if backend == "matplotlib" else "fortplot"
        p = repo_root / "output" / "example" / "python" / backend_dir / example_name
    p.mkdir(parents=True, exist_ok=True)
    return p

_OUTDIR = _parse_outdir()
def out(name: str) -> str:
    return str((_OUTDIR / name))

def main():
    nx, ny = 20, 20

    # Create grid
    x = np.linspace(-2.0, 2.0, nx)
    y = np.linspace(-2.0, 2.0, ny)
    X, Y = np.meshgrid(x, y)

    # Create circular flow field
    U = -Y
    V = X

    # Create figure and add streamplot
    plt.figure(figsize=(10, 7.5))
    plt.streamplot(X, Y, U, V, density=1.0)
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.title('Streamline Plot Demo - Circular Flow')
    plt.savefig(out('streamplot_demo.png'))
    plt.savefig(out('streamplot_demo.pdf'))

    # Arrow variant: emphasize direction with arrowheads
    plt.figure(figsize=(10, 7.5))
    plt.streamplot(
        X, Y, U, V,
        density=1.0,
        arrowsize=1.5,
        arrowstyle='->'
    )
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.title('Streamline Plot Demo - With Arrows')
    plt.savefig(out('streamplot_arrows.png'))
    plt.savefig(out('streamplot_arrows.pdf'))

    # Export TXT for fortplot mode
    if backend == "fortplot":
        with open(out('streamplot_demo.txt'), 'w') as f:
            f.write("Streamline Plot Demo - Circular Flow\n")
            f.write("=====================================\n\n")
            f.write("Grid size: {}x{}\n".format(nx, ny))
            f.write("X range: -2.0 to 2.0\n")
            f.write("Y range: -2.0 to 2.0\n")
            f.write("Flow field: Circular (U=-Y, V=X)\n")
            f.write("Density: 1.0\n")
        with open(out('streamplot_arrows.txt'), 'w') as f:
            f.write("Streamline Plot Demo - With Arrows\n")
            f.write("===================================\n\n")
            f.write("Grid size: {}x{}\n".format(nx, ny))
            f.write("Arrowstyle: ->\n")
            f.write("Arrowsize: 1.5\n")

    print('Streamplot demo completed!')

if __name__ == "__main__":
    main()
