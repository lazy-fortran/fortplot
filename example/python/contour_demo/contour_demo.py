#!/usr/bin/env python3
"""
Contour plotting examples - Dual mode: fortplot or matplotlib
Equivalent to contour_demo.f90 for visual comparison
"""

import sys
from pathlib import Path
import numpy as np

# Dual-mode import: --matplotlib uses matplotlib, default uses fortplot
if "--matplotlib" in sys.argv:
    import matplotlib.pyplot as plt
    backend = "matplotlib"
else:
    import fortplot.fortplot as plt
    backend = "fortplot"

def _parse_outdir() -> Path:
    args = sys.argv[:]
    outdir_arg: str | None = None
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

def gaussian_contours():
    """Gaussian contour demonstration - equivalent to Fortran version"""
    print(f"=== Contour Examples ({backend}) ===")
    
    # Generate contour grid (same as Fortran)
    x_grid = np.linspace(-3.0, 3.0, 30)
    y_grid = np.linspace(-3.0, 3.0, 30)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # Gaussian contour
    Z = np.exp(-(X**2 + Y**2))
    
    plt.figure(figsize=(6.4, 4.8))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("2D Gaussian Function")
    
    # Create contour plot
    plt.contour(X, Y, Z)
    
    plt.savefig(out('contour_gaussian.png'))
    plt.savefig(out('contour_gaussian.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('contour_gaussian.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: contour_gaussian.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

def mixed_contour_line_plot():
    """Mixed contour and line plot - equivalent to Fortran version"""
    # Generate grid (same as Fortran)
    x_grid = np.linspace(-3.0, 3.0, 30)
    y_grid = np.linspace(-3.0, 3.0, 30)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # Saddle function with custom levels
    Z = X**2 - Y**2
    custom_levels = np.array([-4.0, -2.0, 0.0, 2.0, 4.0])
    
    plt.figure(figsize=(6.4, 4.8))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Mixed Plot: Contour + Line")
    
    # Add contour with custom levels
    plt.contour(X, Y, Z, custom_levels)
    
    # Add line plot (cross-section at y=0)
    plt.plot(x_grid, np.exp(-x_grid**2), label="Cross-section at y=0")
    
    plt.legend()
    
    plt.savefig(out('mixed_plot.png'))
    plt.savefig(out('mixed_plot.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('mixed_plot.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: mixed_plot.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

if __name__ == "__main__":
    gaussian_contours()
    mixed_contour_line_plot()
