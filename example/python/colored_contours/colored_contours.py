#!/usr/bin/env python3
"""Demonstrates colored contour plots with different colormaps - Dual mode: fortplot or matplotlib"""

import sys
from pathlib import Path
from typing import Optional
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

def default_gaussian_example():
    """Default colorblind-safe Gaussian example"""
    print(f"=== Default Colorblind-Safe Gaussian Example ({backend}) ===")
    
    # Generate grid
    x_grid = np.linspace(-3.0, 3.0, 30)
    y_grid = np.linspace(-3.0, 3.0, 30)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # 2D Gaussian
    Z = np.exp(-(X**2 + Y**2))
    
    plt.figure(figsize=(8, 6))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("2D Gaussian - Default Colorblind-Safe Colormap")
    
    # Use contourf for filled contours
    plt.contourf(X, Y, Z)
    
    plt.savefig(out('gaussian_default.png'))
    plt.savefig(out('gaussian_default.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('gaussian_default.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: gaussian_default.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

def plasma_saddle_example():
    """Plasma saddle function example"""
    print(f"=== Plasma Saddle Function Example ({backend}) ===")
    
    # Generate grid
    x_grid = np.linspace(-2.5, 2.5, 25)
    y_grid = np.linspace(-2.5, 2.5, 25)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # Saddle function: x^2 - y^2
    Z = X**2 - Y**2
    
    # Custom contour levels
    custom_levels = [-6.0, -4.0, -2.0, -1.0, 1.0, 2.0, 4.0, 6.0]
    
    plt.figure(figsize=(8, 6))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Saddle Function - Plasma Colormap")
    
    # Custom contour levels
    plt.contourf(X, Y, Z, custom_levels)
    
    plt.savefig(out('saddle_plasma.png'))
    plt.savefig(out('saddle_plasma.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('saddle_plasma.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: saddle_plasma.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

def mixed_colormap_comparison():
    """Colormap comparison"""
    print(f"=== Colormap Comparison ({backend}) ===")
    
    # Generate grid
    x_grid = np.linspace(-2.0, 2.0, 20)
    y_grid = np.linspace(-2.0, 2.0, 20)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # Ripple function
    Z = np.sin(np.sqrt(X**2 + Y**2) * 3.0) * np.exp(-0.3 * np.sqrt(X**2 + Y**2))
    
    # Ripple inferno
    plt.figure(figsize=(8, 6))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Ripple Function - Inferno Colormap")
    plt.contourf(X, Y, Z)
    plt.savefig(out('ripple_inferno.png'))
    plt.savefig(out('ripple_inferno.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('ripple_inferno.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    # Ripple coolwarm
    plt.figure(figsize=(8, 6))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Ripple Function - Coolwarm Colormap")
    plt.contourf(X, Y, Z)
    plt.savefig(out('ripple_coolwarm.png'))
    plt.savefig(out('ripple_coolwarm.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('ripple_coolwarm.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    # Jet colormap
    plt.figure(figsize=(8, 6))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Ripple Function - Jet Colormap")
    plt.contourf(X, Y, Z)
    plt.savefig(out('ripple_jet.png'))
    plt.savefig(out('ripple_jet.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('ripple_jet.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: ripple_inferno.png/pdf, ripple_coolwarm.png/pdf, ripple_jet.png/pdf" + ("" if backend == "matplotlib" else " + txt files"))
    print("Colormap comparison complete!")

if __name__ == "__main__":
    default_gaussian_example()
    plasma_saddle_example()
    mixed_colormap_comparison()
