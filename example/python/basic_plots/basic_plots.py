#!/usr/bin/env python3
"""
Basic plotting examples - Dual mode: fortplot or matplotlib
Equivalent to basic_plots.f90 for visual comparison
"""

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

# Determine output directory (default to repo_root/output/example/python/<backend>/<example>/)
# Support optional --outdir <path> or --outdir=<path>
def _parse_outdir() -> Path:
    args = sys.argv[:]
    outdir_arg: Optional[str] = None
    # consume --outdir forms
    for i, a in enumerate(list(args)):
        if a.startswith("--outdir="):
            outdir_arg = a.split("=", 1)[1]
            sys.argv.pop(i)
            break
        if a == "--outdir" and i + 1 < len(args):
            outdir_arg = args[i + 1]
            # remove both
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

def simple_plots():
    """Simple plot - equivalent to Fortran functional API"""
    print(f"=== Basic Plots ({backend}) ===")
    
    # Generate simple sine data - show 2 complete periods (0 to 4π) - exactly like Fortran
    x = np.arange(50) * 4.0 * np.pi / 49.0
    y = np.sin(x)
    
    # Simple plot
    plt.figure(figsize=(6.4, 4.8))
    plt.plot(x, y, label='sin(x)')
    plt.title('Simple Sine Wave')
    plt.xlabel('x')
    plt.ylabel('sin(x)')
    plt.savefig(out('simple_plot.png'))
    plt.savefig(out('simple_plot.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('simple_plot.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: simple_plot.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

def multi_line_plot():
    """Multi-line plot - equivalent to Fortran OO interface"""
    # Generate data exactly like Fortran: x from 0 to 99, divided by 5
    x = np.arange(100) / 5.0
    sx = np.sin(x)
    cx = np.cos(x)
    
    # Multi-line plot - match Fortran exactly
    plt.figure(figsize=(6.4, 4.8))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Sine and Cosine Functions")
    plt.plot(x, sx, label="sin(x)")
    plt.plot(x, cx, label="cos(x)")
    plt.legend()
    plt.savefig(out('multi_line.png'))
    plt.savefig(out('multi_line.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('multi_line.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: multi_line.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

if __name__ == "__main__":
    simple_plots()
    multi_line_plot()
