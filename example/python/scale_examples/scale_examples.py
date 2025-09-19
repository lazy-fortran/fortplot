#!/usr/bin/env python3
"""
Scale examples - Dual mode: fortplot or matplotlib
Equivalent to scale_examples.f90 for visual comparison
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

def log_scale_demo():
    """Log scale demonstration - equivalent to Fortran version"""
    print(f"=== Log Scale Examples ({backend}) ===")
    
    # Generate exponential data for log scale (same as Fortran)
    x_exp = np.arange(1, 51)
    y_exp = np.exp(x_exp * 0.2)
    
    plt.figure(figsize=(6.4, 4.8))
    plt.plot(x_exp, y_exp, linestyle="-")
    plt.yscale('log')
    plt.title('Log Scale Example')
    plt.xlabel('x')
    plt.ylabel('exp(0.2x)')
    plt.savefig(out('log_scale.png'))
    plt.savefig(out('log_scale.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('log_scale.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: log_scale.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

def symlog_scale_demo():
    """Symlog scale demonstration - equivalent to Fortran version"""
    print(f"=== Symlog Scale Examples ({backend}) ===")
    
    # Generate data that goes through zero for symlog (same as Fortran)
    x_exp = np.arange(1, 51)
    y_symlog = x_exp**3 - 50.0 * x_exp
    
    plt.figure(figsize=(6.4, 4.8))
    plt.plot(x_exp, y_symlog, linestyle="-")
    plt.yscale('symlog')  # Simplified for compatibility
    plt.title('Symlog Scale Example')
    plt.xlabel('x')
    plt.ylabel('x^3 - 50x')
    plt.savefig(out('symlog_scale.png'))
    plt.savefig(out('symlog_scale.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('symlog_scale.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: symlog_scale.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

if __name__ == "__main__":
    log_scale_demo()
    symlog_scale_demo()
