#!/usr/bin/env python3
"""
Line styles demonstration - Dual mode: fortplot or matplotlib
Equivalent to line_styles.f90 for visual comparison
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
    print(f"=== Line Style Examples ({backend}) ===")
    
    # Generate test data with clear separation for visibility (same as Fortran)
    x = np.arange(50) * 0.2
    y1 = np.sin(x) + 2.0
    y2 = np.cos(x) + 1.0
    y3 = np.sin(x * 2.0)
    y4 = np.cos(x * 3.0) - 1.0
    y5 = np.sin(x * 0.5) - 2.0
    
    # Comprehensive line style demonstration
    plt.figure(figsize=(8, 6))
    plt.plot(x, y1, label='Solid (-)', linestyle='-')
    plt.plot(x, y2, label='Dashed (--)', linestyle='--')
    plt.plot(x, y3, label='Dotted (:)', linestyle=':')
    plt.plot(x, y4, label='Dash-dot (-.)', linestyle='-.')
    
    plt.title('Complete Line Style Reference')
    plt.xlabel('X values')
    plt.ylabel('Y values')
    plt.legend()
    
    plt.savefig(out('line_styles.png'))
    plt.savefig(out('line_styles.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('line_styles.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    print("Line style examples completed!")
    print("Files created: line_styles.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

if __name__ == "__main__":
    main()
