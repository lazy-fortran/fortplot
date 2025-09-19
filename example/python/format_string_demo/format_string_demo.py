#!/usr/bin/env python3
"""Demonstrate matplotlib-style format strings - Dual mode: fortplot or matplotlib"""

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
    print(f"=== Format String Demo ({backend}) ===")
    n = 50
    
    # Generate sample data
    x = np.arange(n) * 0.2
    y1 = np.sin(x)
    y2 = np.cos(x)
    y3 = np.sin(x * 0.5) * 0.8
    y4 = np.cos(x * 0.5) * 0.6
    
    plt.figure(figsize=(10, 7.5))
    plt.title('Matplotlib-style Format Strings Demo')
    plt.xlabel('X values')
    plt.ylabel('Y values')
    
    # Different format strings using linestyle parameter (pyplot-fortran style)
    plt.plot(x, y1, linestyle='-', label='sin(x) - solid line')
    plt.plot(x, y2, linestyle='--', label='cos(x) - dashed line')
    plt.plot(x, y3, linestyle=':', label='sin(x/2) - dotted')
    plt.plot(x, y4, linestyle='-.', label='cos(x/2) - dash-dot')
    
    plt.legend()
    plt.savefig(out('format_string_demo.png'))
    plt.savefig(out('format_string_demo.pdf'))
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig(out('format_string_demo.txt'))
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: format_string_demo.png/pdf" + ("" if backend == "matplotlib" else "/txt"))
    
    print('Format string demo saved to format_string_demo.png/pdf')

if __name__ == "__main__":
    main()
