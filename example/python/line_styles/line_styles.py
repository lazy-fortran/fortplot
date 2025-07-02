#!/usr/bin/env python3
"""
Line styles demonstration - Dual mode: fortplotlib or matplotlib
Equivalent to line_styles.f90 for visual comparison
"""

import sys
import numpy as np

# Dual-mode import: --matplotlib uses matplotlib, default uses fortplotlib
if "--matplotlib" in sys.argv:
    import matplotlib.pyplot as plt
    backend = "matplotlib"
else:
    import fortplotlib.fortplot as plt
    backend = "fortplotlib"

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
    
    plt.savefig('line_styles.png')
    plt.savefig('line_styles.pdf')
    
    # Save TXT for fortplotlib only
    if backend == "fortplotlib":
        plt.savefig('line_styles.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Line style examples completed!")
    print("Files created: line_styles.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

if __name__ == "__main__":
    main()