#!/usr/bin/env python3
"""Demonstrate matplotlib-style format strings - Dual mode: fortplotlib or matplotlib"""

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
    plt.savefig('format_string_demo.png')
    plt.savefig('format_string_demo.pdf')
    
    # Save TXT for fortplotlib only
    if backend == "fortplotlib":
        plt.savefig('format_string_demo.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: format_string_demo.png/pdf" + ("" if backend == "matplotlib" else "/txt"))
    
    print('Format string demo saved to format_string_demo.png/pdf')

if __name__ == "__main__":
    main()