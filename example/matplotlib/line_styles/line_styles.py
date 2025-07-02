#!/usr/bin/env python3
"""
Line styles demonstration - Python reference using matplotlib
Equivalent to line_styles.f90 for visual comparison
"""

import numpy as np
import matplotlib.pyplot as plt

def main():
    print("=== Line Style Examples (Python Reference) ===")
    
    # Generate test data with clear separation for visibility (same as Fortran)
    x = np.arange(50) * 0.2
    y1 = np.sin(x) + 2.0
    y2 = np.cos(x) + 1.0
    y3 = np.sin(x * 2.0)
    y4 = np.cos(x * 3.0) - 1.0
    y5 = np.sin(x * 0.5) - 2.0
    
    # Comprehensive line style demonstration
    plt.figure(figsize=(8, 6))
    plt.plot(x, y1, label='Solid (-)', linestyle='-', linewidth=2)
    plt.plot(x, y2, label='Dashed (--)', linestyle='--', linewidth=2)
    plt.plot(x, y3, label='Dotted (:)', linestyle=':', linewidth=2)
    plt.plot(x, y4, label='Dash-dot (-.)', linestyle='-.', linewidth=2)
    plt.plot(x, y5, label='None (markers only)', linestyle='None', marker='o', markersize=3)
    
    plt.title('Complete Line Style Reference')
    plt.xlabel('X values')
    plt.ylabel('Y values')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    plt.savefig('example/line_styles/line_styles_ref.png', dpi=150, bbox_inches='tight')
    plt.savefig('example/line_styles/line_styles_ref.pdf', bbox_inches='tight')
    plt.close()
    
    print("Line style examples completed!")
    print("Files created: line_styles_ref.png, line_styles_ref.pdf")
    print("Note: 'None' linestyle shows markers only")
    print()

if __name__ == "__main__":
    main()