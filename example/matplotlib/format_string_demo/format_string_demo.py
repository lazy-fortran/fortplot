#!/usr/bin/env python3
"""Demonstrate matplotlib-style format strings"""

import numpy as np
import matplotlib.pyplot as plt

def main():
    n = 50
    
    # Generate sample data
    x = np.arange(n) * 0.2
    y1 = np.sin(x)
    y2 = np.cos(x)
    y3 = np.sin(x * 0.5) * 0.8
    y4 = np.cos(x * 0.5) * 0.6
    
    fig, ax = plt.subplots(figsize=(10, 7.5))
    ax.set_title('Matplotlib-style Format Strings Demo')
    ax.set_xlabel('X values')
    ax.set_ylabel('Y values')
    
    # Different format strings using linestyle parameter (pyplot-fortran style)
    ax.plot(x, y1, '-', label='sin(x) - solid line')
    ax.plot(x, y2, '--', label='cos(x) - dashed line')
    ax.plot(x, y3, 'o', label='sin(x/2) - circles only')
    ax.plot(x, y4, 'x-', label='cos(x/2) - x markers with line')
    
    ax.legend()
    plt.savefig('example/matplotlib/format_string_demo/format_string_demo.png', dpi=100)
    plt.close()
    
    print('Format string demo saved to format_string_demo.png')

if __name__ == "__main__":
    main()