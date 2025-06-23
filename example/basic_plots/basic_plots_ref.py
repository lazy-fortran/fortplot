#!/usr/bin/env python3
"""
Basic plotting examples - Python reference using matplotlib
Equivalent to basic_plots.f90 for visual comparison
"""

import numpy as np
import matplotlib.pyplot as plt

def simple_plots():
    """Simple plot using matplotlib - equivalent to Fortran functional API"""
    print("=== Basic Plots (Python Reference) ===")
    
    # Generate simple sine data (same as Fortran)
    x = np.arange(50) * 0.2
    y = np.sin(x)
    
    # Simple plot
    plt.figure(figsize=(6.4, 4.8))  # Default matplotlib size
    plt.plot(x, y, label='sin(x)')
    plt.title('Simple Sine Wave')
    plt.xlabel('x')
    plt.ylabel('sin(x)')
    plt.legend()
    plt.savefig('example/basic_plots/simple_plot_ref.png')
    plt.close()
    
    print("Created: simple_plot_ref.png")

def multi_line_plot():
    """Multi-line plot using matplotlib - equivalent to Fortran OO interface"""
    # Generate data (same as Fortran)
    x = np.arange(100) / 5.0
    sx = np.sin(x)
    cx = np.cos(x)
    
    # Multi-line plot
    plt.figure(figsize=(6.4, 4.8))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Sine and Cosine Functions")
    plt.plot(x, sx, label="sin(x)")
    plt.plot(x, cx, label="cos(x)")
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.savefig('example/basic_plots/multi_line_ref.png')
    plt.savefig('example/basic_plots/multi_line_ref.pdf')
    plt.close()
    
    print("Created: multi_line_ref.png/pdf")

if __name__ == "__main__":
    simple_plots()
    multi_line_plot()