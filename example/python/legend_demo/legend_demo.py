#!/usr/bin/env python3
"""Demonstration of legend functionality following SOLID principles
Shows legend positioning, labeling, and rendering across all backends - Dual mode: fortplot or matplotlib"""

import sys
import numpy as np

# Dual-mode import: --matplotlib uses matplotlib, default uses fortplot
if "--matplotlib" in sys.argv:
    import matplotlib.pyplot as plt
    backend = "matplotlib"
else:
    import fortplot.fortplot as plt
    backend = "fortplot"

def basic_legend_example():
    """Basic legend usage with labeled plots"""
    print(f"=== Basic Legend Example ({backend}) ===")
    
    # Generate data
    x = np.arange(50) * 0.2
    y1 = np.sin(x)
    y2 = np.cos(x)
    
    plt.figure(figsize=(8, 6))
    plt.title("Basic Legend Demo")
    plt.xlabel("x")
    plt.ylabel("y")
    
    # Add labeled plots
    plt.plot(x, y1, linestyle="-", label="sin(x)")
    plt.plot(x, y2, linestyle="--", label="cos(x)")
    
    # Add legend with default position (upper right)
    plt.legend()
    
    plt.savefig('basic_legend.png')
    plt.savefig('basic_legend.pdf')
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig('basic_legend.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: basic_legend.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

def positioned_legend_example():
    """Demonstrate different legend positions"""
    print(f"=== Legend Positioning Examples ({backend}) ===")
    
    # Generate test data
    x = np.arange(1, 21, dtype=float)
    y1 = np.sqrt(x)
    y2 = np.log(x)
    
    # Single legend position demo (upper right - default)
    plt.figure(figsize=(8, 6))
    plt.title("Legend Positioning Demo")
    plt.xlabel("x")
    plt.ylabel("y")
    plt.plot(x, y1, linestyle="-", label="sqrt(x)")
    plt.plot(x, y2, linestyle="--", label="log(x)")
    plt.legend()
    
    plt.savefig('legend_positioning.png')
    plt.savefig('legend_positioning.pdf')
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig('legend_positioning.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: legend_positioning.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

def multi_function_legend_example():
    """Complex legend with multiple mathematical functions"""
    print(f"=== Multi-Function Legend Example ({backend}) ===")
    
    # Generate mathematical functions
    x = np.arange(100) * 0.1
    y1 = np.exp(-x/2.0) * np.cos(x)
    y2 = x * np.exp(-x/3.0)
    # Handle division by zero for sin(x)/x
    y3 = np.where(x == 0, 1.0, np.sin(x) / x)
    y4 = x**2 * np.exp(-x)
    
    plt.figure(figsize=(10, 7.5))
    plt.title("Mathematical Functions with Legend")
    plt.xlabel("x")
    plt.ylabel("f(x)")
    
    # Add multiple labeled functions
    plt.plot(x, y1, linestyle="-", label="e^(-x/2)cos(x)")
    plt.plot(x, y2, linestyle="--", label="xe^(-x/3)")
    plt.plot(x, y3, linestyle=":", label="sin(x)/x")
    plt.plot(x, y4, linestyle="-.", label="x^2*exp(-x)")
    
    # Add legend
    plt.legend()
    
    plt.savefig('multi_function_legend.png')
    plt.savefig('multi_function_legend.pdf')
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig('multi_function_legend.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: multi_function_legend.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

if __name__ == "__main__":
    basic_legend_example()
    positioned_legend_example()
    multi_function_legend_example()