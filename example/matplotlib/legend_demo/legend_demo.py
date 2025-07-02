#!/usr/bin/env python3
"""Demonstration of legend functionality following SOLID principles
Shows legend positioning, labeling, and rendering across all backends"""

import numpy as np
import matplotlib.pyplot as plt

def basic_legend_example():
    """Basic legend usage with labeled plots"""
    print("=== Basic Legend Example ===")
    
    # Generate data
    x = np.arange(50) * 0.2
    y1 = np.sin(x)
    y2 = np.cos(x)
    
    fig, ax = plt.subplots(figsize=(8, 6))
    ax.set_title("Basic Legend Demo")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    
    # Add labeled plots
    ax.plot(x, y1, label="sin(x)")
    ax.plot(x, y2, label="cos(x)")
    
    # Add legend with default position (upper right)
    ax.legend()
    
    plt.savefig('example/matplotlib/legend_demo/basic_legend.png', dpi=100)
    plt.savefig('example/matplotlib/legend_demo/basic_legend.pdf')
    plt.close()
    
    print("Created: basic_legend.png/pdf")

def positioned_legend_example():
    """Demonstrate different legend positions"""
    print("=== Legend Positioning Examples ===")
    
    # Generate test data
    x = np.arange(1, 21, dtype=float)
    y1 = np.sqrt(x)
    y2 = np.log(x)
    
    # Upper left position
    fig1, ax1 = plt.subplots(figsize=(8, 6))
    ax1.set_title("Legend: Upper Left")
    ax1.plot(x, y1, label="√x")
    ax1.plot(x, y2, label="ln(x)")
    ax1.legend(loc="upper left")
    plt.savefig('example/matplotlib/legend_demo/legend_upper_left.png', dpi=100)
    plt.close()
    
    # Upper right position (default)
    fig2, ax2 = plt.subplots(figsize=(8, 6))
    ax2.set_title("Legend: Upper Right")
    ax2.plot(x, y1, label="√x")
    ax2.plot(x, y2, label="ln(x)")
    ax2.legend(loc="upper right")
    plt.savefig('example/matplotlib/legend_demo/legend_upper_right.png', dpi=100)
    plt.close()
    
    # Lower left position
    fig3, ax3 = plt.subplots(figsize=(8, 6))
    ax3.set_title("Legend: Lower Left")
    ax3.plot(x, y1, label="√x")
    ax3.plot(x, y2, label="ln(x)")
    ax3.legend(loc="lower left")
    plt.savefig('example/matplotlib/legend_demo/legend_lower_left.png', dpi=100)
    plt.close()
    
    # Lower right position
    fig4, ax4 = plt.subplots(figsize=(8, 6))
    ax4.set_title("Legend: Lower Right")
    ax4.plot(x, y1, label="√x")
    ax4.plot(x, y2, label="ln(x)")
    ax4.legend(loc="lower right")
    plt.savefig('example/matplotlib/legend_demo/legend_lower_right.png', dpi=100)
    plt.close()
    
    print("Created: legend_upper_left/right.png, legend_lower_left/right.png")

def multi_function_legend_example():
    """Complex legend with multiple mathematical functions"""
    print("=== Multi-Function Legend Example ===")
    
    # Generate mathematical functions
    x = np.arange(100) * 0.1
    y1 = np.exp(-x/2.0) * np.cos(x)
    y2 = x * np.exp(-x/3.0)
    # Handle division by zero for sin(x)/x
    y3 = np.where(x == 0, 1.0, np.sin(x) / x)
    y4 = x**2 * np.exp(-x)
    
    fig, ax = plt.subplots(figsize=(10, 7.5))
    ax.set_title("Mathematical Functions with Legend")
    ax.set_xlabel("x")
    ax.set_ylabel("f(x)")
    
    # Add multiple labeled functions
    ax.plot(x, y1, label="e^(-x/2)cos(x)")
    ax.plot(x, y2, label="xe^(-x/3)")
    ax.plot(x, y3, label="sin(x)/x")
    ax.plot(x, y4, label="x²e^(-x)")
    
    # Add legend
    ax.legend()
    
    plt.savefig('example/matplotlib/legend_demo/multi_function_legend.png', dpi=100)
    plt.savefig('example/matplotlib/legend_demo/multi_function_legend.pdf')
    plt.close()
    
    print("Created: multi_function_legend.png/pdf")

if __name__ == "__main__":
    basic_legend_example()
    positioned_legend_example()
    multi_function_legend_example()