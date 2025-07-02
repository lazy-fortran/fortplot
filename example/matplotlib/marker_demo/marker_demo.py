#!/usr/bin/env python3
"""Demonstrates different marker types and styles"""

import numpy as np
import matplotlib.pyplot as plt

def demo_scatter_plot():
    """Creates a scatter plot to demonstrate markers in practical use"""
    # Generate scatter data
    x = np.arange(1, 21) * 0.3
    y = np.sin(x) + 0.1 * (np.arange(1, 21) - 10) / 10.0  # Sine wave with trend
    
    fig, ax = plt.subplots(figsize=(7.5, 5.625))
    ax.set_title("Scatter Plot with Antialiased Markers")
    ax.set_xlabel("X Values")
    ax.set_ylabel("Y Values")
    
    # Scatter plot with markers (pyplot-fortran style)
    ax.plot(x, y, 'o', label='Data Points')
    
    # Add trend line for context
    ax.plot(x, np.sin(x), '-', label='Sin(x) Reference')
    
    ax.legend()
    plt.savefig('example/matplotlib/marker_demo/scatter_plot.png', dpi=100)
    plt.savefig('example/matplotlib/marker_demo/scatter_plot.pdf')
    plt.close()

def demo_all_marker_types():
    """Demonstrates all available marker types with realistic data"""
    # Generate realistic data for each marker type
    x = np.arange(1, 11) * 0.5
    y1 = np.sin(x) + 3.0
    y2 = np.cos(x) + 2.0
    y3 = np.sin(x * 2.0) + 1.0
    y4 = np.cos(x * 1.5)
    
    fig, ax = plt.subplots(figsize=(7.5, 5))
    ax.set_title("All Marker Types")
    ax.set_xlabel("X Values")
    ax.set_ylabel("Y Values")
    
    # Draw each marker type with data (pyplot-fortran style)
    ax.plot(x, y1, 'o', label='Circle')
    ax.plot(x, y2, 's', label='Square')
    ax.plot(x, y3, 'D', label='Diamond')
    ax.plot(x, y4, 'x', label='Cross')
    
    ax.legend()
    plt.savefig('example/matplotlib/marker_demo/all_marker_types.png', dpi=100)
    plt.savefig('example/matplotlib/marker_demo/all_marker_types.pdf')
    plt.close()

def demo_marker_colors():
    """Demonstrates different colored markers with realistic data"""
    # Generate realistic data for color demonstration
    x = np.arange(1, 9) * 0.6
    y1 = np.exp(-x * 0.3) * np.cos(x) + 2.5
    y2 = np.exp(-x * 0.2) * np.sin(x) + 1.5  
    y3 = np.exp(-x * 0.4) * np.sin(x * 1.5) + 0.5
    
    fig, ax = plt.subplots(figsize=(6.25, 5))
    ax.set_title("Marker Colors and Styles")
    ax.set_xlabel("X Position")
    ax.set_ylabel("Y Position")
    
    # Different marker types with automatic color cycling (pyplot-fortran style)
    ax.plot(x, y1, 'o', label='Blue circles')
    ax.plot(x, y2, 's', label='Green squares')
    ax.plot(x, y3, 'D', label='Orange diamonds')
    
    ax.legend()
    plt.savefig('example/matplotlib/marker_demo/marker_colors.png', dpi=100)
    plt.savefig('example/matplotlib/marker_demo/marker_colors.pdf')
    plt.close()

if __name__ == "__main__":
    demo_scatter_plot()
    demo_all_marker_types()
    demo_marker_colors()
    
    print("=== Marker Examples ===")
    print("Created: scatter_plot.png/pdf")
    print("Created: all_marker_types.png/pdf")
    print("Created: marker_colors.png/pdf")
    print("All marker examples completed!")