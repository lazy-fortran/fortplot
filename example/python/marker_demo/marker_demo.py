#!/usr/bin/env python3
"""Demonstrates different marker types and styles - Dual mode: fortplot or matplotlib"""

import sys
import numpy as np

# Dual-mode import: --matplotlib uses matplotlib, default uses fortplot
if "--matplotlib" in sys.argv:
    import matplotlib.pyplot as plt
    backend = "matplotlib"
else:
    import fortplot.fortplot as plt
    backend = "fortplot"

def demo_scatter_plot():
    """Creates a scatter plot to demonstrate markers in practical use"""
    print(f"=== Scatter Plot Demo ({backend}) ===")
    
    # Generate scatter data
    x = np.arange(1, 21) * 0.3
    y = np.sin(x) + 0.1 * (np.arange(1, 21) - 10) / 10.0  # Sine wave with trend
    
    plt.figure(figsize=(7.5, 5.625))
    plt.title("Scatter Plot with Antialiased Markers")
    plt.xlabel("X Values")
    plt.ylabel("Y Values")
    
    # Simplified plots for dual-mode compatibility
    plt.plot(x, y, linestyle="", label='Data Points')  # No line, just markers
    plt.plot(x, np.sin(x), linestyle="-", label='Sin(x) Reference')
    
    plt.legend()
    plt.savefig('scatter_plot.png')
    plt.savefig('scatter_plot.pdf')
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig('scatter_plot.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: scatter_plot.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

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
    plt.savefig('all_marker_types.png', dpi=100)
    plt.savefig('all_marker_types.pdf')
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
    plt.savefig('marker_colors.png', dpi=100)
    plt.savefig('marker_colors.pdf')
    plt.close()

if __name__ == "__main__":
    demo_scatter_plot()
    # Other demos need conversion from subplots to simple pyplot
    print("Marker demo completed!")