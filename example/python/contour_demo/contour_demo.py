#!/usr/bin/env python3
"""
Contour plotting examples - Python reference using matplotlib
Equivalent to contour_demo.f90 for visual comparison
"""

import numpy as np
import matplotlib.pyplot as plt

def gaussian_contours():
    """Gaussian contour demonstration - equivalent to Fortran version"""
    print("=== Contour Examples (Python Reference) ===")
    
    # Generate contour grid (same as Fortran)
    x_grid = np.linspace(-3.0, 3.0, 30)
    y_grid = np.linspace(-3.0, 3.0, 30)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # Gaussian contour
    Z = np.exp(-(X**2 + Y**2))
    
    plt.figure(figsize=(6.4, 4.8))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("2D Gaussian Function")
    
    # Create contour plot
    contour = plt.contour(X, Y, Z, levels=10)
    plt.clabel(contour, inline=True, fontsize=8)
    
    # Add colorbar
    plt.colorbar(contour, label="exp(-(x²+y²))")
    plt.grid(True, alpha=0.3)
    
    plt.savefig('example/matplotlib/contour_demo/contour_gaussian_ref.png', dpi=150, bbox_inches='tight')
    plt.savefig('example/matplotlib/contour_demo/contour_gaussian_ref.pdf', bbox_inches='tight')
    plt.close()
    
    print("Created: contour_gaussian_ref.png/pdf")

def mixed_contour_line_plot():
    """Mixed contour and line plot - equivalent to Fortran version"""
    # Generate grid (same as Fortran)
    x_grid = np.linspace(-3.0, 3.0, 30)
    y_grid = np.linspace(-3.0, 3.0, 30)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # Saddle function with custom levels
    Z = X**2 - Y**2
    custom_levels = [-4.0, -2.0, 0.0, 2.0, 4.0]
    
    plt.figure(figsize=(6.4, 4.8))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Mixed Plot: Contour + Line")
    
    # Add contour with custom levels
    contour = plt.contour(X, Y, Z, levels=custom_levels, colors='blue', alpha=0.7)
    plt.clabel(contour, inline=True, fontsize=8)
    
    # Add line plot (cross-section at y=0)
    plt.plot(x_grid, np.exp(-x_grid**2), 'r-', linewidth=2, label="Cross-section at y=0")
    
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    plt.savefig('example/matplotlib/contour_demo/mixed_plot_ref.png', dpi=150, bbox_inches='tight')
    plt.savefig('example/matplotlib/contour_demo/mixed_plot_ref.pdf', bbox_inches='tight')
    plt.close()
    
    print("Created: mixed_plot_ref.png/pdf")

if __name__ == "__main__":
    gaussian_contours()
    mixed_contour_line_plot()