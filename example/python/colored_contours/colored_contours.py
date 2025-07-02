#!/usr/bin/env python3
"""Demonstrates colored contour plots with different colormaps - Dual mode: fortplotlib or matplotlib"""

import sys
import numpy as np

# Dual-mode import: --matplotlib uses matplotlib, default uses fortplotlib
if "--matplotlib" in sys.argv:
    import matplotlib.pyplot as plt
    backend = "matplotlib"
else:
    import fortplotlib.fortplot as plt
    backend = "fortplotlib"

def default_gaussian_example():
    """Default colorblind-safe Gaussian example"""
    print(f"=== Default Colorblind-Safe Gaussian Example ({backend}) ===")
    
    # Generate grid
    x_grid = np.linspace(-3.0, 3.0, 30)
    y_grid = np.linspace(-3.0, 3.0, 30)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # 2D Gaussian
    Z = np.exp(-(X**2 + Y**2))
    
    plt.figure(figsize=(8, 6))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("2D Gaussian - Default Colorblind-Safe Colormap")
    
    # Use contourf for filled contours
    plt.contourf(X, Y, Z)
    
    plt.savefig('gaussian_default.png')
    plt.savefig('gaussian_default.pdf')
    
    # Save TXT for fortplotlib only
    if backend == "fortplotlib":
        plt.savefig('gaussian_default.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: gaussian_default.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

def plasma_saddle_example():
    """Plasma saddle function example"""
    print(f"=== Plasma Saddle Function Example ({backend}) ===")
    
    # Generate grid
    x_grid = np.linspace(-2.5, 2.5, 25)
    y_grid = np.linspace(-2.5, 2.5, 25)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # Saddle function: x^2 - y^2
    Z = X**2 - Y**2
    
    # Custom contour levels
    custom_levels = [-6.0, -4.0, -2.0, -1.0, 1.0, 2.0, 4.0, 6.0]
    
    plt.figure(figsize=(8, 6))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Saddle Function - Plasma Colormap")
    
    # Custom contour levels
    plt.contourf(X, Y, Z, custom_levels)
    
    plt.savefig('saddle_plasma.png')
    plt.savefig('saddle_plasma.pdf')
    
    # Save TXT for fortplotlib only
    if backend == "fortplotlib":
        plt.savefig('saddle_plasma.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: saddle_plasma.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

def mixed_colormap_comparison():
    """Colormap comparison"""
    print(f"=== Colormap Comparison ({backend}) ===")
    
    # Generate grid
    x_grid = np.linspace(-2.0, 2.0, 20)
    y_grid = np.linspace(-2.0, 2.0, 20)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # Ripple function
    Z = np.sin(np.sqrt(X**2 + Y**2) * 3.0) * np.exp(-0.3 * np.sqrt(X**2 + Y**2))
    
    # Ripple inferno
    plt.figure(figsize=(8, 6))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Ripple Function - Inferno Colormap")
    plt.contourf(X, Y, Z)
    plt.savefig('ripple_inferno.png')
    plt.savefig('ripple_inferno.pdf')
    
    # Save TXT for fortplotlib only
    if backend == "fortplotlib":
        plt.savefig('ripple_inferno.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    # Ripple coolwarm
    plt.figure(figsize=(8, 6))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Ripple Function - Coolwarm Colormap")
    plt.contourf(X, Y, Z)
    plt.savefig('ripple_coolwarm.png')
    plt.savefig('ripple_coolwarm.pdf')
    
    # Save TXT for fortplotlib only
    if backend == "fortplotlib":
        plt.savefig('ripple_coolwarm.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    # Jet colormap
    plt.figure(figsize=(8, 6))
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Ripple Function - Jet Colormap")
    plt.contourf(X, Y, Z)
    plt.savefig('ripple_jet.png')
    plt.savefig('ripple_jet.pdf')
    
    # Save TXT for fortplotlib only
    if backend == "fortplotlib":
        plt.savefig('ripple_jet.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: ripple_inferno.png/pdf, ripple_coolwarm.png/pdf, ripple_jet.png/pdf" + ("" if backend == "matplotlib" else " + txt files"))
    print("Colormap comparison complete!")

if __name__ == "__main__":
    default_gaussian_example()
    plasma_saddle_example()
    mixed_colormap_comparison()