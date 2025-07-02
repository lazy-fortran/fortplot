#!/usr/bin/env python3
"""Demonstrates colored contour plots with different colormaps"""

import numpy as np
import matplotlib.pyplot as plt

def default_gaussian_example():
    """Default colorblind-safe Gaussian example"""
    print("=== Default Colorblind-Safe Gaussian Example ===")
    
    # Generate grid
    x_grid = np.linspace(-3.0, 3.0, 30)
    y_grid = np.linspace(-3.0, 3.0, 30)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # 2D Gaussian
    Z = np.exp(-(X**2 + Y**2))
    
    fig, ax = plt.subplots(figsize=(8, 6))
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_title("2D Gaussian - Default Colorblind-Safe Colormap")
    contourf = ax.contourf(X, Y, Z, cmap='viridis')  # Uses colorblind-safe viridis colormap
    plt.colorbar(contourf)
    
    plt.savefig('example/matplotlib/colored_contours/gaussian_default.png', dpi=100)
    plt.savefig('example/matplotlib/colored_contours/gaussian_default.pdf')
    plt.close()
    
    print("Created: gaussian_default.png/pdf")

def plasma_saddle_example():
    """Plasma saddle function example"""
    print("=== Plasma Saddle Function Example ===")
    
    # Generate grid
    x_grid = np.linspace(-2.5, 2.5, 25)
    y_grid = np.linspace(-2.5, 2.5, 25)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # Saddle function: x^2 - y^2
    Z = X**2 - Y**2
    
    # Custom contour levels
    custom_levels = [-6.0, -4.0, -2.0, -1.0, 1.0, 2.0, 4.0, 6.0]
    
    fig, ax = plt.subplots(figsize=(8, 6))
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_title("Saddle Function - Plasma Colormap")
    contourf = ax.contourf(X, Y, Z, levels=custom_levels, cmap="plasma")
    plt.colorbar(contourf)
    
    plt.savefig('example/matplotlib/colored_contours/saddle_plasma.png', dpi=100)
    plt.savefig('example/matplotlib/colored_contours/saddle_plasma.pdf')
    plt.close()
    
    print("Created: saddle_plasma.png/pdf")

def mixed_colormap_comparison():
    """Colormap comparison"""
    print("=== Colormap Comparison ===")
    
    # Generate grid
    x_grid = np.linspace(-2.0, 2.0, 20)
    y_grid = np.linspace(-2.0, 2.0, 20)
    X, Y = np.meshgrid(x_grid, y_grid)
    
    # Ripple function
    Z = np.sin(np.sqrt(X**2 + Y**2) * 3.0) * np.exp(-0.3 * np.sqrt(X**2 + Y**2))
    
    # Inferno colormap
    fig1, ax1 = plt.subplots(figsize=(8, 6))
    ax1.set_xlabel("x")
    ax1.set_ylabel("y")
    ax1.set_title("Ripple Function - Inferno Colormap")
    contourf1 = ax1.contourf(X, Y, Z, cmap="inferno")
    plt.colorbar(contourf1)
    plt.savefig('example/matplotlib/colored_contours/ripple_inferno.png', dpi=100)
    plt.savefig('example/matplotlib/colored_contours/ripple_inferno.pdf')
    plt.close()
    
    # Coolwarm colormap
    fig2, ax2 = plt.subplots(figsize=(8, 6))
    ax2.set_xlabel("x")
    ax2.set_ylabel("y")
    ax2.set_title("Ripple Function - Coolwarm Colormap")
    contourf2 = ax2.contourf(X, Y, Z, cmap="coolwarm")
    plt.colorbar(contourf2)
    plt.savefig('example/matplotlib/colored_contours/ripple_coolwarm.png', dpi=100)
    plt.savefig('example/matplotlib/colored_contours/ripple_coolwarm.pdf')
    plt.close()
    
    # Jet colormap
    fig3, ax3 = plt.subplots(figsize=(8, 6))
    ax3.set_xlabel("x")
    ax3.set_ylabel("y")
    ax3.set_title("Ripple Function - Jet Colormap")
    contourf3 = ax3.contourf(X, Y, Z, cmap="jet")
    plt.colorbar(contourf3)
    plt.savefig('example/matplotlib/colored_contours/ripple_jet.png', dpi=100)
    plt.savefig('example/matplotlib/colored_contours/ripple_jet.pdf')
    plt.close()
    
    print("Created: ripple_inferno.png/pdf, ripple_coolwarm.png/pdf, ripple_jet.png/pdf")
    print("Colormap comparison complete!")

if __name__ == "__main__":
    default_gaussian_example()
    plasma_saddle_example()
    mixed_colormap_comparison()