#!/usr/bin/env python3
"""Demonstrates pcolormesh functionality with different colormaps - Dual mode: fortplot or matplotlib"""

import sys
import numpy as np

# Dual-mode import: --matplotlib uses matplotlib, default uses fortplot
if "--matplotlib" in sys.argv:
    import matplotlib.pyplot as plt
    backend = "matplotlib"
else:
    import fortplot.fortplot as plt
    backend = "fortplot"

def demo_basic_gradient():
    """Basic pcolormesh with simple gradient"""
    print(f"=== Basic Pcolormesh - Linear Gradient ({backend}) ===")
    
    # Create coordinate arrays for regular grid
    x = np.array([i * 0.4 for i in range(6)])
    y = np.array([i * 0.3 for i in range(5)])
    
    # Create test data - simple gradient
    # For 1D x (len=nx) and y (len=ny), matplotlib expects C.shape == (ny-1, nx-1)
    ny = len(y)
    nx = len(x)
    C = np.zeros((ny - 1, nx - 1))
    for j in range(ny - 1):
        for i in range(nx - 1):
            C[j, i] = i + j * 0.5
    
    plt.figure(figsize=(8, 6))
    plt.title('Basic Pcolormesh - Linear Gradient')
    plt.xlabel('X coordinate')
    plt.ylabel('Y coordinate')
    plt.pcolormesh(x, y, C, cmap='viridis')
    
    plt.savefig('pcolormesh_basic.png')
    plt.savefig('pcolormesh_basic.pdf')
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig('pcolormesh_basic.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: pcolormesh_basic.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

def demo_sinusoidal_pattern():
    """Pcolormesh with sinusoidal pattern"""
    print(f"=== Pcolormesh - Sinusoidal Pattern ({backend}) ===")
    
    # Create coordinate arrays
    x = np.array([i * 0.2 for i in range(9)])
    y = np.array([i * 0.15 for i in range(9)])
    
    # Create sinusoidal pattern
    C = np.zeros((8, 8))
    for i in range(8):
        for j in range(8):
            xi = (x[i] + x[i+1]) * 0.5  # Center of quad
            yj = (y[j] + y[j+1]) * 0.5  # Center of quad
            C[j, i] = np.sin(2.0 * np.pi * xi) * np.cos(3.0 * np.pi * yj)
    
    plt.figure(figsize=(8, 6))
    plt.title('Pcolormesh - Sinusoidal Pattern')
    plt.xlabel('X coordinate')
    plt.ylabel('Y coordinate')
    plt.pcolormesh(x, y, C, cmap='coolwarm')
    
    plt.savefig('pcolormesh_sinusoidal.png')
    plt.savefig('pcolormesh_sinusoidal.pdf')
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig('pcolormesh_sinusoidal.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: pcolormesh_sinusoidal.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

def demo_different_colormaps():
    """Demo different colormaps"""
    print(f"=== Pcolormesh - Radial Pattern (Plasma) ({backend}) ===")
    
    # Create coordinate arrays
    x = np.array([i * 0.3 for i in range(6)])
    y = np.array([i * 0.25 for i in range(6)])
    
    # Create radial pattern
    C = np.zeros((5, 5))
    for i in range(5):
        for j in range(5):
            r = np.sqrt((x[i] - 1.0)**2 + (y[j] - 0.6)**2)
            C[j, i] = np.exp(-r)
    
    plt.figure(figsize=(8, 6))
    plt.title('Pcolormesh - Radial Pattern (Plasma)')
    plt.xlabel('X coordinate')
    plt.ylabel('Y coordinate')
    plt.pcolormesh(x, y, C, cmap='plasma')
    
    plt.savefig('pcolormesh_plasma.png')
    plt.savefig('pcolormesh_plasma.pdf')
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig('pcolormesh_plasma.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: pcolormesh_plasma.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

if __name__ == "__main__":
    demo_basic_gradient()
    demo_sinusoidal_pattern()
    demo_different_colormaps()
