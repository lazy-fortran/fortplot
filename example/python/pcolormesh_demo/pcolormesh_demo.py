#!/usr/bin/env python3
"""Demonstrates pcolormesh functionality with different colormaps - Dual mode: fortplotlib or matplotlib"""

import sys
import numpy as np

# Dual-mode import: --matplotlib uses matplotlib, default uses fortplotlib
if "--matplotlib" in sys.argv:
    import matplotlib.pyplot as plt
    backend = "matplotlib"
else:
    import fortplotlib.fortplot as plt
    backend = "fortplotlib"

def demo_basic_gradient():
    """Basic pcolormesh with simple gradient"""
    print(f"=== Basic Pcolormesh - Linear Gradient ({backend}) ===")
    
    # Create coordinate arrays for regular grid
    x = np.array([i * 0.4 for i in range(6)])
    y = np.array([i * 0.3 for i in range(5)])
    
    # Create meshgrid for pcolormesh
    X, Y = np.meshgrid(x, y, indexing='ij')
    
    # Create test data - simple gradient
    C = np.zeros((5, 4))  # Note: C dimensions are (len(y)-1, len(x)-1)
    for i in range(4):
        for j in range(5):
            C[j, i] = i + j * 0.5
    
    plt.figure(figsize=(8, 6))
    plt.title('Basic Pcolormesh - Linear Gradient')
    plt.xlabel('X coordinate')
    plt.ylabel('Y coordinate')
    plt.pcolormesh(x, y, C, cmap='viridis')
    
    plt.savefig('pcolormesh_basic.png')
    plt.savefig('pcolormesh_basic.pdf')
    
    # Save TXT for fortplotlib only
    if backend == "fortplotlib":
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
    
    # Save TXT for fortplotlib only
    if backend == "fortplotlib":
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
    
    # Save TXT for fortplotlib only
    if backend == "fortplotlib":
        plt.savefig('pcolormesh_plasma.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: pcolormesh_plasma.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

if __name__ == "__main__":
    demo_basic_gradient()
    demo_sinusoidal_pattern()
    demo_different_colormaps()