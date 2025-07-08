#!/usr/bin/env python3
"""
Demonstrates 2D scatter plots with matplotlib
Equivalent to fortplotlib's scatter_demo.f90

This shows the matplotlib approach for comparison with fortplotlib's cleaner API.
"""

import numpy as np
import matplotlib.pyplot as plt

# Basic 2D scatter plot
n = 50
x = np.linspace(0, 10, n)
y = np.sin(x) + 0.1 * np.random.randn(n)

plt.figure(figsize=(6.4, 4.8))
plt.scatter(x, y, label='Data points')
plt.xlabel('X')
plt.ylabel('Y')
plt.title('Basic Scatter Plot')
plt.legend()
plt.grid(True, alpha=0.3)
plt.savefig('output/example/python/scatter_demo/scatter_basic.png', dpi=100)
plt.close()

# Multiple scatter plots with different colors and sizes
plt.figure(figsize=(6.4, 4.8))

# Dataset 1: Linear trend
x1 = np.linspace(0, 10, 30)
y1 = 0.5 * x1 + np.random.randn(30) * 0.5

# Dataset 2: Quadratic trend
x2 = np.linspace(0, 10, 40)
y2 = 0.1 * x2**2 - x2 + 2 + np.random.randn(40) * 0.5

# Dataset 3: Exponential decay
x3 = np.linspace(0, 10, 35)
y3 = 5 * np.exp(-0.3 * x3) + np.random.randn(35) * 0.3

plt.scatter(x1, y1, c='blue', s=50, alpha=0.6, label='Linear')
plt.scatter(x2, y2, c='red', s=80, alpha=0.6, label='Quadratic')
plt.scatter(x3, y3, c='green', s=60, alpha=0.6, label='Exponential')

plt.xlabel('X')
plt.ylabel('Y')
plt.title('Multiple Scatter Plots')
plt.legend()
plt.grid(True, alpha=0.3)
plt.savefig('output/example/python/scatter_demo/scatter_multi.png', dpi=100)
plt.close()

# Gaussian distribution scatter
n_points = 200
x_gauss = np.random.randn(n_points)
y_gauss = np.random.randn(n_points)

# Color by distance from origin
colors = np.sqrt(x_gauss**2 + y_gauss**2)

plt.figure(figsize=(6.4, 4.8))
scatter = plt.scatter(x_gauss, y_gauss, c=colors, cmap='viridis', 
                     s=50, alpha=0.6, edgecolors='black', linewidth=0.5)
plt.colorbar(scatter, label='Distance from origin')
plt.xlabel('X')
plt.ylabel('Y')
plt.title('Gaussian Distribution')
plt.grid(True, alpha=0.3)
plt.axis('equal')
plt.savefig('output/example/python/scatter_demo/scatter_gaussian.png', dpi=100)
plt.close()

print("2D scatter plot demos complete!")
print("Generated files:")
print("  - scatter_basic.png    (basic 2D scatter)")
print("  - scatter_multi.png    (multiple datasets)")
print("  - scatter_gaussian.png (Gaussian distribution)")