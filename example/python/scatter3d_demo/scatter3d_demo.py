#!/usr/bin/env python3
"""
Demonstrates 3D scatter plots with matplotlib
Equivalent to fortplotlib's scatter3d_demo.f90

This shows the matplotlib approach for comparison with fortplotlib's cleaner API.
"""

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Generate the same data as fortplotlib example
n = 100
theta = 2.0 * np.pi * np.arange(n) / (n - 1)
phi = np.pi * (np.arange(n) / (n - 1) - 0.5)

# Create distorted sphere points
x = np.cos(theta) * np.cos(phi) * (1.0 + 0.3 * np.sin(5 * theta))
y = np.sin(theta) * np.cos(phi) * (1.0 + 0.3 * np.sin(5 * theta))
z = np.sin(phi) + 0.1 * np.cos(10 * theta)

# Basic 3D scatter plot
fig = plt.figure(figsize=(6.4, 4.8))
ax = fig.add_subplot(111, projection='3d')
ax.scatter(x, y, z, c=z, cmap='viridis', label='3D Scatter')
ax.set_xlabel('X Label')
ax.set_ylabel('Y Label')
ax.set_zlabel('Z Label')
ax.set_title('3D Scatter Plot')
ax.legend()
plt.savefig('output/example/python/scatter3d_demo/scatter3d_demo.png', dpi=100)
plt.close()

# Multiple scatter plots
fig = plt.figure(figsize=(6.4, 4.8))
ax = fig.add_subplot(111, projection='3d')

# Generate three different datasets
n_sets = 3
for i in range(n_sets):
    offset = i * 0.5
    x_i = x + offset
    y_i = y
    z_i = z + offset * 0.5
    ax.scatter(x_i, y_i, z_i, label=f'Dataset {i+1}', alpha=0.6)

ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
ax.set_title('Multiple 3D Scatter Plots')
ax.legend()
plt.savefig('output/example/python/scatter3d_demo/scatter3d_multi.png', dpi=100)
plt.close()

# Parametric curve with scatter points
t = np.linspace(0, 4 * np.pi, 100)
x_curve = np.sin(t)
y_curve = np.cos(t)
z_curve = t / (4 * np.pi)

fig = plt.figure(figsize=(6.4, 4.8))
ax = fig.add_subplot(111, projection='3d')

# Plot the curve
ax.plot(x_curve, y_curve, z_curve, 'b-', label='Parametric curve')

# Add scatter points at intervals
n_points = 10
indices = np.linspace(0, len(t)-1, n_points, dtype=int)
ax.scatter(x_curve[indices], y_curve[indices], z_curve[indices], 
           c='green', s=100, label='Sample points')

ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
ax.set_title('Parametric Curve with Points')
ax.legend()
plt.savefig('output/example/python/scatter3d_demo/scatter3d_curve.png', dpi=100)
plt.close()

print("3D scatter plot demos complete!")
print("Generated files:")
print("  - scatter3d_demo.png   (basic 3D scatter)")
print("  - scatter3d_multi.png  (multiple scatter plots)")
print("  - scatter3d_curve.png  (parametric curve with points)")