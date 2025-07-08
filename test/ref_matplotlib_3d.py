#!/usr/bin/env python3
"""Generate matplotlib 3D reference plots for comparison"""

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Create figure
fig = plt.figure(figsize=(6.4, 4.8))
ax = fig.add_subplot(111, projection='3d')

# Generate the same data as fortplotlib scatter3d_demo
n = 100
theta = 2.0 * np.pi * np.arange(n) / (n - 1)
phi = np.pi * (np.arange(n) / (n - 1) - 0.5)

x = np.cos(theta) * np.cos(phi) * (1.0 + 0.3 * np.sin(5 * theta))
y = np.sin(theta) * np.cos(phi) * (1.0 + 0.3 * np.sin(5 * theta))
z = np.sin(phi) + 0.1 * np.cos(10 * theta)

# Create scatter plot
ax.scatter(x, y, z, c=z, cmap='viridis', label='3D Scatter')

# Set labels
ax.set_xlabel('X Label')
ax.set_ylabel('Y Label')
ax.set_zlabel('Z Label')

# Save
plt.savefig('output/test/ref_matplotlib_3d/scatter3d_ref.png', dpi=100)
plt.close()

print("Generated matplotlib reference: output/test/ref_matplotlib_3d/scatter3d_ref.png")