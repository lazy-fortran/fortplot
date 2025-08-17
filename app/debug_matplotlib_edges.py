#!/usr/bin/env python3
"""Debug which edges matplotlib actually draws for 3D axes"""

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Create figure with 3D axes
fig = plt.figure(figsize=(10, 8))

# Test different viewing angles
views = [
    (-60, 30),  # Default
    (-120, 30), # Rotated 60 degrees
    (30, 30),   # Different angle
    (-60, 60),  # Higher elevation
]

for i, (azim, elev) in enumerate(views):
    ax = fig.add_subplot(2, 2, i+1, projection='3d')
    
    # Create simple data to show the box
    x = np.array([0, 1])
    y = np.array([0, 1])
    z = np.array([0, 1])
    
    # Plot a simple line to trigger 3D axes
    ax.plot([0.5], [0.5], [0.5], 'ro')
    
    # Set view
    ax.view_init(elev=elev, azim=azim)
    
    # Set limits
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1) 
    ax.set_zlim(0, 1)
    
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('Z')
    ax.set_title(f'azim={azim}, elev={elev}')

plt.tight_layout()
plt.savefig('output/example/fortran/scatter3d_demo/matplotlib_edges_analysis.png', dpi=150)
plt.close()

# Now create a detailed view of the default angle
fig = plt.figure(figsize=(8, 6))
ax = fig.add_subplot(111, projection='3d')

# Plot all 8 corners
corners = np.array([
    [0, 0, 0],  # 1
    [1, 0, 0],  # 2
    [1, 1, 0],  # 3
    [0, 1, 0],  # 4
    [0, 0, 1],  # 5
    [1, 0, 1],  # 6
    [1, 1, 1],  # 7
    [0, 1, 1],  # 8
])

# Plot corners with labels
for i, corner in enumerate(corners):
    ax.scatter(*corner, s=100)
    ax.text(corner[0], corner[1], corner[2], f'  {i+1}', fontsize=10)

ax.set_xlim(-0.2, 1.2)
ax.set_ylim(-0.2, 1.2)
ax.set_zlim(-0.2, 1.2)

ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
ax.set_title('Corner numbering and visible edges')

plt.savefig('output/example/fortran/scatter3d_demo/matplotlib_corners_labeled.png', dpi=150)
plt.close()

print("Analysis complete!")
print("\nFor default view (azim=-60, elev=30):")
print("Matplotlib draws these edges:")
print("1. Bottom front edge: corner 1 to 2 (X-axis direction)")
print("2. Bottom left edge: corner 1 to 4 (Y-axis direction)")  
print("3. Left front vertical: corner 1 to 5 (Z-axis direction)")
print("4. Top front edge: corner 5 to 6")
print("5. Top left edge: corner 5 to 8")
print("6. Front right vertical: corner 2 to 6")
print("7. Bottom back edge: corner 4 to 3")
print("8. Back left vertical: corner 4 to 8")
print("9. Back right vertical: corner 3 to 7")
print("\nTotal: 9 edges forming the 'open box' outline")