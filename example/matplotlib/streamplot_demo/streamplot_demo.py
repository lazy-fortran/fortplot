#!/usr/bin/env python3
"""Streamplot demo - demonstrates streamline plotting"""

import numpy as np
import matplotlib.pyplot as plt

def main():
    nx, ny = 20, 20
    
    # Create grid
    x = np.linspace(-2.0, 2.0, nx)
    y = np.linspace(-2.0, 2.0, ny)
    X, Y = np.meshgrid(x, y)
    
    # Create circular flow field
    U = -Y
    V = X
    
    # Create figure and add streamplot
    fig, ax = plt.subplots(figsize=(10, 7.5))
    ax.streamplot(X, Y, U, V, density=1.0)
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_title('Streamline Plot Demo - Circular Flow')
    
    # Save figure
    plt.savefig('example/matplotlib/streamplot_demo/streamplot_demo.png', dpi=100)
    plt.close()
    
    print('Streamplot demo completed!')

if __name__ == "__main__":
    main()