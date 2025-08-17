#!/usr/bin/env python3
"""Streamplot demo - demonstrates streamline plotting"""

import numpy as np
import sys

if "--matplotlib" in sys.argv:
    import matplotlib.pyplot as plt
    backend = "matplotlib"
else:
    import fortplot.fortplot as plt
    backend = "fortplot"

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
    plt.figure(figsize=(10, 7.5))
    plt.streamplot(X, Y, U, V, density=1.0)
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.title('Streamline Plot Demo - Circular Flow')
    plt.savefig('streamplot_demo.png')
    plt.savefig('streamplot_demo.pdf')

    # Export TXT for fortplot mode
    if backend == "fortplot":
        with open('streamplot_demo.txt', 'w') as f:
            f.write("Streamline Plot Demo - Circular Flow\n")
            f.write("=====================================\n\n")
            f.write("Grid size: {}x{}\n".format(nx, ny))
            f.write("X range: -2.0 to 2.0\n")
            f.write("Y range: -2.0 to 2.0\n")
            f.write("Flow field: Circular (U=-Y, V=X)\n")
            f.write("Density: 1.0\n")

    print('Streamplot demo completed!')

if __name__ == "__main__":
    main()
