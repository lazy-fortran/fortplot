#!/usr/bin/env python3
"""Debug matplotlib streamplot to understand exact behavior"""

import numpy as np
import matplotlib.pyplot as plt

def main():
    # Exact same setup as our Fortran code
    nx, ny = 20, 20
    x = np.linspace(-2.0, 2.0, nx)
    y = np.linspace(-2.0, 2.0, ny)
    X, Y = np.meshgrid(x, y)
    
    # Circular flow field: u = -y, v = x
    U = -Y
    V = X
    
    # Create figure with matplotlib defaults
    fig, ax = plt.subplots(figsize=(10, 7.5))
    
    # Use exact matplotlib defaults - let's see what parameters it actually uses
    print("Matplotlib streamplot with defaults:")
    print(f"  density={1.0}")
    print(f"  maxlength={4.0}")
    print(f"  minlength={0.1}")
    print(f"  broken_streamlines={True}")
    
    # Plot with defaults
    stream = ax.streamplot(X, Y, U, V, density=1.0, maxlength=4.0, 
                          minlength=0.1, broken_streamlines=True)
    
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_title('Matplotlib Reference - Circular Flow (Default Parameters)')
    ax.set_aspect('equal')
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig('debug_matplotlib_reference.png', dpi=150, bbox_inches='tight')
    plt.savefig('debug_matplotlib_reference.pdf', bbox_inches='tight')
    
    # Let's also try with broken_streamlines=False to see the difference
    fig2, ax2 = plt.subplots(figsize=(10, 7.5))
    
    print("\nMatplotlib streamplot with broken_streamlines=False:")
    stream2 = ax2.streamplot(X, Y, U, V, density=1.0, maxlength=4.0, 
                           minlength=0.1, broken_streamlines=False)
    
    ax2.set_xlabel('X')
    ax2.set_ylabel('Y')
    ax2.set_title('Matplotlib Reference - broken_streamlines=False')
    ax2.set_aspect('equal')
    ax2.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig('debug_matplotlib_broken_false.png', dpi=150, bbox_inches='tight')
    plt.close()
    
    print("Matplotlib reference files generated!")

if __name__ == "__main__":
    main()