#!/usr/bin/env python3
"""
Scale examples - Dual mode: fortplot or matplotlib
Equivalent to scale_examples.f90 for visual comparison
"""

import sys
import numpy as np

# Dual-mode import: --matplotlib uses matplotlib, default uses fortplot
if "--matplotlib" in sys.argv:
    import matplotlib.pyplot as plt
    backend = "matplotlib"
else:
    import fortplot.fortplot as plt
    backend = "fortplot"

def log_scale_demo():
    """Log scale demonstration - equivalent to Fortran version"""
    print(f"=== Log Scale Examples ({backend}) ===")
    
    # Generate exponential data for log scale (same as Fortran)
    x_exp = np.arange(1, 51)
    y_exp = np.exp(x_exp * 0.2)
    
    plt.figure(figsize=(6.4, 4.8))
    plt.plot(x_exp, y_exp, linestyle="-")
    plt.yscale('log')
    plt.title('Log Scale Example')
    plt.xlabel('x')
    plt.ylabel('exp(0.2x)')
    plt.savefig('log_scale.png')
    plt.savefig('log_scale.pdf')
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig('log_scale.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: log_scale.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

def symlog_scale_demo():
    """Symlog scale demonstration - equivalent to Fortran version"""
    print(f"=== Symlog Scale Examples ({backend}) ===")
    
    # Generate data that goes through zero for symlog (same as Fortran)
    x_exp = np.arange(1, 51)
    y_symlog = x_exp**3 - 50.0 * x_exp
    
    plt.figure(figsize=(6.4, 4.8))
    plt.plot(x_exp, y_symlog, linestyle="-")
    plt.yscale('symlog')  # Simplified for compatibility
    plt.title('Symlog Scale Example')
    plt.xlabel('x')
    plt.ylabel('x^3 - 50x')
    plt.savefig('symlog_scale.png')
    plt.savefig('symlog_scale.pdf')
    
    # Save TXT for fortplot only
    if backend == "fortplot":
        plt.savefig('symlog_scale.txt')
    
    if backend == "matplotlib":
        plt.close()
    
    print("Created: symlog_scale.png/pdf" + ("" if backend == "matplotlib" else "/txt"))

if __name__ == "__main__":
    log_scale_demo()
    symlog_scale_demo()