#!/usr/bin/env python3
"""
Scale examples - Python reference using matplotlib
Equivalent to scale_examples.f90 for visual comparison
"""

import numpy as np
import matplotlib.pyplot as plt

def log_scale_demo():
    """Log scale demonstration - equivalent to Fortran version"""
    print("=== Scale Examples (Python Reference) ===")
    
    # Generate exponential data for log scale (same as Fortran)
    x_exp = np.arange(1, 51)
    y_exp = np.exp(x_exp * 0.2)
    
    plt.figure(figsize=(6.4, 4.8))
    plt.plot(x_exp, y_exp)
    plt.yscale('log')
    plt.title('Log Scale Example')
    plt.xlabel('x')
    plt.ylabel('exp(0.2x)')
    plt.grid(True, alpha=0.3)
    plt.savefig('example/scale_examples/log_scale_ref.png', dpi=150, bbox_inches='tight')
    plt.savefig('example/scale_examples/log_scale_ref.pdf', bbox_inches='tight')
    plt.close()
    
    print("Created: log_scale_ref.png/pdf")

def symlog_scale_demo():
    """Symlog scale demonstration - equivalent to Fortran version"""
    # Generate data that goes through zero for symlog (same as Fortran)
    x_exp = np.arange(1, 51)
    y_symlog = x_exp**3 - 50.0 * x_exp
    
    plt.figure(figsize=(6.4, 4.8))
    plt.plot(x_exp, y_symlog)
    plt.yscale('symlog', linthresh=10.0)  # linthresh equivalent to threshold
    plt.title('Symlog Scale Example')
    plt.xlabel('x')
    plt.ylabel('x³ - 50x')
    plt.grid(True, alpha=0.3)
    plt.savefig('example/scale_examples/symlog_scale_ref.png', dpi=150, bbox_inches='tight')
    plt.savefig('example/scale_examples/symlog_scale_ref.pdf', bbox_inches='tight')
    plt.close()
    
    print("Created: symlog_scale_ref.png/pdf")

if __name__ == "__main__":
    log_scale_demo()
    symlog_scale_demo()