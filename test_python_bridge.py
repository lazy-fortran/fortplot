#!/usr/bin/env python3
"""Test the Python bridge claimed in README"""

try:
    import numpy as np
    import fortplot.fortplot as plt
    
    print("Testing Python bridge from README...")
    
    # Test basic example from README
    x = np.linspace(0, 10, 50) 
    y = np.sin(x)
    
    plt.figure()
    plt.plot(x, y)
    plt.title("Python Bridge Test")
    plt.xlabel("x")
    plt.ylabel("sin(x)")
    plt.savefig("python_bridge_test.png")
    
    print("Python bridge test completed successfully")
    
except ImportError as e:
    print(f"Import error: {e}")
    print("Python bridge not properly installed")
except Exception as e:
    print(f"Error during execution: {e}")
    print("Python bridge has functional issues")