#!/usr/bin/env python3
"""Test the new Python interface functions"""

import numpy as np
import sys
import os

# Add the python directory to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'python'))

try:
    import fortplotlib.fortplot as plt
    print("✓ Successfully imported fortplotlib.fortplot")
except ImportError as e:
    print(f"✗ Failed to import fortplotlib: {e}")
    sys.exit(1)

def test_basic_plot():
    """Test basic plot functionality"""
    print("\n=== Testing basic plot ===")
    x = np.linspace(0, 10, 100)
    y = np.sin(x)
    
    plt.figure()
    plt.plot(x, y, label="sin(x)")
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Basic plot test")
    plt.legend()
    plt.savefig("test_basic_plot.png")
    print("✓ Basic plot test passed")

def test_contour():
    """Test contour functionality"""
    print("\n=== Testing contour ===")
    x = np.linspace(-2, 2, 20)
    y = np.linspace(-2, 2, 20)
    X, Y = np.meshgrid(x, y)
    Z = X**2 + Y**2
    
    plt.figure()
    plt.contour(X, Y, Z)
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Contour test")
    plt.savefig("test_contour.png")
    print("✓ Contour test passed")

def test_contourf():
    """Test filled contour functionality"""
    print("\n=== Testing contourf ===")
    x = np.linspace(-2, 2, 20)
    y = np.linspace(-2, 2, 20)
    X, Y = np.meshgrid(x, y)
    Z = X**2 + Y**2
    
    plt.figure()
    plt.contourf(X, Y, Z)
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Filled contour test")
    plt.savefig("test_contourf.png")
    print("✓ Filled contour test passed")

def test_streamplot():
    """Test streamplot functionality"""
    print("\n=== Testing streamplot ===")
    x = np.linspace(-2, 2, 20)
    y = np.linspace(-2, 2, 20)
    X, Y = np.meshgrid(x, y)
    U = -Y  # Circular flow
    V = X
    
    plt.figure()
    plt.streamplot(x, y, U, V, density=1.0)
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Streamplot test")
    plt.savefig("test_streamplot.png")
    print("✓ Streamplot test passed")

def test_scaling():
    """Test scale functionality"""
    print("\n=== Testing scaling ===")
    x = np.logspace(-2, 2, 100)
    y = x**2
    
    plt.figure()
    plt.plot(x, y, label="x^2")
    plt.xscale("log")
    plt.yscale("log")
    plt.xlabel("x (log scale)")
    plt.ylabel("y (log scale)")
    plt.title("Log scale test")  
    plt.legend()
    plt.savefig("test_scaling.png")
    print("✓ Scaling test passed")

def main():
    """Run all tests"""
    print("Testing new Python interface functions...")
    
    try:
        test_basic_plot()
        test_contour()
        test_contourf()
        test_streamplot()
        test_scaling()
        
        print("\n🎉 All tests passed!")
        print("Generated test files:")
        for filename in ["test_basic_plot.png", "test_contour.png", "test_contourf.png", 
                        "test_streamplot.png", "test_scaling.png"]:
            if os.path.exists(filename):
                print(f"  ✓ {filename}")
            else:
                print(f"  ✗ {filename} (not found)")
                
    except Exception as e:
        print(f"\n❌ Test failed with error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main()