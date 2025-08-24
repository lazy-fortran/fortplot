#!/usr/bin/env python3
"""
Debug the bridge connection by testing basic functionality.
"""

import numpy as np
import sys
sys.path.insert(0, "python")
import fortplot

print("Testing bridge connection...")

try:
    # Simple test
    x = np.array([1, 2, 3])
    y = np.array([1, 4, 9])
    
    print("Creating figure...")
    fortplot.figure()
    
    print("Adding plot...")
    fortplot.plot(x, y, label='test')
    
    print("Adding labels...")
    fortplot.xlabel('x')
    fortplot.ylabel('y')
    fortplot.title('Debug Test')
    
    print("Saving figure...")
    fortplot.savefig('debug_test.png')
    
    print("Test completed!")
    
    # Check if file exists
    import os
    if os.path.exists('debug_test.png'):
        print(f"✓ PNG file created: {os.path.getsize('debug_test.png')} bytes")
    else:
        print("✗ PNG file was not created")

except Exception as e:
    print(f"✗ Test failed with error: {e}")
    import traceback
    traceback.print_exc()