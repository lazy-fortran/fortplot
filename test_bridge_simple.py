#!/usr/bin/env python3
"""
Simple test to verify the bridge actually creates files.
"""

import numpy as np
import sys
import os
sys.path.insert(0, "python")
import fortplot

# Clean up any previous test files
test_files = ['simple_test.png', 'simple_test.pdf', 'simple_test.txt']
for f in test_files:
    if os.path.exists(f):
        os.remove(f)

print("Testing bridge with simple data...")

try:
    # Create simple test data
    x = np.array([1, 2, 3])
    y = np.array([1, 4, 9])
    
    # Create plot
    fortplot.figure()
    fortplot.plot(x, y, label='test data')
    fortplot.xlabel('X Values')
    fortplot.ylabel('Y Values')
    fortplot.title('Simple Bridge Test')
    fortplot.legend()
    
    # Save to current directory 
    fortplot.savefig('simple_test.png')
    fortplot.savefig('simple_test.pdf')
    fortplot.savefig('simple_test.txt')
    
    # Check results
    print("Checking for output files...")
    for filename in test_files:
        if os.path.exists(filename):
            size = os.path.getsize(filename)
            print(f"✓ {filename}: {size} bytes")
        else:
            print(f"✗ {filename}: not found")
    
    print("Test completed!")

except Exception as e:
    print(f"Error: {e}")
    import traceback
    traceback.print_exc()