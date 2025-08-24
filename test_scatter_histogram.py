#!/usr/bin/env python3
"""
Test scatter and histogram functionality specifically.
"""

import numpy as np
import sys
import os
sys.path.insert(0, "python")
import fortplot

# Test scatter plot
print("Testing scatter plot...")
try:
    x = np.random.random(20) * 10
    y = np.random.random(20) * 10
    
    fortplot.figure()
    fortplot.scatter(x, y, label='random points')
    fortplot.xlabel('X Values')
    fortplot.ylabel('Y Values')
    fortplot.title('Scatter Plot Test')
    fortplot.legend()
    fortplot.savefig('test_scatter.png')
    
    if os.path.exists('test_scatter.png'):
        size = os.path.getsize('test_scatter.png')
        print(f"✓ Scatter plot created: {size} bytes")
    else:
        print("✗ Scatter plot failed")

except Exception as e:
    print(f"✗ Scatter plot error: {e}")

# Test histogram
print("Testing histogram...")
try:
    data = np.random.normal(0, 1, 1000)
    
    fortplot.figure()
    fortplot.histogram(data, label='normal distribution')
    fortplot.xlabel('Value')
    fortplot.ylabel('Frequency')
    fortplot.title('Histogram Test')
    fortplot.legend()
    fortplot.savefig('test_histogram.png')
    
    if os.path.exists('test_histogram.png'):
        size = os.path.getsize('test_histogram.png')
        print(f"✓ Histogram created: {size} bytes")
    else:
        print("✗ Histogram failed")

except Exception as e:
    print(f"✗ Histogram error: {e}")

print("Tests completed!")