#!/usr/bin/env python3
from PIL import Image
import numpy as np

img = Image.open('test_line_styles_simple.png')
arr = np.array(img)
h, w, _ = arr.shape

print(f'Image size: {w}x{h}')
print('\nAnalyzing horizontal lines for patterns...\n')

# Find lines by looking for rows with many non-white pixels
line_rows = []
for y in range(h):
    row = arr[y, :]
    non_white_count = np.sum(np.any(row != [255, 255, 255], axis=1))
    if non_white_count > 50:  # A line should have many pixels
        line_rows.append((y, non_white_count))

print(f'Found {len(line_rows)} potential line rows')

# Analyze each line for patterns
for y, count in line_rows[:10]:  # Check first 10 lines
    row = arr[y, :]
    non_white_mask = np.any(row != [255, 255, 255], axis=1)
    non_white_indices = np.where(non_white_mask)[0]
    
    if len(non_white_indices) > 1:
        # Check for gaps
        diffs = np.diff(non_white_indices)
        gaps = diffs[diffs > 1]
        
        if len(gaps) > 0:
            print(f'Row {y}: PATTERNED - {len(gaps)} gaps found, avg gap size: {np.mean(gaps):.1f}')
        else:
            print(f'Row {y}: SOLID - continuous line ({count} pixels)')
    else:
        print(f'Row {y}: Single pixel or no line')