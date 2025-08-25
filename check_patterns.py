#!/usr/bin/env python3
from PIL import Image
import numpy as np

img = Image.open('output/example/fortran/line_styles/line_styles.png')
arr = np.array(img)

# Sample multiple horizontal lines to detect patterns
print('Checking for line style patterns...')
print('Row# | Non-white pixels | Pattern detected')
print('-' * 45)

pattern_rows = []
for y in range(100, 500, 20):
    row = arr[y, :]
    # Get indices of non-white pixels
    non_white_mask = np.any(row != [255, 255, 255], axis=1)
    non_white_indices = np.where(non_white_mask)[0]
    num_non_white = len(non_white_indices)
    
    # Check for gaps (pattern) by looking at consecutive indices
    has_gaps = False
    if num_non_white > 1:
        diffs = np.diff(non_white_indices)
        # If there are jumps > 2 pixels, we have gaps (patterns)
        has_gaps = np.any(diffs > 2)
    
    pattern_type = 'PATTERNED' if has_gaps else ('SOLID' if num_non_white > 10 else 'NONE')
    print(f'{y:4d} | {num_non_white:16d} | {pattern_type}')
    
    if has_gaps:
        pattern_rows.append(y)

print(f'\nRows with patterns detected: {len(pattern_rows)}')
if pattern_rows:
    print('Pattern rows:', pattern_rows[:5], '...' if len(pattern_rows) > 5 else '')
else:
    print('WARNING: No patterned lines detected! Line styles may not be working.')