#!/usr/bin/env python3
from PIL import Image
import numpy as np
import sys

filename = sys.argv[1] if len(sys.argv) > 1 else 'output/example/fortran/line_styles/line_styles.png'
img = Image.open(filename)
arr = np.array(img)
h, w, _ = arr.shape

print(f'Analyzing {filename}')
print(f'Image size: {w}x{h}\n')

# Find all horizontal segments 
segments_by_row = {}
for y in range(h):
    row = arr[y, :]
    non_white_mask = np.any(row != [255, 255, 255], axis=1)
    non_white_indices = np.where(non_white_mask)[0]
    
    if len(non_white_indices) > 0:
        # Find continuous segments
        segments = []
        start = non_white_indices[0]
        for i in range(1, len(non_white_indices)):
            if non_white_indices[i] - non_white_indices[i-1] > 1:
                # Gap found - end current segment
                segments.append((start, non_white_indices[i-1]))
                start = non_white_indices[i]
        segments.append((start, non_white_indices[-1]))
        
        if len(segments) > 1:  # Multiple segments = pattern
            segments_by_row[y] = segments

print(f'Found {len(segments_by_row)} rows with patterns (multiple segments)')

# Group consecutive patterned rows as lines
if segments_by_row:
    lines = []
    current_line = [list(segments_by_row.keys())[0]]
    
    for y in list(segments_by_row.keys())[1:]:
        if y - current_line[-1] <= 2:  # Within 2 pixels = same line
            current_line.append(y)
        else:
            lines.append(current_line)
            current_line = [y]
    lines.append(current_line)
    
    print(f'\nFound {len(lines)} patterned lines:')
    for i, line in enumerate(lines[:5]):  # Show first 5
        y = line[len(line)//2]  # Middle row of line
        segments = segments_by_row[y]
        gaps = [segments[i+1][0] - segments[i][1] for i in range(len(segments)-1)]
        avg_gap = np.mean(gaps) if gaps else 0
        print(f'  Line {i+1}: Y={min(line)}-{max(line)}, {len(segments)} segments, avg gap: {avg_gap:.1f} pixels')
else:
    print('\nNo patterned lines found! All lines appear to be solid.')
    
# Check for solid lines too
solid_rows = []
for y in range(h):
    row = arr[y, :]
    non_white_mask = np.any(row != [255, 255, 255], axis=1)
    non_white_indices = np.where(non_white_mask)[0]
    
    if len(non_white_indices) > 100:  # Long continuous line
        diffs = np.diff(non_white_indices)
        if np.all(diffs == 1):  # All consecutive
            solid_rows.append(y)

if solid_rows:
    print(f'\nFound {len(solid_rows)} solid line rows')
    print(f'  First few at Y: {solid_rows[:5]}')