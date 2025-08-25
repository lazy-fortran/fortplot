title: Ascii Heatmap
---

# ASCII Heatmap

This example demonstrates terminal-based heatmap visualization using ASCII characters.

## Files

- `ascii_heatmap_demo.f90` - Source code
- Output shown directly in terminal

## Running

```bash
make example ARGS="ascii_heatmap_demo"
```

## Features Demonstrated

- **Terminal visualization**: No GUI required
- **Density mapping**: Characters represent values
- **Automatic scaling**: Data range mapped to characters
- **Compact display**: Efficient space usage

## ASCII Character Mapping

Values mapped to density characters:
- ` ` (space) - Lowest values
- `.` - Low values
- `:` - Medium-low values
- `-` - Medium values
- `=` - Medium-high values
- `+` - High values
- `*` - Higher values
- `#` - Highest values

## Use Cases

- **Remote sessions**: When GUI not available
- **Quick visualization**: Fast data inspection
- **Log files**: Can be saved as text
- **CI/CD pipelines**: Terminal-based testing

## Example Output

```
ASCII Heatmap Demo
==================

    #***+++===---:::...
    **+++===---:::...
    *+++===---:::...
    +++===---:::...
    ++===---:::...
    +===---:::...
    ===---:::...
    ==---:::...
    =---:::...
    ---:::...
```
