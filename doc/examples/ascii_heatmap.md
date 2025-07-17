title: Ascii Heatmap
---

# ASCII Heatmap

This example demonstrates terminal-based heatmap visualization using ASCII characters.

## Source Files

### Fortran Source

📄 [ascii_heatmap_demo.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/ascii_heatmap/ascii_heatmap_demo.f90)

### Generated Output Files

- Various output files in PNG, PDF, and ASCII formats

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
