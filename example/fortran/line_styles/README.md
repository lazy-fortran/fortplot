title: Line Styles Example
---

# Line Styles

This example demonstrates all available line styles in fortplot, showing how to customize the appearance of plotted lines.

## Files

- `line_styles.f90` - Source code
- `line_styles.png/pdf/txt` - Output showing all line styles

## Running

```bash
make example ARGS="line_styles"
```

## Features Demonstrated

- **Named Constants**: Use predefined constants for better code readability
- **String Shortcuts**: Compatible with matplotlib-style strings
- **Marker Combinations**: Combine with markers for scatter plots
- **Clear Separation**: Data offset vertically for visual clarity

## Available Line Styles

- Solid line (`-` or `LINESTYLE_SOLID`)
- Dashed line (`--` or `LINESTYLE_DASHED`)
- Dotted line (`:` or `LINESTYLE_DOTTED`)
- Dash-dot line (`-.` or `LINESTYLE_DASHDOT`)

## Output Examples

The example generates the following output files:
- `line_styles.png` - Shows all available line styles (solid, dashed, dotted, dash-dot) with clear visual separation
- `line_styles.pdf` - Vector format of the same visualization
- `line_styles.txt` - ASCII art representation of the line styles

See the [documentation gallery](https://krystophny.github.io/fortplot/) for visual examples.