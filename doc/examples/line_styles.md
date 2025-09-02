title: Line Styles
---

# Line Styles

Source: [example/fortran/line_styles/line_styles.f90](../../example/fortran/line_styles/line_styles.f90)

This example demonstrates all available line styles in fortplotlib, showing how to customize the appearance of plotted lines.

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

## Output

### Line Styles

![line_styles.png](../../media/examples/line_styles/line_styles.png)

<!-- ASCII preview removed to keep pages concise; full ASCII linked below. -->

> **Full ASCII Output**: [Download line_styles.txt](../../media/examples/line_styles/line_styles.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/line_styles/line_styles.pdf)
