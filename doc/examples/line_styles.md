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

![line_styles.png](../../output/example/fortran/line_styles/line_styles.png)

ASCII output preview:
```
                         Complete Line Style Reference
+--------------------------------------------------------------------------------+
|3.0  ________________                              -- Solid (-)                  |
|                                                                                |
|2.0  ---- ---- ---- ---- ----                     -- Dashed (--)               |
|                                                                                |
|1.0  .... .... .... .... ....                     -- Dotted (:)               |
|                                                                                |
|0.0  _._ _._ _._ _._ _._ _._                        -- Dash-dot (-.)             |
|                                                                                |
|-1.0+--------+----------+----------+----------+----------+--------+            |
     0        2          4          6          8         10                     |
+--------------------------------------------------------------------------------+
                                       x
Line Styles
```

> **Full ASCII Output**: [Download line_styles.txt](../../output/example/fortran/line_styles/line_styles.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../output/example/fortran/line_styles/line_styles.pdf)