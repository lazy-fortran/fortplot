title: Format String Demo
---

# Format String Demo

Source: [format_string_demo.f90](../../sourcefile/format_string_demo.f90.html)

This example demonstrates matplotlib-style format strings for quick and intuitive plot styling.

## Files

- `format_string_demo.f90` - Source code
- `format_string_demo.png/pdf/txt` - Various format string examples

## Running

```bash
make example ARGS="format_string_demo"
```

## Features Demonstrated

- **Color shortcuts**: Single letter color codes
- **Line style codes**: Solid, dashed, dotted
- **Marker codes**: Combined with line styles
- **Compact notation**: Full styling in one string

## Format String Syntax

Format: `[color][marker][linestyle]`

### Colors
- `b` - blue
- `r` - red
- `g` - green
- `k` - black
- `m` - magenta
- `c` - cyan
- `y` - yellow

### Line Styles
- `-` - solid line
- `--` - dashed line
- `:` - dotted line
- `-.` - dash-dot line

### Markers
- `o` - circle
- `s` - square
- `^` - triangle
- `*` - star

## Examples

- `'r-'` - Red solid line
- `'bo'` - Blue circles (no line)
- `'g--'` - Green dashed line
- `'k:o'` - Black dotted line with circles

## Output Example

![Format String Demo](../../media/examples/format_string_demo/format_string_demo.png)
