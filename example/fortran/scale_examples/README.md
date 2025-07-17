title: Scale Examples
---

# Scale Examples

This example demonstrates different axis scaling options including logarithmic and symmetric logarithmic (symlog) scales.

## Files

- `scale_examples.f90` - Source code
- `log_scale.png/pdf/txt` - Logarithmic scale example
- `symlog_scale.png/pdf/txt` - Symmetric logarithmic scale example

## Running

```bash
make example ARGS="scale_examples"
```

## Features Demonstrated

- **Logarithmic scaling**: For exponential growth visualization
- **Symmetric log**: Handles positive and negative values with log-like behavior
- **Linear threshold**: Symlog parameter controls transition to linear near zero
- **Automatic tick generation**: Smart tick placement for non-linear scales

## Output Examples

The example generates the following output files:
- `log_scale.png` - Demonstrates logarithmic scaling for exponential growth visualization
- `log_scale.pdf` - Vector format of the logarithmic scale plot
- `log_scale.txt` - ASCII art representation with logarithmic axes
- `symlog_scale.png` - Shows symmetric logarithmic scaling that handles positive and negative values
- `symlog_scale.pdf` - Vector format of the symlog scale plot
- `symlog_scale.txt` - ASCII art representation with symmetric log axes

See the [documentation gallery](https://krystophny.github.io/fortplotlib/) for visual examples.