title: Basic Plots Example
---

# Basic Plots

This example demonstrates the fundamental plotting capabilities of fortplot using both the simple functional API and the object-oriented interface.

## Files

- `basic_plots.f90` - Source code
- `simple_plot.png/pdf/txt` - Simple sine wave outputs
- `multi_line.png/pdf/txt` - Multi-line plot outputs

## Running

```bash
make example ARGS="basic_plots"
```

## Features Demonstrated

- **Functional API**: Simple, matplotlib-like interface with global figure management
- **Object-Oriented API**: More control through `figure_t` type
- **Multiple output formats**: PNG, PDF, and ASCII text
- **Line labeling**: Automatic legend generation
- **Axis labeling**: Clear axis titles and labels

## Output Examples

The example generates the following output files:
- `simple_plot.png` - Simple sine wave visualization
- `multi_line.png` - Multiple functions on the same plot

See the [documentation gallery](https://krystophny.github.io/fortplot/) for visual examples.