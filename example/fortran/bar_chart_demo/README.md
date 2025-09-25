title: Bar Chart Demo
---

# Bar Chart Demo

Demonstrates grouped vertical and horizontal bar charts using both the stateful
(pyplot-style) and object-oriented (`figure_t`) fortplot APIs.

## Files

- `bar_chart_demo.f90` - Source demonstrating grouped and horizontal bars
- Generated outputs in `output/example/fortran/bar_chart_demo/`

## Running

```bash
make example ARGS="bar_chart_demo"
```

## Features Demonstrated

- **Grouped bars**: Compare multiple series per category
- **Horizontal bars**: Present ranked metrics across categories
- **OO workflow**: Call `bar_impl` with an explicit `figure_t` instance to avoid globals
- **Multiple formats**: Save PNG, PDF, and ASCII outputs for each scenario

## Output Summary

Running the demo generates:
- `stateful_grouped.(png|pdf|txt)` - Vertical grouped comparison
- `stateful_horizontal.(png|pdf|txt)` - Horizontal completion chart
- `oo_grouped.(png|pdf|txt)` - Object API grouped budget comparison

Perfect for showcasing categorical comparisons and verifying bar chart support
in fortplot.
