title: Bar Chart Demo
---

# Bar Chart Demo

Source: [bar_chart_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/bar_chart_demo/bar_chart_demo.f90)

Demonstrates grouped bar charts (vertical and horizontal) via both the stateful API and `figure_t`.

- `bar_chart_demo.f90` - Source demonstrating grouped and horizontal bars
- Generated outputs in `output/example/fortran/bar_chart_demo/`

- **Grouped bars**: Compare multiple series per category
- **Horizontal bars**: Present ranked metrics across categories
- **OO workflow**: Call `bar_impl` with an explicit `figure_t` instance to avoid globals
- **Multiple formats**: Save PNG, PDF, and ASCII outputs for each scenario

Running the demo generates:
- `stateful_grouped.(png|pdf|txt)` - Vertical grouped comparison
- `stateful_horizontal.(png|pdf|txt)` - Horizontal completion chart
- `oo_grouped.(png|pdf|txt)` - Object API grouped budget comparison

Perfect for showcasing categorical comparisons and verifying bar chart support
in fortplot.

## Files

- `bar_chart_demo.f90` - Source code
- Run the example to populate `output/example/fortran/bar_chart_demo/`

## Running

```bash
make example ARGS="bar_chart_demo"
```

## Output

Run this example to generate plots and other media assets.

