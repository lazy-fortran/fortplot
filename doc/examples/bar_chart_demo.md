title: Bar Chart Demo
---

# Bar Chart Demo

Source: [bar_chart_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/bar_chart_demo/bar_chart_demo.f90)

Demonstrates grouped vertical and horizontal bar charts using both the stateful
(pyplot-style) and object-oriented (`figure_t`) fortplot APIs.

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
- Generated media in `output/example/fortran/bar_chart_demo/`

## Running

```bash
make example ARGS="bar_chart_demo"
```

## Output

### Oo Grouped

![oo_grouped.png](../../media/examples/bar_chart_demo/oo_grouped.png)

ASCII output:
```
(Content embedded here)
```

[Download ASCII](../../media/examples/bar_chart_demo/oo_grouped.txt)

[Download PDF](../../media/examples/bar_chart_demo/oo_grouped.pdf)

### Stateful Grouped

![stateful_grouped.png](../../media/examples/bar_chart_demo/stateful_grouped.png)

ASCII output:
```
(Content embedded here)
```

[Download ASCII](../../media/examples/bar_chart_demo/stateful_grouped.txt)

[Download PDF](../../media/examples/bar_chart_demo/stateful_grouped.pdf)

### Stateful Horizontal

![stateful_horizontal.png](../../media/examples/bar_chart_demo/stateful_horizontal.png)

ASCII output:
```
(Content embedded here)
```

[Download ASCII](../../media/examples/bar_chart_demo/stateful_horizontal.txt)

[Download PDF](../../media/examples/bar_chart_demo/stateful_horizontal.pdf)

