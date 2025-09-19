title: Scatter Demo
---

# Scatter Demo

Source: [scatter_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/scatter_demo/scatter_demo.f90)

Demonstrates enhanced scatter plotting with color mapping, variable marker sizes, and bubble charts.

- `scatter_demo.f90` - Source code
- `scatter_plot.png/pdf/txt` - Basic scatter outputs
- `bubble_chart.png/pdf/txt` - Bubble chart outputs

- **Basic scatter plots**: Simple point visualization
- **Bubble charts**: Variable marker sizes based on data values
- **Color mapping**: Colors based on data values with colorbar
- **Multiple marker shapes**: Different point styles
- **Scientific data visualization**: Proper scaling and presentation

The demo generates scatter plots showing different visualization techniques for multi-dimensional data presentation.

## Files

- `scatter_demo.f90` - Source code
- Generated media in `output/example/fortran/scatter_demo/`

## Running

```bash
make example ARGS="scatter_demo"
```

## Output

### Scatter Basic

![scatter_basic.png](../../media/examples/scatter_demo/scatter_basic.png)

### Scatter Gaussian

![scatter_gaussian.png](../../media/examples/scatter_demo/scatter_gaussian.png)

### Scatter Multi

![scatter_multi.png](../../media/examples/scatter_demo/scatter_multi.png)

