title: Pie Chart Demo
---

# Pie Chart Demo

Source: [pie_chart_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/pie_chart_demo/pie_chart_demo.f90)

This example shows how to build pie charts with fortplot using both the stateful
API and the `figure_t` object API. It demonstrates exploded wedges, automatic
percentage labels via `autopct`, custom start angles, and multiple output formats
(PNG, PDF, ASCII).

Run it with:

Generated media is written to `output/example/fortran/pie_chart_demo/`.

## Files

- `pie_chart_demo.f90` - Source code
- Generated media in `output/example/fortran/pie_chart_demo/`

## Running

```bash
make example ARGS="pie_chart_demo"
```

## Output

### Oo Energy

![oo_energy.png](../../media/examples/pie_chart_demo/oo_energy.png)

ASCII output:
```
(Content embedded here)
```

[Download ASCII](../../media/examples/pie_chart_demo/oo_energy.txt)

[Download PDF](../../media/examples/pie_chart_demo/oo_energy.pdf)

### Stateful Sales

![stateful_sales.png](../../media/examples/pie_chart_demo/stateful_sales.png)

ASCII output:
```
(Content embedded here)
```

[Download ASCII](../../media/examples/pie_chart_demo/stateful_sales.txt)

[Download PDF](../../media/examples/pie_chart_demo/stateful_sales.pdf)

