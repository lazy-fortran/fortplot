title: Twin Axes Demo
---

# Twin Axes Demo

Source: [twin_axes_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/twin_axes_demo/twin_axes_demo.f90)

Demonstrates the `twinx` and `twiny` helpers introduced in issue #1358. The
demo plots three synthetic time series and assigns each to a different axis:

- Left y-axis: primary signal (temperature-like)
- Right y-axis: secondary signal with log scaling
- Top x-axis: cumulative index with log scaling

Run the example via:

Outputs are written to `output/example/fortran/twin_axes_demo/` in PNG and ASCII
formats.

## Files

- `twin_axes_demo.f90` - Source code
- Generated media in `output/example/fortran/twin_axes_demo/`

## Running

```bash
make example ARGS="twin_axes_demo"
```

## Output

### Twin Axes Demo

![twin_axes_demo.png](../../media/examples/twin_axes_demo/twin_axes_demo.png)

ASCII output:
```
(Content embedded here)
```

[Download ASCII](../../media/examples/twin_axes_demo/twin_axes_demo.txt)

