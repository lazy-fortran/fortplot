# twin_axes_demo

Demonstrates `twinx` and `twiny` for multiple axes on one figure.

The demo plots three synthetic time series and assigns each to a different axis:

- Left y-axis: primary signal (temperature-like)
- Right y-axis: secondary signal with log scaling
- Top x-axis: cumulative index with log scaling

Run the example via:

```bash
make example ARGS="twin_axes_demo"
```

Outputs are written to `output/example/fortran/twin_axes_demo/` in PNG and ASCII
formats.
