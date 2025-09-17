# fill_between_demo

Demonstrates how to create filled regions using both the stateful and object
APIs. The program first applies `fill_between` with a mask to highlight part of
an oscillatory curve, then uses `figure_t%add_fill_between` to shade a baseline
response computed from an exponential envelope.

## Run It

```bash
make example ARGS="fill_between_demo"
```

Generated files are written to `output/example/fortran/fill_between_demo/` in
both PNG and ASCII formats.
