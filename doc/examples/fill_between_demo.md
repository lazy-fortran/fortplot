title: Fill Between Demo
---

# Fill Between Demo

Source: [fill_between_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/fill_between_demo/fill_between_demo.f90)

Demonstrates how to create filled regions using both the stateful and object
APIs. The program first applies `fill_between` with a mask to highlight part of
an oscillatory curve, then uses `figure_t%add_fill_between` to shade a baseline
response computed from an exponential envelope.

Generated files are written to `output/example/fortran/fill_between_demo/` in
both PNG and ASCII formats.

## Files

- `fill_between_demo.f90` - Source code
- Run the example to populate `output/example/fortran/fill_between_demo/`

## Running

```bash
make example ARGS="fill_between_demo"
```

## Output

Run this example to generate plots and other media assets.

