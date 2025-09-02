title: Streamplot Demo
---

# Streamplot Demo

Source: [example/fortran/streamplot_demo/streamplot_demo.f90](../../example/fortran/streamplot_demo/streamplot_demo.f90)

This example shows vector field visualization using streamlines.

## Files

- `streamplot_demo.f90` - Source code
- `streamplot_demo.png/pdf/txt` - Vector field streamlines

## Running

```bash
make example ARGS="streamplot_demo"
```

## Features Demonstrated

- **Vector field visualization**: Shows flow direction and magnitude
- **Adaptive density**: Streamline placement based on field properties
- **Arrow indicators**: Direction shown with arrows
- **Integration accuracy**: RK4 integration for smooth curves

## Vector Field Components

- **U component**: Horizontal velocity/force
- **V component**: Vertical velocity/force
- **Magnitude**: Shown by streamline density
- **Direction**: Indicated by arrows along streamlines

## Output Example

![Streamplot Demo](../../output/example/fortran/streamplot_demo/streamplot_demo.png)
