title: Streamplot Demo
---

# Streamplot Demo

This example shows vector field visualization using streamlines.

## Source Files

### Fortran Source

📄 [streamplot_demo.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/streamplot_demo/streamplot_demo.f90)

### Python Equivalent

🐍 [streamplot_demo.py](https://github.com/krystophny/fortplotlib/blob/main/example/python/streamplot_demo/streamplot_demo.py)

### Generated Output Files

- Various output files in PNG, PDF, and ASCII formats

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

![Streamplot Demo](../../media/examples/streamplot_demo.png)
