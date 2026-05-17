title: Quiver Demo
---

# Quiver Demo

Source: [quiver_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/quiver_demo/quiver_demo.f90)

See source and outputs below.

## Files

- `quiver_demo.f90` - Source code
- Generated media in `output/example/fortran/quiver_demo/`

## Running

```bash
make example ARGS="quiver_demo"
```

## Output

### Quiver Demo

![quiver_demo.png](../../media/examples/quiver_demo/quiver_demo.png)

ASCII output:
```

                        Quiver Plot Demo - Circular Flow
+--------------------------------------------------------------------------------+
| 2.0                                                                            |
| |                                                                              |
|                                                                                |
| |                                                                              |
| 1.5                                                                            |
| |                                                                              |
| |     \        \        >        >        >        >        /        /         |
| 1.0                                                                            |
| |     \        \        \        >        >        /        /        /         |
| |     \        \        \        >        >        /        /        /         |
| 0.5                                                                            |
| |     \        \        \        >        >        /        /        /         |
|                                                                                |
| |     v        v        v        \        /        ^        ^        ^         |
| |     v        v        v        /        \        ^        ^        ^         |
| 0.0                                                                            |
| |     /        /        /        <        <        \        \        \         |
| |                                                                              |
| -0.5  /        /        /        <        <        \        \        \         |
| |     /        /        /        <        <        \        \        \         |
|                                                                                |
| |     /        /        <        <        <        <        \        \         |
| |                                                                              |
| -1.0                                                                           |
| |                                                                              |
| |                                                                              |
| -1.5                                                                           |
| |                                                                              |
| |   -    -   -    -   -    -   -    -   -    -   -    -   -    -   -    -    - |
| -2.0     -1.5      -1.0     -0.5      0.0       0.5       1.0      1.5    2.0  |
+--------------------------------------------------------------------------------+
                                       X
Y
```

[Download ASCII](../../media/examples/quiver_demo/quiver_demo.txt)

[Download PDF](../../media/examples/quiver_demo/quiver_demo.pdf)

### Quiver Scaled

![quiver_scaled.png](../../media/examples/quiver_demo/quiver_scaled.png)

ASCII output:
```

                       Quiver Plot Demo - Smaller Arrows
+--------------------------------------------------------------------------------+
| 2.0                                                                            |
| |                                                                              |
|                                                                                |
| |                                                                              |
| 1.5                                                                            |
| |                                                                              |
| |     \        \        >        >        >        >        /        /         |
| 1.0                                                                            |
| |     \        \        \        >        >        /        /        /         |
| |     \        \        \        >        >        /        /        /         |
| 0.5                                                                            |
| |     \        \        \        >        >        /        /        /         |
|                                                                                |
| |     v        v        v        \        /        ^        ^        ^         |
| |     v        v        v        /        \        ^        ^        ^         |
| 0.0                                                                            |
| |     /        /        /        <        <        \        \        \         |
| |                                                                              |
| -0.5  /        /        /        <        <        \        \        \         |
| |     /        /        /        <        <        \        \        \         |
|                                                                                |
| |     /        /        <        <        <        <        \        \         |
| |                                                                              |
| -1.0                                                                           |
| |                                                                              |
| |                                                                              |
| -1.5                                                                           |
| |                                                                              |
| |   -    -   -    -   -    -   -    -   -    -   -    -   -    -   -    -    - |
| -2.0     -1.5      -1.0     -0.5      0.0       0.5       1.0      1.5    2.0  |
+--------------------------------------------------------------------------------+
                                       X
Y
```

[Download ASCII](../../media/examples/quiver_demo/quiver_scaled.txt)

[Download PDF](../../media/examples/quiver_demo/quiver_scaled.pdf)

