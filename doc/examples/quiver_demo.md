title: Quiver Demo
---

# Quiver Demo

Source: [quiver_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/quiver_demo/quiver_demo.f90)

Quiver plots for discrete vector fields with scaled arrows and backend outputs.

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
| 2                                                                              |
| |                                                                              |
|                                                                                |
| |                                                                              |
| |                                                                              |
|                                                                                |
| |                                                                              |
| |\       >       >       >       >        >       >       >       >       /    |
| 1\       \       >       >       >        >       >       >       /       /    |
| |                                                                              |
| |\       \       \       >       >        >       >       /       /       /    |
|  \       \       \       \       >        >       /       /       /       /    |
| |                                                                              |
| |v       v       v       \       \        /       /       ^       ^       ^    |
| 0v       v       v       /       /        \       \       ^       ^       ^    |
| |                                                                              |
| |/       /       /       /       <        <       \       \       \       \    |
|  /       /       /       <       <        <       <       \       \       \    |
| |                                                                              |
| |/       /       <       <       <        <       <       <       \       \    |
|  /       <       <       <       <        <       <       <       <       \    |
| |                                                                              |
| |                                                                              |
| -1                                                                             |
| |                                                                              |
| |                                                                              |
|                                                                                |
| |                                                                              |
| |   -   -   -    -   -   -   -    -   -   -    -   -   -   -    -   -   -    - |
| -2-2               -1                 0                 1                2     |
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
| 2                                                                              |
| |                                                                              |
|                                                                                |
| |                                                                              |
| |                                                                              |
|                                                                                |
| |                                                                              |
| |\       >       >       >       >        >       >       >       >       /    |
| 1\       \       >       >       >        >       >       >       /       /    |
| |                                                                              |
| |\       \       \       >       >        >       >       /       /       /    |
|  \       \       \       \       >        >       /       /       /       /    |
| |                                                                              |
| |v       v       v       \       \        /       /       ^       ^       ^    |
| 0v       v       v       /       /        \       \       ^       ^       ^    |
| |                                                                              |
| |/       /       /       /       <        <       \       \       \       \    |
|  /       /       /       <       <        <       <       \       \       \    |
| |                                                                              |
| |/       /       <       <       <        <       <       <       \       \    |
|  /       <       <       <       <        <       <       <       <       \    |
| |                                                                              |
| |                                                                              |
| -1                                                                             |
| |                                                                              |
| |                                                                              |
|                                                                                |
| |                                                                              |
| |   -   -   -    -   -   -   -    -   -   -    -   -   -   -    -   -   -    - |
| -2-2               -1                 0                 1                2     |
+--------------------------------------------------------------------------------+
                                       X
Y
```

[Download ASCII](../../media/examples/quiver_demo/quiver_scaled.txt)

[Download PDF](../../media/examples/quiver_demo/quiver_scaled.pdf)

