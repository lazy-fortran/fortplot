title: Grid Demo
---

# Grid Demo

Source: [grid_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/grid_demo/grid_demo.f90)

Demonstrates grid functionality for enhanced plot readability.

- `grid_demo.f90` - Source code
- `grid_demo.png/pdf/txt` - Example outputs

- **Grid lines**: Major and minor grid lines
- **Grid customization**: Line styles, colors, transparency
- **Axis control**: Grid alignment with tick marks
- **Professional presentation**: Enhanced plot readability

The demo shows how grid lines improve data visualization and readability.

## Files

- `grid_demo.f90` - Source code
- Generated media in `output/example/fortran/grid_demo/`

## Running

```bash
make example ARGS="grid_demo"
```

## Output

### Grid Demo

![grid_demo.png](../../media/examples/grid_demo/grid_demo.png)

ASCII output:
```

                             Basic Plot - Grid Demo
+--------------------------------------------------------------------------------+
|                                                                                |
| *           -                                                                  |
| =        --  --                                     =                          |
|  == =   -       -                              === =  =       -- Damped sine   |
| 0.5   -          -                           =         =      -- Cosine        |
| *    =            -                         =           =                      |
|       =             -                                     =                    |
| *   -   =                                  =               =- --- -            |
|          =           -                   =              - -        --          |
|   -                                                    -    =         -        |
| *-        =           -                 =             -                --      |
| 0.0                     -              =            -         =           -    |
| -           =                                      -           =           -   |
|              =           -           =           -                          -  |
| *                          -                    -               =              |
|               =                     =          -                  =            |
| *                           -      =         -                                 |
|                 =            -                                     =           |
|                  =             - =         --                       =          |
| -0.5              =             =-       -                                     |
|                                =   --- --                             =        |
|                     ==       =                                         =       |
| ** * * * * * * * * * *=*=* *=* * * * * * * * * * * * * * * * * * * * * *=*=**  |
|0               2                4                6               8             |
+--------------------------------------------------------------------------------+
                                    Time (s)
Amplitude
```

[Download ASCII](../../media/examples/grid_demo/grid_demo.txt)

[Download PDF](../../media/examples/grid_demo/grid_demo.pdf)

