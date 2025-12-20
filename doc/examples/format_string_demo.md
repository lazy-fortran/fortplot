title: Format String Demo
---

# Format String Demo

Source: [format_string_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/format_string_demo/format_string_demo.f90)

Matplotlib-style format strings for color, marker, and line style.

## Files

- `format_string_demo.f90` - Source code
- Generated media in `output/example/fortran/format_string_demo/`

## Running

```bash
make example ARGS="format_string_demo"
```

## Output

### Format String Demo

![format_string_demo.png](../../media/examples/format_string_demo/format_string_demo.png)

ASCII output:
```

                      Matplotlib-style Format Strings Demo
+--------------------------------------------------------------------------------+
| 1.0                                                                            |
| *                                                                              |
|  = =     - -- --                               == ==       - -- -              |
|     =  -                                    -- sin(x) - solid line-            |
| *     -          -o o oo oo oo o            -- cos(x) - dashed line-           |
| 0.5  -=       oo o-              oo       = o sin(x/2) - circles only          |
| xx xx xx    o      -                oo   =  x cos(x/2) - x markers with line   |
| *  -    =x x        -                  o=            -   ==          -         |
|    -   o =  x x      --                 o o         -     =           -        |
| * -   o   =    x x                     =    o      -       =           -       |
|  - oo      =      x x -               =      o     -        =           -      |
| 0.0        =          x-             =         o  -          =           - x x |
| o           =          x-x          =           o-o           =          x     |
|              =           -x x       =           -  o                  x x -    |
|               =          -   x     =           -     o        =      x     -   |
| *             =           -    x x=            -      o        =  xx        -  |
|                =           -     =x x         -         o o   x x            - |
| -0.5            =           -   ==   x xx    -            xx x   ==            |
|                  =           -  =         x xx xx xx xx x    oo   =            |
|                                =            -                   o o=           |
| *                 =          = -          -                        o oo oo o o |
|                     =       =    -      -                            =         |
| ** * * * * * * * * * *=* ** * * * * *-*-* * * * * ** * * * * * * * * *=*=* **= |
|0              2                4               6               8               |
+--------------------------------------------------------------------------------+
                                    X values
Y values
```

[Download ASCII](../../media/examples/format_string_demo/format_string_demo.txt)

[Download PDF](../../media/examples/format_string_demo/format_string_demo.pdf)

