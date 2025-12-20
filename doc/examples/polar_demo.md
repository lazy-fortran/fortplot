title: Polar Demo
---

# Polar Demo

Source: [polar_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/polar_demo/polar_demo.f90)

Demonstrates fortplot's polar plotting API with custom colors, linestyles, and markers.

Generated outputs are written to `output/example/fortran/polar_demo/` as PNG and text summaries.

## Files

- `polar_demo.f90` - Source code
- Generated media in `output/example/fortran/polar_demo/`

## Running

```bash
make example ARGS="polar_demo"
```

## Output

### Polar Demo

![polar_demo.png](../../media/examples/polar_demo/polar_demo.png)

ASCII output:
```

               polar_demo: custom colors, markers, and linestyles
+--------------------------------------------------------------------------------+
|                                                                                |
| 1.0                      o                                                     |
|                    oooooooooooooooo                                            |
|                  oo                oooo                   o primary rose       |
| *               o                      ooo                # secondary petals   |
|                 o                        ooo                                   |
| 0.5             o       #############      ooooo  ooooooooooooooooooooooo      |
|                 oo   ###             ####      ooo                      oooo   |
| *                o   #                  ###                                 o  |
|                   o  ##                    #############                    oo |
| *              oooo   ##                                ######             oo  |
| 0.0      ooooooo       ###                                   ###        ooo    |
| *    ooooo               #                                     #   ooooo       |
|   ooo                  ###                                   ###oooo           |
| oo                    ##                                ######                 |
| o                    ##                    #############  o                    |
| -0.5                 #                  ###                o                   |
|  oooo                ###   ooo       ####                  oo                  |
| *   oooooooooooooooooooo#############                       o                  |
|                                  ooo                        o                  |
| *                                  ooo                      o                  |
| -1.0                                  oooo                oo                   |
| *      *       *       *      *       *   oooooooooooooooo   *       *       * |
|      -1.0             -0.5            0.0             0.5              1.0     |
+--------------------------------------------------------------------------------+
```

[Download ASCII](../../media/examples/polar_demo/polar_demo.txt)

