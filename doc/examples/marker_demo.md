title: Marker Demo
---

# Marker Demo

Source: [marker_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/marker_demo/marker_demo.f90)

Marker styles and marker sizing.

## Files

- `marker_demo.f90` - Source code
- Generated media in `output/example/fortran/marker_demo/`

## Running

```bash
make example ARGS="marker_demo"
```

## Output

### All Marker Types

![all_marker_types.png](../../media/examples/marker_demo/all_marker_types.png)

ASCII output:
```

                                All Marker Types
+--------------------------------------------------------------------------------+
|                                                                                |
| *                o                                                             |
|         o                o                                                     |
| *                                 o                                o Circle    |
| 3                                                                  # Square    |
| *                                         o                        % Diamond   |
| #                                                                  x Cross     |
| *                                                  o                           |
| *       #                                                                      |
| 2                                                          o                 # |
| *       %        #                                         %        o        o |
| %                                                  %                #          |
| *                        #                                          %          |
| 1                                 #                        #                   |
| *                %                        #        #       x                   |
| x                                         %                         x          |
| *                                                  x                         % |
| *                        %                                                   x |
| 0       x                         %                                            |
| *                                         x                                    |
| *                                                                              |
| *                x                                                             |
| *   *   *   *   *   *   *x  *   * x *   *   *   *   *   *   *   *   *   *   *  |
|        1                 2                3                 4                5 |
+--------------------------------------------------------------------------------+
                                    X Values
Y Values
```

[Download ASCII](../../media/examples/marker_demo/all_marker_types.txt)

[Download PDF](../../media/examples/marker_demo/all_marker_types.pdf)

### Marker Colors

![marker_colors.png](../../media/examples/marker_demo/marker_colors.png)

ASCII output:
```

                            Marker Colors and Styles
+--------------------------------------------------------------------------------+
|                                                                                |
| 3.0                                                                            |
|                                                                                |
| *                                                          o Blue circles      |
|                                                            # Green squares     |
| 2.5        o                                               % Orange diamonds   |
|                                                                              o |
| *                    o                                            o            |
| *          #                                          o                        |
| 2.0                  #          o           o                                  |
| #                               #                                              |
|                                                                                |
| *                                                                              |
| 1.5                                         #                                  |
| *                                                                              |
| *                                                     #                        |
| %          %                                                      #          # |
| 1.0                                                                            |
|                                                                                |
| *                    %                                                         |
|                                                                   %          % |
| 0.5                                                                            |
| *   *    *   *    *   *    *   *%   *   *   %*   *    %   *    *   *    *    * |
|       1                  2                  3                  4               |
+--------------------------------------------------------------------------------+
                                   X Position
Y Position
```

[Download ASCII](../../media/examples/marker_demo/marker_colors.txt)

[Download PDF](../../media/examples/marker_demo/marker_colors.pdf)

### Scatter Plot

![scatter_plot.png](../../media/examples/marker_demo/scatter_plot.png)

ASCII output:
```

                     Scatter Plot with Antialiased Markers
+--------------------------------------------------------------------------------+
|                                                                                |
| *               =                                                              |
|             = = o = o =                                                        |
|           = o           o                                o Data Points         |
| *       o                 =                              -- Sin(x) Reference   |
| 0.5   =                     o                                                  |
|     =                         =                                                |
| * = o                                                                          |
|                                 o                                              |
| *                                 =                                            |
| o                                                                              |
| 0.0                                 o                                          |
| *                                     =                                        |
|                                                                                |
|                                         o                                    o |
| *                                         =                                  = |
|                                             o                             =    |
| -0.5                                        =                           o      |
|                                               =                         =      |
|                                                 o                   o =        |
| *                                                 =                 =          |
|                                                     o           o =            |
| *  *  *   *  *  *   *  *  *   *  *  *   *  *  *   *  *= o = o =*= *   *  *   * |
|         1             2             3             4             5            6 |
+--------------------------------------------------------------------------------+
                                    X Values
Y Values
```

[Download ASCII](../../media/examples/marker_demo/scatter_plot.txt)

[Download PDF](../../media/examples/marker_demo/scatter_plot.pdf)

