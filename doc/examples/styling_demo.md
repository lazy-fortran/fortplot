title: Styling Demo
---

# Styling Demo

Source: [styling_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/styling_demo/styling_demo.f90)

See source and outputs below.

## Files

- `styling_demo.f90` - Source code
- Generated media in `output/example/fortran/styling_demo/`

## Running

```bash
make example ARGS="styling_demo"
```

## Output

### Format Strings

![format_strings.png](../../media/examples/styling_demo/format_strings.png)

ASCII output:
```

                        Matplotlib-style Format Strings
+--------------------------------------------------------------------------------+
| 1.0                                                                            |
| *                                                                              |
|                                                                                |
| 0.8                                         -- sin(x) - solid line             |
|                                             -- cos(x) - dashed line            |
| *                                           o sin(x/2) - circles only          |
|                                             x cos(x/2) - x markers with line   |
| 0.5                                                                            |
| %#         -- -                                 # #        - --                |
|    ##  - -     -                             # #   # #    -     - -            |
| 0.2   #        o oo o oo oo oo o o          #         # -          -           |
| xx xx-xx    o o    -              o oo   ##           - ##           -         |
| *  -   o#x xx x     --                 oo           --    #          --        |
|   - o o  ##    x x    -                #  o o      -      ##           --      |
| 0.0o       #      x x x-             ##      o o  --        #            -   x |
| o          ##          x-x          #           o-o          ##         xx x   |
|              #           -x xx     ##          --  o o        #      xx   --   |
|               #           --   x xx            -      o o      #x xx        -- |
| -0.2           ##           -   ##  xx xx    --           xx xx  #             |
|                  #           - ##         x xx xx xx xx x    oo o #            |
|                   #          # -          -                       oo oo oo o o |
|                     # #     #    --    --                             #      # |
| -0.5                   # ##         --                                  ## #   |
|                                                                                |
|                                                                                |
| -0.8                                                                           |
|                                                                                |
|                                                                                |
| ** * * * * * * * * * * * ** * * * * * * * * * * * ** * * * * * * * * * * * **  |
|0          2           3            5           6           8           9       |
+--------------------------------------------------------------------------------+
                                    X values
Y values
```

[Download ASCII](../../media/examples/styling_demo/format_strings.txt)

[Download PDF](../../media/examples/styling_demo/format_strings.pdf)

### Line Styles

![line_styles.png](../../media/examples/styling_demo/line_styles.png)

ASCII output:
```

                              Line Style Reference
+--------------------------------------------------------------------------------+
|                                                                                |
| 3                                                                              |
| *                                                                              |
| *                                                        -- Solid (-)          |
| *                                                        -- Dashed (--)        |
| 2                                                        -- Dotted (:)         |
| *                                                        -- Dash-dot (-.)      |
| *                                                        -- None (invisible)   |
| *        - -- --                                         o Markers only        |
| *  -----         -----                              --- -         -- --        |
| 1#-#                  ----                   # ##-## #                 ----    |
| *   # ####                ----          ### #---      # ###                --- |
| *  ## ##  ####               # # #- ####- -          ## ## ####                |
| *###    ####  ####       ####  #####               ##     ###  #####       ### |
| 0%         ## %% %# # ##### ## %%%% ##         %###%         ## % %% ## #### # |
| * %%         ###  %% ###     %%    %% ###     ##   %%         ###   %%####     |
| *  %%       %   ###%#       %%      %%  ### ##      %%       %%  ### #%        |
| *    %%   %%         %%   %%          %%   %%         %%   %%         %%%   %% |
| -1 oo oo o oo          % %              % %             % %              % %   |
| *             oo oo o o                                                      o |
| *                      o oo oo                                       oo oo o   |
| *                              o oo oo oo                 oo oo o oo           |
| *                                         o oo oo oo oo o                      |
| -2                                                                             |
| *                                                                              |
| *                                                                              |
| *                                                                              |
| -3                                                                             |
| ** * * * * * * * * * * * ** * * * * * * * * * * * ** * * * * * * * * * * * **  |
|0          2           3            5           6           8           9       |
+--------------------------------------------------------------------------------+
                                    X values
Y values
```

[Download ASCII](../../media/examples/styling_demo/line_styles.txt)

[Download PDF](../../media/examples/styling_demo/line_styles.pdf)

### Marker Types

![marker_types.png](../../media/examples/styling_demo/marker_types.png)

ASCII output:
```

                                All Marker Types
+--------------------------------------------------------------------------------+
|                                                                                |
| *                                                                              |
|                                                                                |
| *                                                                  o Circle    |
| 3.2                                                                # Square    |
|                                                                    % Diamond   |
| *                                                                  x Cross     |
| *                                                                              |
| *                                                                              |
| 2.4                                                                            |
| *                                                                              |
| *                                                                              |
| *                o                                                             |
| 1.6     o                o        o                                            |
| o                                                                              |
| #                                         o                                    |
| *       #                                          o                           |
|                                                            o                 # |
| 0.8     %        #                                         %        #        o |
| *                        #                         %       #        %          |
| *                %                #       #        #                           |
| x                                         %                x        x          |
| *                        %                         x                         x |
| 0.0     x                         %                                            |
| *                                         x                                    |
|                  x                x                                            |
| *                        x                                                     |
| *                                                                              |
| -0.8*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  |
| 0.6       1.2        1.8        2.4       3.0        3.6        4.2       4.8  |
+--------------------------------------------------------------------------------+
                                    X Values
Y Values
```

[Download ASCII](../../media/examples/styling_demo/marker_types.txt)

[Download PDF](../../media/examples/styling_demo/marker_types.pdf)

### Scatter Plot

![scatter_plot.png](../../media/examples/styling_demo/scatter_plot.png)

ASCII output:
```

                           Scatter Plot with Markers
+--------------------------------------------------------------------------------+
|                                                                                |
| *                                                                              |
|                                                                                |
| 0.8                                                      o Data Points         |
|                                                          -- Sin(x) Reference   |
| *                                                                              |
| 0.5                                                                            |
|                                                                                |
| *             # # # #                                                          |
|           # #   o   o # #                                                      |
| 0.2   # #                 # #                                                  |
|     #                       o #                                                |
| * # o                           #                                              |
| #                                 #                                            |
| 0.0                                 #                                          |
| *                                     #                                        |
|                                         #                                    o |
|                                           #                                  # |
| -0.2                                        #                           o #    |
|                                               # o                     # #      |
|                                                 # #                 #          |
|                                                     # # o   o   # #            |
| -0.5                                                    # # # #                |
|                                                                                |
|                                                                                |
| -0.8                                                                           |
|                                                                                |
|                                                                                |
| *  *  *   *  *  *   *  *  *   *  *  *   *  *  *   *  *  *   *  *  *   *  *   * |
|      0.8        1.6        2.4         3.2        4.0        4.8        5.6    |
+--------------------------------------------------------------------------------+
                                    X Values
Y Values
```

[Download ASCII](../../media/examples/styling_demo/scatter_plot.txt)

[Download PDF](../../media/examples/styling_demo/scatter_plot.pdf)

