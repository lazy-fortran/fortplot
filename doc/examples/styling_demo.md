title: Styling Demo
---

# Styling Demo

Source: [styling_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/styling_demo/styling_demo.f90)

Line styles, markers, format strings, and scatter styling.

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
| |                                                                              |
|                                                                                |
|                                                                                |
| |                                                                              |
|                                                                                |
|                                                                                |
| |                                                                              |
| 0.5                                                                            |
|    ## #     --- --                            ## ##     -- -- -                |
| |      #  -        -o ooo oo oo            ##       ##-        --              |
|    xx xx-#    o oo o--          ooo o     #         --#           -            |
|        - xx#xxx       -              o oo#          -  ##          -           |
| |    --  oo #   xx x   -               #  oo      --     #          -          |
| 0.0 - oo     #      x xx-            ##     o o  -        #          -   x     |
|    oo         #         x-xx         #         o-o         #         xx-x      |
| |              ##          - xx    ##          -  o o       ##  x xx    -      |
|                  #         --   xxx x        --      oo     x xx        --     |
|                  ##          -   #   x xx xx-        xx xx x  ##               |
| |                  #          - #          -x xx xx x       o oo#              |
|                     # #       # --      - -                     o oo oo oo     |
| -0.5                   ## ## #    - -- -                           # ## ##     |
| |                                                                              |
|                                                                                |
|  x cos(x/2) - x markers with line                                              |
| |-- sin(x) - solid line                                                        |
|  -- cos(x) - dashed line                                                       |
| -1.0                                                                           |
| |o sin(x/2) - circles only- -- - - -- - - -- - - -- - - -- - - -- - -- - - --  |
|    0             2             4              6             8             10   |
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
| |                                                                              |
| |                                                        -- Solid (-)          |
| |                                                        -- Dashed (--)        |
| |                                                        -- Dotted (:)         |
| |                                                        -- Dash-dot (-.)      |
| |                                                        -- None (invisible)   |
| |                                                        o Markers only        |
| |       --- --- -- --                                -- -- -- ---              |
| |   ----             ----                        ----             ----         |
| |  ## ## ##              -----           ####-##-## ####              ----     |
| |      # # ####              -# #-- -#### ---        ## ####                   |
| |   ####  ###  #####       ###  #####            ####  ###  #####       ##     |
| |  ##        ##  % %# ###### ## #%% ##         ###        ##  %%# ## #####     |
| |    %%        ### %%%###     %%   %% ##     ### %%%        ### %%%###         |
| |     %%     %%   ### #%     %%     %%  #####      %%     %%   ## ##           |
| |       %%  %%         %%  %%         %%  %%         %%  %%         %%  %%     |
| |1 oo oo oo ooo oo        %             %               %             %        |
| |                  oo ooo o                                           o oo     |
| |                          o oo ooo                           ooo oo o         |
| |                                   oo oo ooo oo oo ooo oo oo                  |
| |2                                                                             |
| |                                                                              |
| |                                                                              |
| |                                                                              |
| |                                                                              |
| |                                                                              |
| |- - - -- - - -- - - -- - - -- - - -- - - -- - - -- - - -- - - -- - -- - - --  |
| -3 0             2             4              6             8             10   |
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
| 4                                                                              |
| |                                                                              |
|                                                                                |
| |                                                                  o Circle    |
| |                                                                  # Square    |
| |                                                                  % Diamond   |
| |                                                                  x Cross     |
| 3                                                                              |
| |                                                                              |
| |                                                                              |
| |                                                                              |
| |                                                                              |
| 2                                                                              |
| |          o       o      o                                                    |
| |  o                              o                                            |
| |  #                                      o                                    |
| |          #                                      o                            |
| |                  #                                     o       o       #     |
| 1  %       %                                      %      %       #             |
| |                         #       #                      #       %             |
| |                  %                      #       #      x       x             |
| |  x                                      %       x                      %     |
| |          x              %       %                                      x     |
| 0                                         x                                    |
| |                  x              x                                            |
| |                         x                                                    |
| |                                                                              |
| |                                                                              |
| |  -   -   -   -   -   -  -   -   -   -   -   -   -  -   -   -   -   -   -  -  |
| -1        1               2               3               4              5     |
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
| 1.0                                                                            |
|                                                                                |
| |                                                                              |
|                                                          o Data Points         |
|                                                          -- Sin(x) Reference   |
| |                                                                              |
|                                                                                |
| 0.5                                                                            |
| |                                                                              |
|               # # # ## #                                                       |
| |         # # o          # #                                                   |
|          #o                  #                                                 |
|      # #                       ##                                              |
|    #                              #                                            |
| |  o                                #                                          |
| 0.0                                   #                                        |
|                                         #                                o     |
| |                                         #                              #     |
|                                             ##                       # #       |
|                                                #                  o#           |
| |                                                # #          o # #            |
| -0.5                                                 # ## # # #                |
|                                                                                |
| |                                                                              |
|                                                                                |
|                                                                                |
| |                                                                              |
|                                                                                |
| | -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - |
| -1.0       1            2           3            4           5           6     |
+--------------------------------------------------------------------------------+
                                    X Values
Y Values
```

[Download ASCII](../../media/examples/styling_demo/scatter_plot.txt)

[Download PDF](../../media/examples/styling_demo/scatter_plot.pdf)

