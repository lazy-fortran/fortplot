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
| %                                                                              |
|  #       - -- -                                ## ##       - -- -              |
| 0.8#           -                            -- sin(x) - solid line-            |
|     #  -         -    oo oo o               -- cos(x) - dashed line            |
| *     -          oo o        o o            o sin(x/2) - circles only          |
|      -#        o  -              oo       # x cos(x/2) - x markers with line   |
| xx xx  #    o o    -                o                 - #            -         |
| 0.5   xx x o        -                o   #           -   #           -         |
|    -    #o xx        -                 oo                 #           -        |
| 0.2-   o #    x       -                 # o         -     #                    |
|   -   o   #    x x                     #    o      -                   -       |
| *-  o             x   -               #      o     -       #            -      |
|    o       #        x  -                       o  -         #            -   x |
| 0.0        #          xx-            #                       #             x   |
| o           #            x          #           o-                       x     |
|                          -x                     - o           #         x -    |
|              #           -  x       #          -   o          #      xx    -   |
| -0.2          #           -  x x   #                 o         #   x           |
|               #                  x#            -      o           x         -  |
|                            -     #x x         -         o     x x            - |
|                #            -        x x     -            ox x   #             |
| -0.5            #            -  ##      x x -           x xo o    #            |
|                  #           -  #           xx xx xx xx       o   #            |
|                                #            -                   o o#           |
| -0.8              #            -          -                        o o         |
|                              #                                       #o oo o o |
|                     #       #    -      -                             #        |
| ** * * * * * * * * * *#% %% * * * * *-*-* * * * * ** * * * * * * * * * *#% %*# |
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
| 3           -                                                                  |
| *      - - -  -- -                                      - -- -- - -            |
| *   ---           ---                                --  -- Solid (-)-         |
| * --                 --                            --    -- Dashed (--)--      |
| 2#                     --                       #-#-     -- Dotted (:)   -     |
| *  ##                    --                 ## #-  # ##  -- Dash-dot (-.) ---  |
| *     ###                  ---          ### ---         #-- None (invisible) - |
| *        ##                  - -      ### - -            o Markers only        |
| *   # #    ##                # # -- ## --             # #   ###                |
| 1  #   ###   ##             ##   ####               ##   ##   ##             # |
| * #       #    ###        ##    ###                #      ##    ###         #  |
| %#         #      #      #   # #   ##             #         #     ## #     #   |
| %          ##  % %  # ##### #   %%  #           %#%          ##   %   # #### # |
| 0%%          #%   %    #       %  %% ##        ##  %          #%%  %     #     |
| * %          %#   %%  #      %%    %   #      %#   %%         %#    %  ##      |
| *  %         % ##  %###      %      %   #    ##     %         % ###  ##        |
| *   %       %    ## %       %        %   ## #%       %       %     # #%        |
| -1  %%     %         %     %%        %      %        %%     %%        %        |
| *    %     %          %    %          %     %         %     %          %    %% |
| *     %% %%           %% %%            %% %%           %% %%            %% %   |
| oo oo oo o               %              %                 %              %     |
| *          oo o                                                                |
| -2             o oo                                                            |
| *                   o oo                                                   o o |
| *                        oo                                           o oo     |
| *                           oo o                                   o o         |
| -3                               oo oo                       oo o o            |
| ** * * * * * * * * * * * ** * * * * * *oo o oo*oo oo oo*o*oo * * * * * * * **  |
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
| *                o                                                             |
|         o                o                                                     |
| *                                                                  o Circle    |
| 3.2                               o                                # Square    |
|                                                                    % Diamond   |
| *                                         o                        x Cross     |
| *                                                                              |
| #                                                                              |
| 2.4     #                                          o                           |
| *                                                                              |
| *                                                          o                 # |
| *                #                                         %        o        o |
| 1.6     %                                                           #          |
| *                                                  %                           |
| *                        #                                          %          |
| *                                                          #                   |
|                  %                #                #                           |
| 0.8                                       #                x        x          |
| x                                         %                                    |
| *                                                  x                           |
|                                                                              x |
| *                        %                                                     |
| 0.0     x                         %                                            |
| *                                         x                                    |
|                                                                                |
| *                                                                              |
| *                x                                                             |
| -0.8*   *   *   *   *   *x  *   * x *   *   *   *   *   *   *   *   *   *   *  |
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
| *               #                                                              |
|             # # o # #                                                          |
| 0.8       # o         # #                                o Data Points         |
|         #               o                                -- Sin(x) Reference   |
| *       o                 #                                                    |
| 0.5   #                     #                                                  |
|     #                                                                          |
| *   o                         #                                                |
|   #                             #                                              |
| 0.2                                                                            |
| #                                 #                                            |
| o                                                                              |
|                                     #                                          |
| 0.0                                                                            |
| *                                     #                                        |
|                                                                                |
|                                         #                                    o |
| -0.2                                                                           |
|                                           #                                  # |
|                                                                                |
|                                             #                           o #    |
| -0.5                                                                    #      |
|                                               #                                |
|                                                 #                   o #        |
| -0.8                                                                #          |
|                                                   # o                          |
|                                                     #           o #            |
| *  *  *   *  *  *   *  *  *   *  *  *   *  *  *   *  *# # # # #*# *   *  *   * |
|      0.8        1.6        2.4         3.2        4.0        4.8        5.6    |
+--------------------------------------------------------------------------------+
                                    X Values
Y Values
```

[Download ASCII](../../media/examples/styling_demo/scatter_plot.txt)

[Download PDF](../../media/examples/styling_demo/scatter_plot.pdf)

