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
| *     -- - -  -- --                                     - -- -- - --           |
| *  ---             ----                             ---  -- Solid (-)--        |
| 2--                   ---                        ---     -- Dashed (--)---     |
| *= == =                  --                 == == == ==  -- Dotted (:)    ---  |
| *      ===                 ---          === ---         =-- Dash-dot (-.)    - |
| *         ==                   - -- -===- -              -- None (invisible)   |
| 1  %% %%%%  ===             %% % %===               %%% %o Markers only      % |
| *%%       %%   ===        %%   ===%%               %      %%    ===        %%  |
| *          %      = = == %= ==      %            %%         %%     = == ==%= = |
| 0##         %%## ##    %%      ####  %         #% ##          % # ##     %     |
| * ##         #%   ##  %      ##    #  %%      #%   ##         #%%   #  %%      |
| *  #        ## %%%%#%%      ##      #   %%% %%%     ##       ##  %%% %%        |
| -1  #      ##       ##      #        #      ##       #       #        #        |
| *    #     #          #    #          #     #         #     #          #    ## |
| *     ## ##           ## ##            ## ##           ## ##            ## #   |
| oo oo oo o oo                                                                  |
| -2            oo oo                                                            |
| *                   o oo                                                   o o |
| *                        oo oo                                       oo oo     |
| -3                             o oo                           o o oo           |
| ** * * * * * * * * * * * ** * * * * oo*oo o oo*oo oo oo*o*oo o * * * * * * **  |
|0              2                4               6               8               |
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

[Download ASCII](../../media/examples/styling_demo/marker_types.txt)

[Download PDF](../../media/examples/styling_demo/marker_types.pdf)

### Scatter Plot

![scatter_plot.png](../../media/examples/styling_demo/scatter_plot.png)

ASCII output:
```

                           Scatter Plot with Markers
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

[Download ASCII](../../media/examples/styling_demo/scatter_plot.txt)

[Download PDF](../../media/examples/styling_demo/scatter_plot.pdf)

