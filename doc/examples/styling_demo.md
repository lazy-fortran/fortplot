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
<pre><code>

                        Matplotlib-style Format Strings
+--------------------------------------------------------------------------------+
| 1.00                                                                           |
| |                                                                              |
| |                                                                              |
| |  ##        -- -                             ## ##      - -- -                |
| 0.75  #     -    -                          #       #   -                      |
| |      #  -        -   oo oo o             #          -        -               |
| |        #         oo o       o o                    #          -              |
| 0.50    -#      oo  -            oo       #          -                         |
| |  xx xx x#   o      -              oo   #          - #           -            |
| |      -  x#xx        -                o#           -  #           -           |
| |     -   o   x x     -                #o          -    #          -           |
| 0.25 -   o  #    x     -                  o       -      #          -          |
| |      o    #      xx   -              #   o     -       #           -         |
| |   - o      #        x -             #     o             #          -         |
| 0.00o         #        xx-           #        o  -         #            xx     |
| |  o          #           x          #         o-                     x        |
| |                         -x        #          - o         #         x -       |
| -0.25          #           - xx                -  o         #     xx    -      |
| |               #          -    x  #          -     oo       #  x       -      |
| |                #          -    xx                   o       xx               |
| |                #           -    # xx       -          o  xx #          -     |
| -0.50             #          -   #     xx   -           xx     #               |
| |                  #            #         xxx xx xx xxx    oo   #              |
| |                             - #          -                  o #              |
| -0.75-- sin(x) - solid line               -                    oo o            |
| |   - - cos(x) - dashed line                                      #o oo oo     |
| |    o  sin(x/2) - circles only         -                          #     #     |
| |   -x- cos(x/2) - x markers with line -                             ## #      |
| -1.00------------+-------------+--------------+-------------+--------------+-- |
|    0             2             4              6             8             10   |
+--------------------------------------------------------------------------------+
                                    X values
Y values
</code></pre>

[Download ASCII](../../media/examples/styling_demo/format_strings.txt)

[Download PDF](../../media/examples/styling_demo/format_strings.pdf)

### Line Styles

![line_styles.png](../../media/examples/styling_demo/line_styles.png)

ASCII output:
<pre><code>

                              Line Style Reference
+--------------------------------------------------------------------------------+
|                                                                                |
| |                                                                              |
| 3                                                                              |
| |         - --- -- -                                  - -- -- --               |
| |      ---          ---                             --          - --           |
| |    ---              ---                        ---               ---         |
| 2  ##                   ---                   ##-##                  ---       |
| |     ## #                 --              ## --    ##                  -      |
| |        ####                --        ####---        ###                -     |
| |           ###                 --   ###- -              ##                    |
| 1      # ###  ##             ## ##-### -            #####  ##                  |
| |    ##     #   ##          #    ##               ##     #   ##         ##     |
| |   #        #    ###      #    #  ##            #       ##    ##       #      |
| |  ##         #  % %  ### ## ##  %   #         %#%         #   %  ## #####     |
| 0   %%         # % %    ##      % %% #        %# %         ##%% %    #         |
| |    %%        %##  %% ##     %%   %  ##     %#   %%        %##  %  ##         |
| |     %       %% ##  %#       %     %  ###  ##     %        % ##  %#           |
| |      %      %    ## %      %       %    ##%       %      %    # #%           |
| -1     %%    %         %    %        %%    %         %    %        %%    %     |
| |       %%  %          %%  %%         %%  %          %%  %%         %%  %      |
| |  oo oo %% %           % %%           %% %           % %%           %% %      |
| |        oo ooo                                                                |
| -2  --- Solid (-)                                                              |
| |   - - Dashed (--)                                                     oo     |
| |   ... Dotted (:)        oo                                         oo        |
| |   -.- Dash-dot (-.)        oo o                              oo oo           |
| -3      None (invisible)         oo oo                     oo o                |
| |    o  Markers only                   oo ooo oo oo ooo oo                     |
| +--+-------------+-------------+--------------+-------------+--------------+-- |
|    0             2             4              6             8             10   |
+--------------------------------------------------------------------------------+
                                    X values
Y values
</code></pre>

[Download ASCII](../../media/examples/styling_demo/line_styles.txt)

[Download PDF](../../media/examples/styling_demo/line_styles.pdf)

### Marker Types

![marker_types.png](../../media/examples/styling_demo/marker_types.png)

ASCII output:
<pre><code>

                                All Marker Types
+--------------------------------------------------------------------------------+
| 4                                                                              |
| |                                                                              |
| |                                                                              |
| |          o       o      o                                       o  Circle    |
| |                                                                 #  Square    |
| |  o                              o                               %  Diamond   |
| 3                                                                 x  Cross     |
| |                                         o                                    |
| |  #                                                                           |
| |                                                 o                            |
| |          #                                                                   |
| 2                                                        o               #     |
| |                  #                                             o       o     |
| |  %       %                                             %                     |
| |                                                 %              #             |
| |                         #                                      %             |
| |                                                        #                     |
| 1                  %              #       #       #                            |
| |                                                        x       x             |
| |  x                                      %                                    |
| |                                                 x                      %     |
| |                         %                                              x     |
| 0          x                      %                                            |
| |                                         x                                    |
| |                                                                              |
| |                  x                                                           |
| |                                 x                                            |
| |                         x                                                    |
| -1--------+---------------+---------------+---------------+--------------+---- |
|           1               2               3               4              5     |
+--------------------------------------------------------------------------------+
                                    X Values
Y Values
</code></pre>

[Download ASCII](../../media/examples/styling_demo/marker_types.txt)

[Download PDF](../../media/examples/styling_demo/marker_types.pdf)

### Scatter Plot

![scatter_plot.png](../../media/examples/styling_demo/scatter_plot.png)

ASCII output:
<pre><code>

                           Scatter Plot with Markers
+--------------------------------------------------------------------------------+
| 1.00                                                                           |
| |                                                                              |
| |                                                                              |
| |               - o --                                   o  Data Points        |
| 0.75        - o      o - -                              --- Sin(x) Reference   |
| |         -              o                                                     |
| |         o                -                                                   |
| 0.50     -                   o                                                 |
| |      -                       -                                               |
| |      o                                                                       |
| |    -                          o                                              |
| 0.25                                                                           |
| |  o                              -                                            |
| |                                   o                                          |
| 0.00                                                                           |
| |                                     -                                        |
| |                                                                              |
| -0.25                                   o                                o     |
| |                                                                        -     |
| |                                         -                                    |
| |                                           o                          -       |
| -0.50                                                                o         |
| |                                            -                       -         |
| |                                              o                   -           |
| -0.75                                                             o            |
| |                                                - o            - -            |
| |                                                  - - o      o                |
| |                                                      -- o -                  |
| -1.00------+------------+-----------+------------+-----------+-----------+---- |
|            1            2           3            4           5           6     |
+--------------------------------------------------------------------------------+
                                    X Values
Y Values
</code></pre>

[Download ASCII](../../media/examples/styling_demo/scatter_plot.txt)

[Download PDF](../../media/examples/styling_demo/scatter_plot.pdf)

