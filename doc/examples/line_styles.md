title: Line Styles
---

# Line Styles

Source: [line_styles.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/line_styles/line_styles.f90)

Line styles, dash patterns, and markers.

## Files

- `line_styles.f90` - Source code
- Generated media in `output/example/fortran/line_styles/`

## Running

```bash
make example ARGS="line_styles"
```

## Output

### Line Styles

![line_styles.png](../../media/examples/line_styles/line_styles.png)

ASCII output:
```

                         Complete Line Style Reference
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

[Download ASCII](../../media/examples/line_styles/line_styles.txt)

[Download PDF](../../media/examples/line_styles/line_styles.pdf)

