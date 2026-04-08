title: Legend Demo
---

# Legend Demo

Source: [legend_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/legend_demo/legend_demo.f90)

Legends, labels, and legend placement.

## Files

- `legend_demo.f90` - Source code
- Generated media in `output/example/fortran/legend_demo/`

## Running

```bash
make example ARGS="legend_demo"
```

## Output

### Basic Legend

![basic_legend.png](../../media/examples/legend_demo/basic_legend.png)

ASCII output:
```

                               Basic Legend Demo
+--------------------------------------------------------------------------------+
| 1.0                                                                            |
| *                                                                              |
|  = =     - -- --                               == ==       - -- -              |
|     =  -                                     =       =    -       --- sin(x)   |
| *     -          -                          =         = -          -- cos(x)   |
| 0.5  -=           -                       =           -                        |
|     -  =           -                     =            - =            -         |
| *  -    =           -                   =            -   ==          -         |
|    -     =           --                 =           -     =           -        |
| * -       =                            =           -       =           -       |
|  -         =          -               =            -        =           -      |
| 0.0        =           -             =            -          =           -     |
| *           =           -           =            -            =          -     |
|              =           -          =           -                         -    |
|               =          -         =           -              =            -   |
| *             =           -       =            -               =            -  |
|                =           -     =            -                 =            - |
| -0.5            =           -   ==           -                   ==            |
|                  =           -  =           -                     =            |
|                                =            -                      =           |
| *                 =          = -          -                                    |
|                     =       =    -      -                            =         |
| ** * * * * * * * * * *=* ** * * * * *-*-* * * * * ** * * * * * * * * *=*=* **= |
|0              2                4               6               8               |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download ASCII](../../media/examples/legend_demo/basic_legend.txt)

[Download PDF](../../media/examples/legend_demo/basic_legend.pdf)

### Legend Box Default

![legend_box_default.png](../../media/examples/legend_demo/legend_box_default.png)

ASCII output:
```

                            Legend Box Styling Demo
+--------------------------------------------------------------------------------+
|                                                                                |
| *                 -                                                            |
|               -- -  - --                                                       |
|             -            --                                   -- sin(x)        |
| *          -                -                                 -- 0.5 sin(x)    |
| 0.5%%    -                   -                                -- 0.7 cos(x)% % |
|       %%                       -                              -- -0.3 sin(x)   |
| *     -  % %  == == = == =       -                                %%           |
|     -    = =%             = =                                   %              |
| *  -  ==      %              = =  -               ## # ## ## #% #              |
|     =          %                 == -        # ##            %    ## ##        |
| 0.0=             %                  =-  # # #              %            ##     |
| %#                %                  # #                  %                # # |
|    ##               %            ## #  -=               %                = =   |
|       ## # #          %     ## #        - = =          %                =      |
| *           # ## ## # #% %#               -  = =   % %               ==  -     |
|                           %                     = %             = ==    -      |
| -0.5                        %               -   %  = = == == ==       -        |
|                              % %             % %                     -         |
|                                  %% %% %% % %  -                   -           |
| *                                               -                 -            |
|                                                   --            -              |
| * *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * -* -* -*  *  *  *  *  * |
|0           1           2            3            4            5           6    |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download ASCII](../../media/examples/legend_demo/legend_box_default.txt)

[Download PDF](../../media/examples/legend_demo/legend_box_default.pdf)

### Legend Box Lower Right

![legend_box_lower_right.png](../../media/examples/legend_demo/legend_box_lower_right.png)

ASCII output:
```

                            Legend Box Styling Demo
+--------------------------------------------------------------------------------+
|                                                                                |
| *                 -                                                            |
|               -- -  - --                                                       |
|             -            --                                                    |
| *          -                -                                                  |
| 0.5%%    -                   -                                          %% % % |
|       %%                       -                                     %%        |
| *     -  % %  == == = == =       -                                %%           |
|     -    = =%             = =                                   %              |
| *  -  ==      %              = =  -               ## # ## ## #% #              |
|     =          %                 == -        # ##            %    ## ##        |
| 0.0=             %                  =-  # # #              %            ##     |
| %#                %                  # #                  %                # # |
|    ##               %            ## #  -=               %                = =   |
|       ## # #          %     ## #        - = =          %                =      |
| *           # ## ## # #% %#               -  = =   % %               ==  -     |
|                           %                     = %             = ==    -      |
| -0.5                        %               -   %  = = == == ==       -        |
|                              % %             % %                     -         |
|                                  %% %% %% % %  -              -- sin(x)        |
| *                                               -             -- 0.5 sin(x)    |
|                                                   --          -- 0.7 cos(x)    |
| * *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * -* -* --- -0.3 sin(x) * |
|0           1           2            3            4            5           6    |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download ASCII](../../media/examples/legend_demo/legend_box_lower_right.txt)

[Download PDF](../../media/examples/legend_demo/legend_box_lower_right.pdf)

### Legend Box Upper Left

![legend_box_upper_left.png](../../media/examples/legend_demo/legend_box_upper_left.png)

ASCII output:
```

                            Legend Box Styling Demo
+--------------------------------------------------------------------------------+
|                                                                                |
| *                 -                                                            |
|               -- -  - --                                                       |
|  -- sin(x)  -            --                                                    |
| *-- 0.5 sin(x)              -                                                  |
| 0.5%%    -                   -                                          %% % % |
|  -- 0.7 cos(x)                 -                                     %%        |
| *-- -0.3 sin(x)= == = == =       -                                %%           |
|     -    = =%             = =                                   %              |
| *  -  ==      %              = =  -               ## # ## ## #% #              |
|     =          %                 == -        # ##            %    ## ##        |
| 0.0=             %                  =-  # # #              %            ##     |
| %#                %                  # #                  %                # # |
|    ##               %            ## #  -=               %                = =   |
|       ## # #          %     ## #        - = =          %                =      |
| *           # ## ## # #% %#               -  = =   % %               ==  -     |
|                           %                     = %             = ==    -      |
| -0.5                        %               -   %  = = == == ==       -        |
|                              % %             % %                     -         |
|                                  %% %% %% % %  -                   -           |
| *                                               -                 -            |
|                                                   --            -              |
| * *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * -* -* -*  *  *  *  *  * |
|0           1           2            3            4            5           6    |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download ASCII](../../media/examples/legend_demo/legend_box_upper_left.txt)

[Download PDF](../../media/examples/legend_demo/legend_box_upper_left.pdf)

### Legend Lower Left

![legend_lower_left.png](../../media/examples/legend_demo/legend_lower_left.png)

ASCII output:
```

                               Legend: Lower Left
+--------------------------------------------------------------------------------+
|                                                                                |
| *                                                                           -  |
| 4                                                                    --------  |
| *                                                             -------          |
| *                                                      -------                 |
| *                                                -------                       |
| *                                          ------                              |
| 3                                     -----                                    |
| *                               ------                                      =  |
|                            -----                            ================   |
| *                     -----                     ============                   |
| *                 ----                ==========                               |
| 2             ----            =========                                        |
| *          ---          ======                                                 |
| *       ---       ======                                                       |
| *    ---      ====                                                             |
|    --      ===                                                                 |
| *--     ===                                                                    |
| 1    ===                                                                       |
| *   =                                                                          |
| *  =                                                                           |
| *-- U+221Ax                                                                    |
| *-- ln(x)* * ***************************************************************** |
|                5                    10                   15                 20 |
+--------------------------------------------------------------------------------+
```

[Download ASCII](../../media/examples/legend_demo/legend_lower_left.txt)

[Download PDF](../../media/examples/legend_demo/legend_lower_left.pdf)

### Legend Lower Right

![legend_lower_right.png](../../media/examples/legend_demo/legend_lower_right.png)

ASCII output:
```

                              Legend: Lower Right
+--------------------------------------------------------------------------------+
|                                                                                |
| *                                                                           -  |
| 4                                                                    --------  |
| *                                                             -------          |
| *                                                      -------                 |
| *                                                -------                       |
| *                                          ------                              |
| 3                                     -----                                    |
| *                               ------                                      =  |
|                            -----                            ================   |
| *                     -----                     ============                   |
| *                 ----                ==========                               |
| 2             ----            =========                                        |
| *          ---          ======                                                 |
| *       ---       ======                                                       |
| *    ---      ====                                                             |
|    --      ===                                                                 |
| *--     ===                                                                    |
| 1    ===                                                                       |
| *   =                                                                          |
| *  =                                                                           |
| *==                                                                 -- U+221Ax |
| ********** * *******************************************************-- ln(x)** |
|                5                    10                   15                 20 |
+--------------------------------------------------------------------------------+
```

[Download ASCII](../../media/examples/legend_demo/legend_lower_right.txt)

[Download PDF](../../media/examples/legend_demo/legend_lower_right.pdf)

### Legend Upper Left

![legend_upper_left.png](../../media/examples/legend_demo/legend_upper_left.png)

ASCII output:
```

                               Legend: Upper Left
+--------------------------------------------------------------------------------+
|                                                                                |
| *                                                                           -  |
| 4                                                                    --------  |
| *-- U+221Ax                                                   -------          |
| *-- ln(x)                                              -------                 |
| *                                                -------                       |
| *                                          ------                              |
| 3                                     -----                                    |
| *                               ------                                      =  |
|                            -----                            ================   |
| *                     -----                     ============                   |
| *                 ----                ==========                               |
| 2             ----            =========                                        |
| *          ---          ======                                                 |
| *       ---       ======                                                       |
| *    ---      ====                                                             |
|    --      ===                                                                 |
| *--     ===                                                                    |
| 1    ===                                                                       |
| *   =                                                                          |
| *  =                                                                           |
| *==                                                                            |
| ********** * ***************************************************************** |
|                5                    10                   15                 20 |
+--------------------------------------------------------------------------------+
```

[Download ASCII](../../media/examples/legend_demo/legend_upper_left.txt)

[Download PDF](../../media/examples/legend_demo/legend_upper_left.pdf)

### Legend Upper Right

![legend_upper_right.png](../../media/examples/legend_demo/legend_upper_right.png)

ASCII output:
```

                              Legend: Upper Right
+--------------------------------------------------------------------------------+
|                                                                                |
| *                                                                           -  |
| 4                                                                    --------  |
| *                                                             -------- U+221Ax |
| *                                                      -------      -- ln(x)   |
| *                                                -------                       |
| *                                          ------                              |
| 3                                     -----                                    |
| *                               ------                                      =  |
|                            -----                            ================   |
| *                     -----                     ============                   |
| *                 ----                ==========                               |
| 2             ----            =========                                        |
| *          ---          ======                                                 |
| *       ---       ======                                                       |
| *    ---      ====                                                             |
|    --      ===                                                                 |
| *--     ===                                                                    |
| 1    ===                                                                       |
| *   =                                                                          |
| *  =                                                                           |
| *==                                                                            |
| ********** * ***************************************************************** |
|                5                    10                   15                 20 |
+--------------------------------------------------------------------------------+
```

[Download ASCII](../../media/examples/legend_demo/legend_upper_right.txt)

[Download PDF](../../media/examples/legend_demo/legend_upper_right.pdf)

### Multi Function Legend

![multi_function_legend.png](../../media/examples/legend_demo/multi_function_legend.png)

ASCII output:
```

                       Mathematical Functions with Legend
+--------------------------------------------------------------------------------+
|                                                                                |
| 1.0                    =                                                       |
|                 ======= ========                                               |
| %%%           ==                =====                    -- $e^{-x/2}cos(x)$   |
| 0.8%%%      ==                       ====                -- $xe^{-x/3}$        |
| *-    %%  ==                             ====            -- $sin(x)/x$         |
|   -    %%==                                  ===         -- $x^{2}e^{-x}$      |
|    -    =%%                                     ====                           |
| 0.6-   =  %%                                        ====                       |
|     - =     %                                           ====                   |
|      =       %%###                                         =====               |
|     = -   ####%   ####                                          =====          |
| 0.4=     ##    %%     ###                                            =====     |
|    =   ##        %       ####                                            ===== |
|       ##         %%         ####                                               |
| 0.2  #  -          %            ####                                           |
|  = ##    -          %%              #####                                      |
| =  #      -          %                   ######       %%%%%%%%%%%%%            |
|  ##        -          %%              ---------###%%%%#########    %%%%%       |
| 0.0         -           %         -----        %%%     -------##########%%%%%# |
|              --          %%%  ----          %%%                              % |
|               --           -%%          %%%%                                   |
| -0.2 * * * * * *-*-*-*-*-** * *%*%*%*%*%* * * * * ** * * * * * * * * * * * **  |
|0              2               4               6                8               |
+--------------------------------------------------------------------------------+
                                       x
f(x)
```

[Download ASCII](../../media/examples/legend_demo/multi_function_legend.txt)

[Download PDF](../../media/examples/legend_demo/multi_function_legend.pdf)

