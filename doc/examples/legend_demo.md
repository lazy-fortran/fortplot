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
<pre><code>

                               Basic Legend Demo
+--------------------------------------------------------------------------------+
| 1.00                                                                           |
| |                                                                              |
| |                                                                              |
| |  ##        -- -                             ## ##      - -- -   --- sin(x)   |
| 0.75  #     -    -                          #       #   -         --- cos(x)   |
| |      #  -        -                       #          -        -               |
| |        #                                           #          -              |
| 0.50    -#          -                     #          -                         |
| |      -  #          -                   #          - #           -            |
| |      -   #          -                 #           -  #           -           |
| |     -               -                #           -    #          -           |
| 0.25 -      #          -                          -      #          -          |
| |           #           -              #         -       #           -         |
| |   -        #          -             #                   #          -         |
| 0.00-         #          -           #           -         #                   |
| |  -          #                      #          -                     -        |
| |                         -         #          -           #           -       |
| -0.25          #           -                   -            #           -      |
| |               #          -       #          -              #          -      |
| |                #          -     #                           #                |
| |                #           -    #          -                #          -     |
| -0.50             #          -   #          -                  #               |
| |                  #            #           -                   #              |
| |                             - #          -                    #              |
| -0.75               #           -         -                                    |
| |                             #                                   #            |
| |                     #      #   -      -                          #     #     |
| |                      ## ##      - -- -                             ## #      |
| -1.00------------+-------------+--------------+-------------+--------------+-- |
|    0             2             4              6             8             10   |
+--------------------------------------------------------------------------------+
                                       x
y
</code></pre>

[Download ASCII](../../media/examples/legend_demo/basic_legend.txt)

[Download PDF](../../media/examples/legend_demo/basic_legend.pdf)

### Legend Box Default

![legend_box_default.png](../../media/examples/legend_demo/legend_box_default.png)

ASCII output:
<pre><code>

                            Legend Box Styling Demo
+--------------------------------------------------------------------------------+
| 1.00                                                                           |
| |                                                                              |
| |                                                                              |
| |                - -- ---                                                      |
| 0.75          - -         -                                                    |
| |            -             -                                                   |
| |  ##       -                -                                          ##     |
| 0.50  ##                      -                                      ##        |
| |        ##                     -                                 ##           |
| |        -  #   ## ## ###                                       #              |
| |      -     ##           ## #   -                             #               |
| 0.25      # # #               #   -                  %% %% %  #                |
| |     -  #      #               ##             % %% %       # %%%              |
| |   - ##         #                # -      %% %            #      %%           |
| 0.00#              #                #-  % %              #           %%        |
| |  #%               #                # #                #               %#     |
| |     %%              #           % %  -#             #                 #      |
| -0.25    %%            #      % %%      - #          #               ## -      |
| |           %%% %       # %% %             ##       #              #  -        |
| |                % %% %%  #               -   #   #             # #            |
| |                          #               -   # ##           ##     -         |
| -0.50                        #                 #    ### ## ##      -           |
| |                             # #           # #                   -            |
| |                                ## #   # ##  -                                |
| -0.75-- sin(x)                       # #       -                -              |
| |   --- 0.5 sin(x)                               -             -               |
| |   --- 0.7 cos(x)                                -         - -                |
| |   --- -0.3 sin(x)                                 --- -- -                   |
| -1.00---------+----------+----------+-----------+----------+----------+------- |
|    0          1          2          3           4          5          6        |
+--------------------------------------------------------------------------------+
                                       x
y
</code></pre>

[Download ASCII](../../media/examples/legend_demo/legend_box_default.txt)

[Download PDF](../../media/examples/legend_demo/legend_box_default.pdf)

### Legend Box Lower Right

![legend_box_lower_right.png](../../media/examples/legend_demo/legend_box_lower_right.png)

ASCII output:
<pre><code>

                            Legend Box Styling Demo
+--------------------------------------------------------------------------------+
| 1.00                                                                           |
| |                                                                              |
| |                                                                              |
| |                - -- ---                                                      |
| 0.75          - -         -                                                    |
| |            -             -                                                   |
| |  ##       -                -                                          ##     |
| 0.50  ##                      -                                      ##        |
| |        ##                     -                                 ##           |
| |        -  #   ## ## ###                                       #              |
| |      -     ##           ## #   -                             #               |
| 0.25      # # #               #   -                  %% %% %  #                |
| |     -  #      #               ##             % %% %       # %%%              |
| |   - ##         #                # -      %% %            #      %%           |
| 0.00#              #                #-  % %              #           %%        |
| |  #%               #                # #                #               %#     |
| |     %%              #           % %  -#             #                 #      |
| -0.25    %%            #      % %%      - #          #               ## -      |
| |           %%% %       # %% %             ##       #              #  -        |
| |                % %% %%  #               -   #   #             # #            |
| |                          #               -   # ##           ##     -         |
| -0.50                        #                 #    ### ## ##      -           |
| |                             # #           # #                   -            |
| |                                ## #   # ##  -                                |
| -0.75                                # #       -             --- sin(x)        |
| |                                                -           --- 0.5 sin(x)    |
| |                                                 -          --- 0.7 cos(x)    |
| |                                                   --- -- - --- -0.3 sin(x)   |
| -1.00---------+----------+----------+-----------+----------+----------+------- |
|    0          1          2          3           4          5          6        |
+--------------------------------------------------------------------------------+
                                       x
y
</code></pre>

[Download ASCII](../../media/examples/legend_demo/legend_box_lower_right.txt)

[Download PDF](../../media/examples/legend_demo/legend_box_lower_right.pdf)

### Legend Box Upper Left

![legend_box_upper_left.png](../../media/examples/legend_demo/legend_box_upper_left.png)

ASCII output:
<pre><code>

                            Legend Box Styling Demo
+--------------------------------------------------------------------------------+
| 1.00                                                                           |
| |                                                                              |
| |                                                                              |
| |   --- sin(x)      - ---                                                      |
| 0.75--- 0.5 sin(x)        -                                                    |
| |   --- 0.7 cos(x)         -                                                   |
| |   --- -0.3 sin(x)          -                                          ##     |
| 0.50  ##                      -                                      ##        |
| |        ##                     -                                 ##           |
| |        -  #   ## ## ###                                       #              |
| |      -     ##           ## #   -                             #               |
| 0.25      # # #               #   -                  %% %% %  #                |
| |     -  #      #               ##             % %% %       # %%%              |
| |   - ##         #                # -      %% %            #      %%           |
| 0.00#              #                #-  % %              #           %%        |
| |  #%               #                # #                #               %#     |
| |     %%              #           % %  -#             #                 #      |
| -0.25    %%            #      % %%      - #          #               ## -      |
| |           %%% %       # %% %             ##       #              #  -        |
| |                % %% %%  #               -   #   #             # #            |
| |                          #               -   # ##           ##     -         |
| -0.50                        #                 #    ### ## ##      -           |
| |                             # #           # #                   -            |
| |                                ## #   # ##  -                                |
| -0.75                                # #       -                -              |
| |                                                -             -               |
| |                                                 -         - -                |
| |                                                   --- -- -                   |
| -1.00---------+----------+----------+-----------+----------+----------+------- |
|    0          1          2          3           4          5          6        |
+--------------------------------------------------------------------------------+
                                       x
y
</code></pre>

[Download ASCII](../../media/examples/legend_demo/legend_box_upper_left.txt)

[Download PDF](../../media/examples/legend_demo/legend_box_upper_left.pdf)

### Legend Lower Left

![legend_lower_left.png](../../media/examples/legend_demo/legend_lower_left.png)

ASCII output:
<pre><code>

                               Legend: Lower Left
+--------------------------------------------------------------------------------+
|                                                                                |
| |                                                                              |
| 4                                                                        -     |
| |                                                                 -------      |
| |                                                         --------             |
| |                                                   -------                    |
| |                                             ------                           |
| 3                                       ------                                 |
| |                                 ------                                       |
| |                            -----                            ############     |
| |                       -----                    #############                 |
| |                   -----             ###########                              |
| 2               ----          #########                                        |
| |            ----       #######                                                |
| |         ---      ######                                                      |
| |      ---     ####                                                            |
| |    ---    ###                                                                |
| 1  --     ##                                                                   |
| |      ###                                                                     |
| |     ##                                                                       |
| |   --- sqrt x                                                                 |
| |   --- ln(x)                                                                  |
| 0--#----+--------+---------+--------+--------+---------+--------+--------+---- |
|        2.5      5.0       7.5     10.0     12.5      15.0     17.5     20.0    |
+--------------------------------------------------------------------------------+


</code></pre>

[Download ASCII](../../media/examples/legend_demo/legend_lower_left.txt)

[Download PDF](../../media/examples/legend_demo/legend_lower_left.pdf)

### Legend Lower Right

![legend_lower_right.png](../../media/examples/legend_demo/legend_lower_right.png)

ASCII output:
<pre><code>

                              Legend: Lower Right
+--------------------------------------------------------------------------------+
|                                                                                |
| |                                                                              |
| 4                                                                        -     |
| |                                                                 -------      |
| |                                                         --------             |
| |                                                   -------                    |
| |                                             ------                           |
| 3                                       ------                                 |
| |                                 ------                                       |
| |                            -----                            ############     |
| |                       -----                    #############                 |
| |                   -----             ###########                              |
| 2               ----          #########                                        |
| |            ----       #######                                                |
| |         ---      ######                                                      |
| |      ---     ####                                                            |
| |    ---    ###                                                                |
| 1  --     ##                                                                   |
| |      ###                                                                     |
| |     ##                                                                       |
| |   ##                                                             --- sqrt x  |
| |   #                                                              --- ln(x)   |
| 0--#----+--------+---------+--------+--------+---------+--------+--------+---- |
|        2.5      5.0       7.5     10.0     12.5      15.0     17.5     20.0    |
+--------------------------------------------------------------------------------+


</code></pre>

[Download ASCII](../../media/examples/legend_demo/legend_lower_right.txt)

[Download PDF](../../media/examples/legend_demo/legend_lower_right.pdf)

### Legend Upper Left

![legend_upper_left.png](../../media/examples/legend_demo/legend_upper_left.png)

ASCII output:
<pre><code>

                               Legend: Upper Left
+--------------------------------------------------------------------------------+
|                                                                                |
| |                                                                              |
| 4                                                                        -     |
| |   --- sqrt x                                                    -------      |
| |   --- ln(x)                                             --------             |
| |                                                   -------                    |
| |                                             ------                           |
| 3                                       ------                                 |
| |                                 ------                                       |
| |                            -----                            ############     |
| |                       -----                    #############                 |
| |                   -----             ###########                              |
| 2               ----          #########                                        |
| |            ----       #######                                                |
| |         ---      ######                                                      |
| |      ---     ####                                                            |
| |    ---    ###                                                                |
| 1  --     ##                                                                   |
| |      ###                                                                     |
| |     ##                                                                       |
| |   ##                                                                         |
| |   #                                                                          |
| 0--#----+--------+---------+--------+--------+---------+--------+--------+---- |
|        2.5      5.0       7.5     10.0     12.5      15.0     17.5     20.0    |
+--------------------------------------------------------------------------------+


</code></pre>

[Download ASCII](../../media/examples/legend_demo/legend_upper_left.txt)

[Download PDF](../../media/examples/legend_demo/legend_upper_left.pdf)

### Legend Upper Right

![legend_upper_right.png](../../media/examples/legend_demo/legend_upper_right.png)

ASCII output:
<pre><code>

                              Legend: Upper Right
+--------------------------------------------------------------------------------+
|                                                                                |
| |                                                                              |
| 4                                                                        -     |
| |                                                                  --- sqrt x  |
| |                                                         -------- --- ln(x)   |
| |                                                   -------                    |
| |                                             ------                           |
| 3                                       ------                                 |
| |                                 ------                                       |
| |                            -----                            ############     |
| |                       -----                    #############                 |
| |                   -----             ###########                              |
| 2               ----          #########                                        |
| |            ----       #######                                                |
| |         ---      ######                                                      |
| |      ---     ####                                                            |
| |    ---    ###                                                                |
| 1  --     ##                                                                   |
| |      ###                                                                     |
| |     ##                                                                       |
| |   ##                                                                         |
| |   #                                                                          |
| 0--#----+--------+---------+--------+--------+---------+--------+--------+---- |
|        2.5      5.0       7.5     10.0     12.5      15.0     17.5     20.0    |
+--------------------------------------------------------------------------------+


</code></pre>

[Download ASCII](../../media/examples/legend_demo/legend_upper_right.txt)

[Download PDF](../../media/examples/legend_demo/legend_upper_right.pdf)

### Multi Function Legend

![multi_function_legend.png](../../media/examples/legend_demo/multi_function_legend.png)

ASCII output:
<pre><code>

                       Mathematical Functions with Legend
+--------------------------------------------------------------------------------+
|                                                                                |
| |                                                                              |
| 1.0                                                                            |
| |                  #############                        --- e^(-x/2)cos(x)     |
| |               ####           ####                     --- xe^(-x/3)          |
| |  #####       ##                  ####                 --- sin(x)/x           |
| |   -  ##     #                       ###               --- x^2e^(-x)          |
| 0.8 -    #   #                           ###                                   |
| |    -    ###                               ###                                |
| |     -    ##                                  ###                             |
| |      -  #  #                                    ###                          |
| 0.6      #    #                                     ####                       |
| |      - #    ##                                        ###                    |
| |       #     %%#%%%%                                      ####                |
| |      # -   %% #    %%%%                                     ####             |
| 0.4    #   %%    ##     %%%                                       ####         |
| |        -%%       #      %%%                                         ####     |
| |     #  %-        #         %%%                                               |
| |    #   % -        #          %%%                                             |
| 0.2    %%  -         #            %%%%                                         |
| |   #  %    -         #               %%%                                      |
| |   # %      -         ##                %%%%%%     #############              |
| |   %%        -         #              --------%%###%%%%%%       ###           |
| 0.0#%         -          ##        ----        ##   ------%%%%%%%%%%#####%     |
| |              -          ##    ---         ###                         ##     |
| |               -           #---          ##                                   |
| |                --       ---###      ####                                     |
| -0.2               --------     #######                                        |
| +--+-------------+-------------+--------------+-------------+-------------+--- |
|    0             2             4              6             8            10    |
+--------------------------------------------------------------------------------+
                                       x
f(x)
</code></pre>

[Download ASCII](../../media/examples/legend_demo/multi_function_legend.txt)

[Download PDF](../../media/examples/legend_demo/multi_function_legend.pdf)

