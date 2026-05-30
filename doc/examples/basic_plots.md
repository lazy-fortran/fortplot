title: Basic Plots
---

# Basic Plots

Source: [basic_plots.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/basic_plots/basic_plots.f90)

Basic line plots and saving outputs (PNG, PDF, ASCII).

## Files

- `basic_plots.f90` - Source code
- Generated media in `output/example/fortran/basic_plots/`

## Running

```bash
make example ARGS="basic_plots"
```

## Output

### Multi Line

![multi_line.png](../../media/examples/basic_plots/multi_line.png)

ASCII output:
```

                           Sine and Cosine Functions
+--------------------------------------------------------------------------------+
| 1.00                                                                           |
| |                                                                              |
|                                                                                |
| 0.75                                                               -- sin(x)   |
|                                                                    -- cos(x)   |
| |                                                                              |
|                                                                                |
| 0.50                                                                           |
| |    ---                #    ---                ##    ---               ##     |
|  ##--  -              ## ## -  --             ##  # --  -              #  ##   |
| 0.25    --           #     #-    -           #     ##    -            #     #- |
|   -#     -          ##    --#     -          #     -#     -          ##    -## |
| |-  #     -        ##     - #     --        #     - ##     -        ##     -   |
|  -  ##    --       #     -   #     -       #     --  ##    -        #     -    |
| 0.00 #     -      ##    --   #      -      #     -    #     -      ##    -     |
| |    ##     -     #     -     #     -     #     --    ##     -     #     -     |
|       #     -    #     --      #    --    #     -      #     -    #     -      |
|        #     -   #     -       #     --  #     -        #     -  ##    --      |
| |      ##     - #     -         #     - ##     -        #     -- #     -       |
| -0.25   #     -#     --         ##    --#     -          #     -#     --       |
|          #    #-     -           ##    #     -            #    #-     -        |
|           #   # -- --              #  # --  --             #  #  -- --         |
| |          ###   --                ###    --                ##    --           |
| -0.50                                                                          |
|                                                                                |
| |                                                                              |
| -0.75                                                                          |
|                                                                                |
| |----------------------------------------------------------------------------  |
| 0.0     2.5       5.0       7.5     10.0      12.5      15.0      17.5         |
+--------------------------------------------------------------------------------+
                                       x
y
```

[Download ASCII](../../media/examples/basic_plots/multi_line.txt)

[Download PDF](../../media/examples/basic_plots/multi_line.pdf)

### Simple Plot

![simple_plot.png](../../media/examples/basic_plots/simple_plot.png)

ASCII output:
```

                                Simple Sine Wave
+--------------------------------------------------------------------------------+
|                                                                                |
| |                                                                              |
| 0.75                                                                           |
|                                                                                |
| |                                                                              |
| 0.50                                                                           |
|                                                                                |
| |      ------                                -------                           |
| 0.25---      --                            --      --                          |
| |  -          --                         --          --                        |
|   -             --                      -              -                       |
| 0.00              -                    --               -                      |
| |                 --                  -                  --                  - |
|                     -               --                    -                --  |
| -0.25                --             -                      --             -    |
| |                     --          --                         --          -     |
|                         --      --                            --      ---      |
| |                        -------                                ------         |
| -0.50                                                                          |
|                                                                                |
| |                                                                              |
| -0.75                                                                          |
| |- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --  |
| 0          2            4           6            8          10          12     |
+--------------------------------------------------------------------------------+
                                       x
sin(x)
```

[Download ASCII](../../media/examples/basic_plots/simple_plot.txt)

[Download PDF](../../media/examples/basic_plots/simple_plot.pdf)

