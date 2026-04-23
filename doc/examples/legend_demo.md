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
|                                                                                |
| *                                                                              |
|  *       * ** *                                ** **    -- sin(x)              |
|    *           *                             *       *  --*cos(x) *            |
|     *  *         *                                      *                      |
| .     *                                     *         *            *           |
|      #*           *                       *           *                        |
|     *  #           #                                  # *            *         |
| .      *            *                    #           *   #           #         |
|    #    #            #                  *                 *           *        |
|    *     *            *                 #           #     #                    |
|   #       #                            *           *                   #       |
| .*                    #               #            #       *            *      |
|            *           *                          *         #            #     |
| #          #            #            *                       *                 |
| *           *                       #            #                       *     |
|                          *                      *             #           #    |
|              #           #          *          #              *            *   |
| .             *           *        #                           #               |
|               #                   *            *                            #  |
|                            #     #            #                 *            * |
|                *            *                *                   #             |
| .               #            #  #*          #                     *            |
|                  *           *  #                                 #            |
|                                *            *                      *           |
| .                 *            *          *                                    |
|                              *                                       *         |
|                     *       *    *      *                             *        |
| .. . . . . . . . . . .** ** . . . * **.*. . . . . .. . . . . . . . . . .** *.* |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download ASCII](../../media/examples/legend_demo/basic_legend.txt)

[Download PDF](../../media/examples/legend_demo/basic_legend.pdf)

### Legend Lower Left

![legend_lower_left.png](../../media/examples/legend_demo/legend_lower_left.png)

ASCII output:
```

                               Legend: Lower Left
+--------------------------------------------------------------------------------+
|                                                                                |
| .                                                                           #* |
|                                                                        #*####  |
| .                                                                ###*##        |
| .                                                           *###*              |
|                                                        #*###                   |
| .                                                 ##*##                        |
|                                              ###*#                             |
| .                                       *###*                                  |
| .                                   *###                                       |
| .                               *###                                        #* |
|                             *###                                *###*###*###   |
| .                        ###                         ###*###*###               |
| .                    ###*                   *###*###*                          |
|                  ###*                ###*###                                   |
| .              #*#             #*###*                                          |
| .           *##          ###*##                                                |
|          ###         ###*#                                                     |
| .      #*        ###*                                                          |
| .    ##       ##*#                                                             |
|    #*      #*#                                                                 |
| .##      ##                                                                    |
| *      #*                                                                      |
|      ##                                                                        |
| .   *                                                                          |
| .  #                                                                           |
| --#√x                                                                        |
| -# ln(x)                                                                       |
| *......... . ................................................................. |
|                                                                                |
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
| .                                                                           #* |
|                                                                        #*####  |
| .                                                                ###*##        |
| .                                                           *###*              |
|                                                        #*###                   |
| .                                                 ##*##                        |
|                                              ###*#                             |
| .                                       *###*                                  |
| .                                   *###                                       |
| .                               *###                                        #* |
|                             *###                                *###*###*###   |
| .                        ###                         ###*###*###               |
| .                    ###*                   *###*###*                          |
|                  ###*                ###*###                                   |
| .              #*#             #*###*                                          |
| .           *##          ###*##                                                |
|          ###         ###*#                                                     |
| .      #*        ###*                                                          |
| .    ##       ##*#                                                             |
|    #*      #*#                                                                 |
| .##      ##                                                                    |
| *      #*                                                                      |
|      ##                                                                        |
| .   *                                                                          |
| .  #                                                                           |
|   #                                                     -- √x                |
| .#                                                      -- ln(x)               |
| *......... . ................................................................. |
|                                                                                |
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
| -- √x                                                                        |
| -- ln(x)                                                                    #* |
|                                                                        #*####  |
| .                                                                ###*##        |
| .                                                           *###*              |
|                                                        #*###                   |
| .                                                 ##*##                        |
|                                              ###*#                             |
| .                                       *###*                                  |
| .                                   *###                                       |
| .                               *###                                        #* |
|                             *###                                *###*###*###   |
| .                        ###                         ###*###*###               |
| .                    ###*                   *###*###*                          |
|                  ###*                ###*###                                   |
| .              #*#             #*###*                                          |
| .           *##          ###*##                                                |
|          ###         ###*#                                                     |
| .      #*        ###*                                                          |
| .    ##       ##*#                                                             |
|    #*      #*#                                                                 |
| .##      ##                                                                    |
| *      #*                                                                      |
|      ##                                                                        |
| .   *                                                                          |
| .  #                                                                           |
|   #                                                                            |
| .#                                                                             |
| *......... . ................................................................. |
|                                                                                |
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
| .                                                                           #* |
|                                                         -- √x        #*####  |
| .                                                       -- ln(x) ###*##        |
| .                                                           *###*              |
|                                                        #*###                   |
| .                                                 ##*##                        |
|                                              ###*#                             |
| .                                       *###*                                  |
| .                                   *###                                       |
| .                               *###                                        #* |
|                             *###                                *###*###*###   |
| .                        ###                         ###*###*###               |
| .                    ###*                   *###*###*                          |
|                  ###*                ###*###                                   |
| .              #*#             #*###*                                          |
| .           *##          ###*##                                                |
|          ###         ###*#                                                     |
| .      #*        ###*                                                          |
| .    ##       ##*#                                                             |
|    #*      #*#                                                                 |
| .##      ##                                                                    |
| *      #*                                                                      |
|      ##                                                                        |
| .   *                                                                          |
| .  #                                                                           |
|   #                                                                            |
| .#                                                                             |
| *......... . ................................................................. |
|                                                                                |
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
| .                      *                                                       |
|                  ****** ********                    -- e^(-x/2)cos(x)          |
|               ***              ****                 -- xe^(-x/3)               |
| ****         **                    ****             -- sin(x)/x                |
| *   **     **                         ****          -- x²e^(-x)               |
| .*    **  *                               **                                   |
|   *    ** *                                 ***                                |
|         **                                     ***                             |
|    *   *  *                                       ***                          |
| .  *   *  **                                        ****                       |
|     * *     *                                           ***                    |
|              *                                             ***                 |
|      *      ********                                          ****             |
|     * *   **  *     ***                                           ***          |
| .  *     *     **      **                                            ****      |
|        **        *       ***                                             ***** |
|    *   *         *          **                                                 |
|   *   * *         *           ***                                              |
| .    *             *             ***                                           |
|  *  *    *          *               ***                                        |
|    *      *          *                 *****            ********               |
| * **      *           *                     ******* *****       *****          |
|  *         *           **             *****************************  ***       |
| *           *           *         ****         ***       ********************* |
|              *           **    ***           **                             ** |
|               *            *****          ***                                  |
|                **         *****        ****                                    |
| *. . . . . . . . *********. . .******** . . . . . .. . . . . . . . . . . . ..  |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download ASCII](../../media/examples/legend_demo/multi_function_legend.txt)

[Download PDF](../../media/examples/legend_demo/multi_function_legend.pdf)

