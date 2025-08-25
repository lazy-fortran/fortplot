title: Basic Plots
---

# Basic Plots

This example demonstrates the fundamental plotting capabilities of fortplotlib using both the simple functional API and the object-oriented interface.

## Files

- `basic_plots.f90` - Source code
- `simple_plot.png/pdf/txt` - Simple sine wave outputs
- `multi_line.png/pdf/txt` - Multi-line plot outputs

## Running

```bash
make example ARGS="basic_plots"
```

## Features Demonstrated

- **Functional API**: Simple, matplotlib-like interface with global figure management
- **Object-Oriented API**: More control through `figure_t` type
- **Multiple output formats**: PNG, PDF, and ASCII text
- **Line labeling**: Automatic legend generation
- **Axis labeling**: Clear axis titles and labels

## Output

### Simple Plot

![simple_plot.png](../../media/examples/basic_plots/simple_plot.png)

ASCII output:
```

                                Simple Sine Wave
+--------------------------------------------------------------------------------+
|                                                                                |
| .        *                                                                     |
|        *# #*                                  #**#*                            |
|       *#    *                                *     #                           |
|              #                              *      *                           |
| .    #        *                                     #                          |
|     *         #                            #         *                         |
|    #                                      *                                    |
| .  *           *                         #            #                        |
|                 #                                      *                       |
|   #                                     *                                      |
|                  *                                     #                       |
| .*                #                     #               *                      |
| #                                      *                                       |
|                   *                                      #                     |
| *                                     #                                      * |
|                    #                                      *                    |
|                                      *                                      #  |
| .                   *               #                     #                *   |
|                      #                                     *                   |
|                                     *                                     #    |
|                       *                                     #                  |
| .                     #            #                         *           *     |
|                                   *                                      #     |
|                        *         #                            #         *      |
| .                       #                                     *        #       |
|                          *      #*                             #               |
|                          #     *                                *    #*        |
| .. .. .. .. .. .. .. .. ..*#**#. .. .. .. .. .. .. .. .. .. .. ..#**#*. .. ..  |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download PDF](../../media/examples/basic_plots/simple_plot.pdf)

### Multi Line

![multi_line.png](../../media/examples/basic_plots/multi_line.png)

ASCII output:
```

                           Sine and Cosine Functions
+--------------------------------------------------------------------------------+
|                                                                                |
| *                                                                              |
| *   ****               ***   ***               ***   ****- sin(x)       ***    |
|  *     *              *   * *   *                 * *   *- cos(x)      *   *   |
|   **    *                   *                 *    *     *            *      * |
| .  *                 *     *     *           *      *                       *  |
|   #*     *           *     *                 #      *     *           *     %  |
|   *#     #                # *     *          *     *#     #          #     # * |
| .  *      *         #     * #     #               # *      *         *     *   |
|  #  #     #         *       *      *        #     *  #                     #   |
|  *  *     *        #      #  #              *              #        #     *    |
|  #   #             *     *         #       #      #  *     *        *          |
| *          #       #     #   *     *       *     *    #     #       #     #    |
|      *     *            *    #      #            #    *            *     *     |
| #    #      #     *           *            #                *            #     |
| *     *           #     #           *     *      *    #      #     #           |
|             *           *     #     #     #     #      *     *    *      *     |
|       #     #    *     #       *     *          *      #          #     #      |
| .      *     *   #             #          *                  #          *      |
|        #         *     *             #   #     #        *     *   *            |
|              #  #     #        *      *  *     *        #     #  #     #       |
|        *      *       *         #     # #      #        *     *# *     *       |
| .       #     # *     #         *     *       *          #     ##      #       |
|         *     *#                 #     #*     #          *      *     *        |
|                *     *           *     *#    *                 *               |
| .        *     *     *                 *                  *    *      *        |
|               *                   *   * *    *                  *              |
|           *   * *   *              *     *  *              *  *  *   *         |
| ..........****...***...............****...**...............****...***........  |
|                                                                                |
+--------------------------------------------------------------------------------+
```

[Download PDF](../../media/examples/basic_plots/multi_line.pdf)