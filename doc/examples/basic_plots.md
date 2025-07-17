title: Basic Plots
---

# Basic Plots

This example demonstrates the fundamental plotting capabilities of fortplotlib using both the simple functional API and the object-oriented interface.

## Source Files

### Fortran Source

📄 [basic_plots.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/basic_plots/basic_plots.f90)

### Python Equivalent

🐍 [basic_plots.py](https://github.com/krystophny/fortplotlib/blob/main/example/python/basic_plots/basic_plots.py)

### Generated Output Files

- `simple_plot.png/pdf/txt` - Simple sine wave visualization
- `multi_line.png/pdf/txt` - Multiple functions on the same plot

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

![simple_plot.png](../../media/examples/simple_plot.png)

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

[Download PDF](../../media/examples/simple_plot.pdf)

### Multi Line

![multi_line.png](../../media/examples/multi_line.png)

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

[Download PDF](../../media/examples/multi_line.pdf)

