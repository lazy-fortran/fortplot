title: Basic Plots
---

# Basic Plots

This example demonstrates the fundamental plotting capabilities of fortplotlib using both the simple functional API and the object-oriented interface.

## Source Files

## Source Code

**Fortran:** [basic_plots.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/basic_plots/basic_plots.f90)
**Python:** [basic_plots.py](https://github.com/krystophny/fortplotlib/blob/main/example/python/basic_plots/basic_plots.py)

```fortran
program basic_plots
    !! Basic plotting examples using both simple and OO APIs
    use fortplot
    implicit none

    call simple_plots()
    call multi_line_plot()

contains

    subroutine simple_plots()
        real(wp), dimension(50) :: x, y
        integer :: i

        print *, "=== Basic Plots ==="

        ! Generate simple sine data - show 2 complete periods (0 to 4π)
        x = [(real(i-1, wp) * 4.0_wp * 3.141592653589793_wp / 49.0_wp, i=1, 50)]
        y = sin(x)

        ! Simple plot using functional API
        call figure()
        call plot(x, y, label='sin(x)')
        call title('Simple Sine Wave')
        call xlabel('x')
        call ylabel('sin(x)')
        call savefig('example/fortran/basic_plots/simple_plot.png')
        call savefig('example/fortran/basic_plots/simple_plot.pdf')
        call savefig('example/fortran/basic_plots/simple_plot.txt')

        print *, "Created: simple_plot.png/pdf/txt"

    end subroutine simple_plots

    subroutine multi_line_plot()
        real(wp), dimension(100) :: x, sx, cx
        type(figure_t) :: fig
        integer :: i

        x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
        sx = sin(x)
        cx = cos(x)

        ! Multi-line plot using OO interface
        call fig%initialize(640, 480)
        call fig%set_xlabel("x")
        call fig%set_ylabel("y")
        call fig%set_title("Sine and Cosine Functions")
        call fig%add_plot(x, sx, label="sin(x)")
        call fig%add_plot(x, cx, label="cos(x)")
        call fig%legend()  ! Add legend for labeled plots
        call fig%savefig('example/fortran/basic_plots/multi_line.png')
        call fig%savefig('example/fortran/basic_plots/multi_line.pdf')
        call fig%savefig('example/fortran/basic_plots/multi_line.txt')

        print *, "Created: multi_line.png/pdf/txt"

    end subroutine multi_line_plot

end program basic_plots
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

