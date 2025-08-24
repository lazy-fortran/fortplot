title: Basic Plots
---

# Basic Plots

This example demonstrates the fundamental plotting capabilities of fortplot using both the simple functional API and the object-oriented interface.

## Source Files

## Source Code

**Fortran:** [basic_plots.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/basic_plots/basic_plots.f90)

**Python:** [basic_plots.py](https://github.com/lazy-fortran/fortplot/blob/main/example/python/basic_plots/basic_plots.py)

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

        ! Generate simple sine data - show 2 complete periods (0 to 4pi)
        x = [(real(i-1, wp) * 4.0_wp * 3.141592653589793_wp / 49.0_wp, i=1, 50)]
        y = sin(x)

        ! Simple plot using functional API
        call figure()
        call plot(x, y, label='sin(x)')
        call title('Simple Sine Wave')
        call xlabel('x')
        call ylabel('y')
        call legend()
        call savefig('output/example/fortran/basic_plots/simple_plot.png')
        call savefig('output/example/fortran/basic_plots/simple_plot.pdf')
        call savefig('output/example/fortran/basic_plots/simple_plot.txt')

        print *, "Created: simple_plot.png/pdf/txt"

    end subroutine simple_plots

    subroutine multi_line_plot()
        real(wp), dimension(50) :: x, sx, cx
        type(figure_t) :: fig
        integer :: i

        x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
        sx = sin(x)
        cx = cos(x)

        ! Multi-line plot using OO interface
        call figure(figsize=[8.0_wp, 6.0_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Sine and Cosine Functions")
        call add_plot(x, sx, label="sin(x)")
        call add_plot(x, cx, label="cos(x)")
        call legend()  ! Add legend for labeled plots
        call savefig('output/example/fortran/basic_plots/multi_line.png')
        call savefig('output/example/fortran/basic_plots/multi_line.pdf')
        call savefig('output/example/fortran/basic_plots/multi_line.txt')

        print *, "Created: multi_line.png/pdf/txt"

    end subroutine multi_line_plot

end program basic_plots
```

## Features Demonstrated

- **Simple plotting**: Basic sine wave using functional API
- **Multi-line plots**: Multiple data series with legends
- **Object-oriented interface**: Using `figure_t` type for advanced control
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
|     Simple Sine Wave                                                           |
| *        #                                                                     |
|        ## ##                                  #####                            |
|       ##    #                                #     #                           |
|              #                              #      #                           |
| *    #        #                                     #                          |
|     #         #                            #         #                         |
|    #                                      #                                    |
| *  #           #                         #            #                        |
|                 #                                     #                       |
|   #                                     #                                      |
|                  #                                     #                       |
| *#                #                     #               #                      |
| #                                      #                                       |
|                   #                                      #                     |
| %                                     #                                      # |
|                    #                                      #                    |
|                                      #                                      #  |
| *                   #               #                     #                #   |
|                      #                                     #                   |
|                                     #                                     #    |
|                       #                                     #                  |
| *                     #            #                         #           #     |
|                                   #                                      #     |
|                        #         #                            #         #      |
| *                       #                                     #        #       |
|                          #      ##                             #               |
|                          #     #                                #    ##        |
| ** ** ** ** ** ** ** ** **#%%#%* ** ** ** ** ** ** ** ** ** ** **#%%#%* ** ** |
|                                                                                |
+--------------------------------------------------------------------------------+
                                       x
```

[Download PDF](../../media/examples/basic_plots/simple_plot.pdf)

### Multi Line

![multi_line.png](../../media/examples/basic_plots/multi_line.png)

ASCII output:
```

                           Sine and Cosine Functions
+--------------------------------------------------------------------------------+
|         Sine and Cosine Functions                                              |
| % sin(x)                                                                       |
| #:cos(x)::::           ###   ###               ###   ####               ###    |
| ####:::#::::          #   # #   #                 # #   #              #   #   |
| ::sin(x)#:::                #                 #    #     #            #      # |
| %###::::::::         #     #     #           #      #                       #  |
| ##c#s(x):#::         #     #                 #      #     #           #     #  |
| ::sin(x):#::              # #     #          #     ##     #          #     # # |
| %###::::::#:        #     # #     #               # #      #         #     #   |
| ##c#s(x)::#:        #       #      #        #     #  #                     #   |
| :#::#:::::#:       #      #  #              #              #        #     #    |
| ##sin(x)::::       #     #         #       #      #  #     #        #          |
| %###:::::::#       #     #   #     #       #     #    #     #       #     #    |
| ::cos(x):::#            #    #      #            #    #            #     #     |
| ####:#::::::#     #           #            #                #            #     |
| %     #           #     #           #     #      #    #      #     #           |
|             #           #     #     #     #     #      #     #    #      #     |
|       #     #    #     #       #     #          #      #          #     #      |
| *      #     #   #             #          #                  #          #      |
|        #         #     #             #   #     #        #     #   #            |
|              #  #     #        #      #  #     #        #     #  #     #       |
|        #      #       #         #     # #      #        #     ## #     #       |
| *       #     # #     #         #     #       #          #     ##      #       |
|         #     ##                 #     ##     #          #      #     #        |
|                #     #           #     ##    #                 #               |
| *        #     #     #                 #                  #    #      #        |
|               #                   #   # #    #                  #              |
|           #   # #   #              #     #  #              #  #  #   #         |
| **********%%%%***%%%***************%%%%***%%***************%%%%***%%%******** |
|                                                                                |
+--------------------------------------------------------------------------------+
                                       x
```

[Download PDF](../../media/examples/basic_plots/multi_line.pdf)