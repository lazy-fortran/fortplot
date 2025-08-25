title: Legend Demo
---

# Legend Demo

This example demonstrates legend placement and customization options.

## Source Files

## Source Code

**Fortran:** [legend_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/legend_demo/legend_demo.f90)

**Python:** [legend_demo.py](https://github.com/lazy-fortran/fortplot/blob/main/example/python/legend_demo/legend_demo.py)

```fortran
program legend_demo
    !! Demonstration of legend functionality following SOLID principles
    !! Shows legend positioning, labeling, and rendering across all backends
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    call basic_legend_example()
    call positioned_legend_example()
    call multi_function_legend_example()
    call legend_box_styling_example()

contains

    subroutine basic_legend_example()
        !! Basic legend usage with labeled plots
        type(figure_t) :: fig
        real(wp), dimension(50) :: x, y1, y2
        integer :: i

        print *, "=== Basic Legend Example ==="

        ! Generate data
        x = [(real(i-1, wp) * 0.2_wp, i=1, 50)]
        y1 = sin(x)
        y2 = cos(x)

        call figure(figsize=[8.0_wp, 6.0_wp])
        call title("Basic Legend Demo")
        call xlabel("x")
        call ylabel("y")

        ! Add labeled plots
        call add_plot(x, y1, label="sin(x)")
        call add_plot(x, y2, label="cos(x)")

        ! Add legend with default position (upper right)
        call legend()

        call savefig('output/example/fortran/legend_demo/basic_legend.png')
        call savefig('output/example/fortran/legend_demo/basic_legend.pdf')
        call savefig('output/example/fortran/legend_demo/basic_legend.txt')

        print *, "Created: basic_legend.png/pdf/txt"
    end subroutine basic_legend_example

    subroutine positioned_legend_example()
        !! Demonstrate different legend positions
        type(figure_t) :: fig1, fig2, fig3, fig4
        real(wp), dimension(20) :: x, y1, y2
        integer :: i

        print *, "=== Legend Positioning Examples ==="

        ! Generate test data
        x = [(real(i, wp), i=1, 20)]
        y1 = x**0.5_wp
        y2 = log(x)

        ! Upper left position
        call figure(figsize=[640.0_wp, 480.0_wp])
        call title("Legend: Upper Left")
        call add_plot(x, y1, label="sqrt(x)")
        call add_plot(x, y2, label="ln(x)")
        call legend(position="upper left")
        call savefig('output/example/fortran/legend_demo/legend_upper_left.png')
        call savefig('output/example/fortran/legend_demo/legend_upper_left.pdf')
        call savefig('output/example/fortran/legend_demo/legend_upper_left.txt')

        ! Upper right position (default)
        call figure(figsize=[640.0_wp, 480.0_wp])
        call title("Legend: Upper Right")
        call add_plot(x, y1, label="sqrt(x)")
        call add_plot(x, y2, label="ln(x)")
        call legend(position="upper right")
        call savefig('output/example/fortran/legend_demo/legend_upper_right.png')
        call savefig('output/example/fortran/legend_demo/legend_upper_right.pdf')
        call savefig('output/example/fortran/legend_demo/legend_upper_right.txt')

        ! Lower left position
        call figure(figsize=[640.0_wp, 480.0_wp])
        call title("Legend: Lower Left")
        call add_plot(x, y1, label="sqrt(x)")
        call add_plot(x, y2, label="ln(x)")
        call legend(position="lower left")
        call savefig('output/example/fortran/legend_demo/legend_lower_left.png')
        call savefig('output/example/fortran/legend_demo/legend_lower_left.pdf')
        call savefig('output/example/fortran/legend_demo/legend_lower_left.txt')

        ! Lower right position
        call figure(figsize=[640.0_wp, 480.0_wp])
        call title("Legend: Lower Right")
        call add_plot(x, y1, label="sqrt(x)")
        call add_plot(x, y2, label="ln(x)")
        call legend(position="lower right")
        call savefig('output/example/fortran/legend_demo/legend_lower_right.png')
        call savefig('output/example/fortran/legend_demo/legend_lower_right.pdf')
        call savefig('output/example/fortran/legend_demo/legend_lower_right.txt')

        print *, "Created: legend_upper_left/right.png/pdf/txt, legend_lower_left/right.png/pdf/txt"
    end subroutine positioned_legend_example

    subroutine multi_function_legend_example()
        !! Complex legend with multiple mathematical functions
        type(figure_t) :: fig
        real(wp), dimension(100) :: x, y1, y2, y3, y4
        integer :: i

        print *, "=== Multi-Function Legend Example ==="

        ! Generate mathematical functions
        x = [(real(i-1, wp) * 0.1_wp, i=1, 100)]
        y1 = exp(-x/2.0_wp) * cos(x)
        y2 = x * exp(-x/3.0_wp)
        y3 = sin(x) / x
        y4 = x**2 * exp(-x)

        call figure(figsize=[8.0_wp, 6.0_wp])
        call title("Mathematical Functions with Legend")
        call xlabel("x")
        call ylabel("f(x)")

        ! Add multiple labeled functions
        call add_plot(x, y1, label="e^(-x/2)cos(x)")
        call add_plot(x, y2, label="xe^(-x/3)")
        call add_plot(x, y3, label="sin(x)/x")
        call add_plot(x, y4, label="x^2*e^(-x)")

        ! Add legend
        call legend()

        call savefig('output/example/fortran/legend_demo/multi_function_legend.png')
        call savefig('output/example/fortran/legend_demo/multi_function_legend.pdf')
        call savefig('output/example/fortran/legend_demo/multi_function_legend.txt')

        print *, "Created: multi_function_legend.png/pdf/txt"
    end subroutine multi_function_legend_example

    subroutine legend_box_styling_example()
        !! Demonstrate legend box styling and multiple entries
        type(figure_t) :: fig
        real(wp), dimension(50) :: x, y1, y2, y3, y4
        integer :: i

        print *, "=== Legend Box Styling Example ==="

        ! Generate test data with multiple functions
        x = [(real(i-1, wp) / 49.0_wp * 2.0_wp * 3.14159_wp, i=1, 50)]
        y1 = sin(x)
        y2 = y1 * 0.5_wp
        y3 = cos(x) * 0.7_wp
        y4 = -y1 * 0.3_wp

        call figure(figsize=[8.0_wp, 6.0_wp])
        call title("Legend Box Styling Demo")
        call xlabel("x")
        call ylabel("y")

        ! Add multiple plots to test legend box styling
        call add_plot(x, y1, label="sin(x)")
        call add_plot(x, y2, label="0.5 sin(x)")
        call add_plot(x, y3, label="0.7 cos(x)")
        call add_plot(x, y4, label="-0.3 sin(x)")

        ! Test legend box with default styling
        call legend()
        call savefig('output/example/fortran/legend_demo/legend_box_default.png')
        call savefig('output/example/fortran/legend_demo/legend_box_default.pdf')
        call savefig('output/example/fortran/legend_demo/legend_box_default.txt')

        ! Test legend box in different positions
        call legend(position="upper left")
        call savefig('output/example/fortran/legend_demo/legend_box_upper_left.png')
        call savefig('output/example/fortran/legend_demo/legend_box_upper_left.pdf')
        call savefig('output/example/fortran/legend_demo/legend_box_upper_left.txt')

        call legend(position="lower right")
        call savefig('output/example/fortran/legend_demo/legend_box_lower_right.png')
        call savefig('output/example/fortran/legend_demo/legend_box_lower_right.pdf')
        call savefig('output/example/fortran/legend_demo/legend_box_lower_right.txt')

        print *, "Created: legend_box_default.png/pdf/txt, legend_box_upper_left.png/pdf/txt, legend_box_lower_right.png/pdf/txt"
    end subroutine legend_box_styling_example

end program legend_demo
```

## Features Demonstrated

- **Automatic placement**: Default "best" location
- **Manual placement**: Specify corner positions
- **Multi-line legends**: Handle multiple labeled plots
- **Smart positioning**: Avoids overlapping with data
- **Box styling**: Legend boxes with frames and backgrounds
- **Multiple entries**: Complex legends with 4+ labeled plots
- **Multi-format output**: PNG, PDF, and ASCII text versions

## Legend Locations

- `best` - Automatic optimal placement
- `upper left` - Top left corner
- `upper right` - Top right corner (default)
- `lower left` - Bottom left corner
- `lower right` - Bottom right corner

## Output

### Basic Legend

![basic_legend.png](../../media/examples/legend_demo/basic_legend.png)

ASCII output:
```
                               Basic Legend Demo
+--------------------------------------------------------------------------------+
|                                                                                |
| .                                                                              |
|                                                                                |
|                                                                                |
|                                                                                |
| .                                                                              |
|                                                                                |
|                                                                                |
| .                                                                              |
|                                                                                |
|                                                                                |
|                                                                                |
| .                                                                              |
|                                                                                |
|                                                                                |
| .                                                                              |
|                                                                                |
|                                                                                |
| .                                                                              |
|                                                                                |
|                                                                                |
|                                                                                |
| .                                                                              |
|                                                                                |
|                                                                                |
| .                                                                              |
|                                                                                |
|                                                                                |
| .. . . . . . . . . . . . .. . . . . . . . . . . . .. . . . . . . . . . . . ..  |
|                                                                                |
+--------------------------------------------------------------------------------+

```

### Legend Upper Left

![legend_upper_left.png](../../media/examples/legend_demo/legend_upper_left.png)

### Legend Upper Right

![legend_upper_right.png](../../media/examples/legend_demo/legend_upper_right.png)

### Legend Lower Left

![legend_lower_left.png](../../media/examples/legend_demo/legend_lower_left.png)

### Legend Lower Right

![legend_lower_right.png](../../media/examples/legend_demo/legend_lower_right.png)

### Multi Function Legend

![multi_function_legend.png](../../media/examples/legend_demo/multi_function_legend.png)