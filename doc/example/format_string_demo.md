title: Format String Demo
---

# Format String Demo

This example demonstrates matplotlib-style format strings for quick and intuitive plot styling.

## Source Files

## Source Code

🔷 **Fortran:** [format_string_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/format_string_demo/format_string_demo.f90)

🐍 **Python:** [format_string_demo.py](https://github.com/lazy-fortran/fortplot/blob/main/example/python/format_string_demo/format_string_demo.py)

```fortran
program format_string_demo
    !! Demonstrate matplotlib-style format strings
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer, parameter :: n = 50
    real(wp) :: x(n), y1(n), y2(n), y3(n), y4(n)
    integer :: i
    type(figure_t) :: fig

    ! Generate sample data
    do i = 1, n
        x(i) = real(i-1, wp) * 0.2_wp
        y1(i) = sin(x(i))
        y2(i) = cos(x(i))
        y3(i) = sin(x(i) * 0.5_wp) * 0.8_wp
        y4(i) = cos(x(i) * 0.5_wp) * 0.6_wp
    end do

    call fig%initialize(800, 600)
    call fig%set_title('Matplotlib-style Format Strings Demo')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')

    ! Different format strings using linestyle parameter (pyplot-fortran style)
    call fig%add_plot(x, y1, label='sin(x) - solid line', linestyle='-')
    call fig%add_plot(x, y2, label='cos(x) - dashed line', linestyle='--')
    call fig%add_plot(x, y3, label='sin(x/2) - circles only', linestyle='o')
    call fig%add_plot(x, y4, label='cos(x/2) - x markers with line', linestyle='x-')

    call fig%legend()
    call fig%savefig('output/example/fortran/format_string_demo/format_string_demo.png')
    call fig%savefig('output/example/fortran/format_string_demo/format_string_demo.pdf')
    call fig%savefig('output/example/fortran/format_string_demo/format_string_demo.txt')

    write(*, '(A)') 'Format string demo saved to format_string_demo.png/pdf/txt'

end program
```

## Features Demonstrated

- **Color shortcuts**: Single letter color codes
- **Line style codes**: Solid, dashed, dotted
- **Marker codes**: Combined with line styles
- **Compact notation**: Full styling in one string

## Format String Syntax

Format: `[color][marker][linestyle]`

### Colors
- `b` - blue
- `r` - red
- `g` - green
- `k` - black
- `m` - magenta
- `c` - cyan
- `y` - yellow

### Line Styles
- `-` - solid line
- `--` - dashed line
- `:` - dotted line
- `-.` - dash-dot line

### Markers
- `o` - circle
- `s` - square
- `^` - triangle
- `*` - star

## Examples

- `'r-'` - Red solid line
- `'bo'` - Blue circles (no line)
- `'g--'` - Green dashed line
- `'k:o'` - Black dotted line with circles

## Output Example

![Format String Demo](media/examples/format_string_demo.png)
