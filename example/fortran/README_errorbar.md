# Error Bar Plotting with fortplotlib

This directory contains comprehensive examples of error bar functionality in fortplotlib. Error bars are essential for scientific plotting to visualize uncertainty and data quality.

## Quick Start

```fortran
use fortplot
type(figure_t) :: fig
real(wp) :: x(10), y(10), yerr(10)

! Generate data with errors
call fig%initialize(800, 600)
call fig%errorbar(x, y, yerr=yerr, label='Data with errors')
call fig%legend()
call fig%savefig('errorbar_plot.png')
```

## Error Bar Types

### 1. Symmetric Error Bars

**Y errors only:**
```fortran
call fig%errorbar(x, y, yerr=yerr)
```

**X errors only:**
```fortran
call fig%errorbar(x, y, xerr=xerr)
```

**Combined X and Y errors:**
```fortran
call fig%errorbar(x, y, xerr=xerr, yerr=yerr)
```

### 2. Asymmetric Error Bars

**Asymmetric Y errors:**
```fortran
call fig%errorbar(x, y, yerr_lower=y_lower_err, yerr_upper=y_upper_err)
```

**Asymmetric X errors:**
```fortran
call fig%errorbar(x, y, xerr_lower=x_lower_err, xerr_upper=x_upper_err)
```

**Combined asymmetric X and Y errors:**
```fortran
call fig%errorbar(x, y, &
                 xerr_lower=x_lower_err, xerr_upper=x_upper_err, &
                 yerr_lower=y_lower_err, yerr_upper=y_upper_err)
```

## Customization Options

### Visual Styling

```fortran
call fig%errorbar(x, y, yerr=yerr, &
                 capsize=8.0_wp, &           ! Error bar cap size
                 elinewidth=2.0_wp, &        ! Error bar line width
                 marker='o', &               ! Data point marker
                 linestyle='--', &           ! Line style for connecting points
                 color=[1.0_wp, 0.0_wp, 0.0_wp], &  ! RGB color
                 label='Custom error bars')
```

### Available Parameters

- **`capsize`**: Size of error bar caps (default: 5.0)
- **`elinewidth`**: Width of error bar lines (default: 1.0)  
- **`marker`**: Data point marker ('o', 's', '^', etc.)
- **`linestyle`**: Line style ('-', '--', ':', '-.')
- **`color`**: RGB color array [r, g, b] (values 0.0-1.0)
- **`label`**: Legend label for the error bar plot

## Backend Support

Error bars work with all fortplotlib backends:

- **PNG**: High-quality raster graphics for publications
- **PDF**: Vector graphics for scalable scientific figures  
- **ASCII**: Terminal display for quick data inspection

```fortran
call fig%savefig('errorbar_plot.png')  ! PNG output
call fig%savefig('errorbar_plot.pdf')  ! PDF output
call fig%savefig('errorbar_plot.txt')  ! ASCII output
```

## Scientific Usage Patterns

### Experimental Data with Measurement Uncertainty

```fortran
! Typical experimental data pattern
real(wp) :: time(20), signal(20), uncertainty(20)

! Calculate relative uncertainty (e.g., 5% of signal + 0.1 absolute)
do i = 1, 20
    uncertainty(i) = abs(signal(i)) * 0.05_wp + 0.1_wp
end do

call fig%errorbar(time, signal, yerr=uncertainty, &
                 marker='o', linestyle='-', &
                 label='Experimental data')
```

### Multiple Datasets Comparison

```fortran
call fig%initialize(800, 600)

! Dataset 1
call fig%errorbar(x1, y1, yerr=yerr1, &
                 color=[1.0_wp, 0.0_wp, 0.0_wp], label='Experiment A')

! Dataset 2  
call fig%errorbar(x2, y2, yerr=yerr2, &
                 color=[0.0_wp, 0.0_wp, 1.0_wp], label='Experiment B')

call fig%legend()
call fig%set_xlabel('Time (s)')
call fig%set_ylabel('Signal amplitude')
call fig%set_title('Experimental Comparison')
```

## Integration with Other Plot Types

Error bars integrate seamlessly with other fortplotlib plot types:

```fortran
call fig%initialize(800, 600)

! Theoretical curve
call fig%add_plot(x_theory, y_theory, label='Theory', linestyle='-')

! Experimental data with error bars
call fig%errorbar(x_exp, y_exp, yerr=yerr_exp, &
                 marker='o', linestyle='', label='Data')

call fig%legend()
```

## Error Handling

The error bar API includes robust error handling:

- **Size validation**: All error arrays must match data array sizes
- **Graceful fallbacks**: Invalid inputs are handled with clear error messages
- **Conflicting parameters**: Warnings when both symmetric and asymmetric errors provided
- **Edge cases**: Zero errors, negative values, and extreme ranges handled properly

## Performance Considerations

- **Large datasets**: Error bars handle thousands of points efficiently
- **Memory usage**: Minimal overhead beyond data storage
- **Rendering speed**: Optimized for all backends including ASCII terminal display

## Examples

Run the comprehensive error bar examples:

```bash
make example ARGS="errorbar_demo"  # Basic examples
make debug ARGS="test_errorbar_comprehensive"  # Full test suite
make debug ARGS="test_errorbar_edge_cases"     # Edge case validation
```

## Common Pitfalls

1. **Array size mismatch**: Ensure error arrays match data array sizes
2. **Missing error data**: At least one error parameter (xerr, yerr, etc.) must be provided
3. **Asymmetric pairing**: Both lower and upper bounds required for asymmetric errors
4. **Color values**: RGB values must be in range [0.0, 1.0]

## API Reference

### Function Signature

```fortran
subroutine errorbar(self, x, y, xerr, yerr, xerr_lower, xerr_upper, &
                   yerr_lower, yerr_upper, capsize, elinewidth, &
                   label, linestyle, marker, color)
    class(figure_t), intent(inout) :: self
    real(wp), intent(in) :: x(:), y(:)
    real(wp), intent(in), optional :: xerr(:), yerr(:)
    real(wp), intent(in), optional :: xerr_lower(:), xerr_upper(:)
    real(wp), intent(in), optional :: yerr_lower(:), yerr_upper(:)
    real(wp), intent(in), optional :: capsize, elinewidth
    character(len=*), intent(in), optional :: label, linestyle, marker
    real(wp), intent(in), optional :: color(3)
end subroutine
```

This comprehensive error bar implementation makes fortplotlib suitable for professional scientific plotting with uncertainty visualization.