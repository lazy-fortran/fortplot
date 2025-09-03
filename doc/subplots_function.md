# Subplots Function

The `subplots()` function creates a grid of subplots on the current figure, similar to matplotlib's `plt.subplots()` function.

## Synopsis

```fortran
call subplots(nrows, ncols)
```

## Parameters

- `nrows` (integer, required): Number of rows in the subplot grid
- `ncols` (integer, required): Number of columns in the subplot grid

## Description

The `subplots()` function initializes a grid of subplots on the global figure. This is useful for creating multi-panel figures where different datasets or visualizations can be displayed side-by-side.

## Example

```fortran
program subplot_example
    use fortplot
    implicit none
    
    real(8) :: x(100), y(100)
    integer :: i
    
    ! Generate data
    do i = 1, 100
        x(i) = real(i-1, 8) * 0.1
        y(i) = sin(x(i))
    end do
    
    ! Create a 2x2 grid of subplots
    call subplots(2, 2)
    
    ! Add plot (currently adds to the figure, subplot targeting not yet implemented)
    call plot(x, y, label="sin(x)")
    call xlabel("x")
    call ylabel("y") 
    call title("Subplot Grid Example")
    
    ! Save figure
    call savefig("subplots_example.png")
end program
```

## Notes

- The function creates the subplot grid structure on the figure
- Individual subplot targeting via the stateful API is not yet fully implemented
- The object-oriented API provides full subplot functionality via the `figure_t%subplot_plot()` method

## See Also

- `subplot()` - Select a specific subplot (stub implementation)
- `figure_t%subplots()` - Object-oriented API for creating subplot grids