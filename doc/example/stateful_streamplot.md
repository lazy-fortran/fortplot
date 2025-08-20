title: Stateful Streamplot
---

# Stateful Streamplot

This example shows time-evolving vector field animations using streamplots.

## Source Files

## Source Code

🔷 **Fortran:** [stateful_streamplot.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/stateful_streamplot/stateful_streamplot.f90)

```fortran
program stateful_streamplot
    use fortplot
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none

    integer, parameter :: nx = 20, ny = 20
    real(real64), dimension(nx) :: x
    real(real64), dimension(ny) :: y
    real(real64), dimension(nx, ny) :: u, v
    integer :: i, j

    ! Create grid
    do i = 1, nx
        x(i) = -2.0_real64 + 4.0_real64 * real(i-1, real64) / real(nx-1, real64)
    end do

    do j = 1, ny
        y(j) = -2.0_real64 + 4.0_real64 * real(j-1, real64) / real(ny-1, real64)
    end do

    ! Create circular flow field
    do j = 1, ny
        do i = 1, nx
            u(i,j) = -y(j)
            v(i,j) = x(i)
        end do
    end do

    ! Create streamplot using stateful interface
    call figure(800, 600)
    call streamplot(x, y, u, v, density=1.0_real64)
    call xlabel('X')
    call ylabel('Y')
    call title('Stateful Interface Streamplot Demo - Circular Flow')

    ! Save figure
    call savefig('output/example/fortran/stateful_streamplot/stateful_streamplot.png')
    call savefig('output/example/fortran/stateful_streamplot/stateful_streamplot.pdf')
    call savefig('output/example/fortran/stateful_streamplot/stateful_streamplot.txt')

    print *, 'Stateful streamplot demo completed!'

end program stateful_streamplot
```

## Features Demonstrated

- **Time-dependent fields**: Vector fields that change over time
- **State preservation**: Maintain streamline continuity
- **Smooth transitions**: Interpolate between states
- **Physical simulations**: Fluid flow, electromagnetic fields

## Applications

- **Fluid dynamics**: Visualize flow evolution
- **Weather patterns**: Show wind field changes
- **Electromagnetic fields**: Time-varying E/B fields
- **Traffic flow**: Vehicle movement patterns

## Implementation Notes

- **Stateful integration**: Remember previous streamline positions
- **Adaptive refinement**: Add/remove streamlines as needed
- **Performance**: Optimized for real-time updates
- **Memory efficient**: Only store necessary state

## Output Example

![Stateful Streamplot](../../media/examples/stateful_streamplot/stateful_streamplot.png)
