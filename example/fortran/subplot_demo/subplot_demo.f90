program subplot_demo
    !! Demonstration of subplot functionality using the stateful API
    !! 
    !! This example demonstrates:
    !! - 2x2 subplot grid layout
    !! - 1x3 subplot horizontal layout 
    !! - Independent titles and labels per subplot
    !! - Different plot types in subplots
    !! 
    !! Expected API Usage (for GREEN phase implementation):
    !! - subplot(rows, cols, index) to create and switch to subplots
    !! - plot(), title(), xlabel(), ylabel() work within current subplot
    !! - savefig() renders entire subplot grid to output file

    use iso_fortran_env, only: real64, wp => real64
    use fortplot, only: figure, plot, xlabel, ylabel, title, savefig, show
    implicit none

    real(wp), allocatable :: x(:), y(:)
    real(wp), allocatable :: x2(:), y2(:)
    integer :: i

    ! Generate test data
    allocate(x(100), y(100))
    do i = 1, 100
        x(i) = real(i - 1, wp) * 0.1_wp
        y(i) = sin(x(i))
    end do

    allocate(x2(50), y2(50))
    do i = 1, 50
        x2(i) = real(i - 1, wp) * 0.2_wp
        y2(i) = exp(-x2(i) * 0.5_wp) * cos(2.0_wp * x2(i))
    end do

    ! Example 1: 2x2 subplot grid
    call figure(800, 600)
    
    print *, 'Creating 2x2 subplot example...'
    print *, 'NOTE: subplot() function not yet implemented (RED phase)'
    print *, 'Expected usage:'
    print *, '  call subplot(2, 2, 1)  ! Top-left'
    print *, '  call plot(x, sin(x))'
    print *, '  call title("Sine Wave")'
    print *, '  call xlabel("x")'
    print *, '  call ylabel("sin(x)")'
    print *, ''
    print *, '  call subplot(2, 2, 2)  ! Top-right'
    print *, '  call plot(x, cos(x))'
    print *, '  call title("Cosine Wave")'
    print *, ''
    print *, '  call subplot(2, 2, 3)  ! Bottom-left'
    print *, '  call plot(x2, y2)'
    print *, '  call title("Damped Oscillation")'
    print *, ''
    print *, '  call subplot(2, 2, 4)  ! Bottom-right'
    print *, '  call plot(x2, x2**2/50)'
    print *, '  call title("Quadratic Function")'

    ! For now, create a single plot as placeholder
    call plot(x, sin(x))
    call title('Placeholder - Will be 2x2 Subplot Grid')
    call xlabel('x')
    call ylabel('y')
    call savefig('output/example/fortran/subplot_demo/subplot_2x2_demo.png')

    ! Example 2: 1x3 subplot layout
    call figure(900, 300)
    
    print *, ''
    print *, 'Creating 1x3 subplot example...'
    print *, 'Expected usage:'
    print *, '  call subplot(1, 3, 1)  ! Left'
    print *, '  call plot(x2, x2)'
    print *, '  call title("Linear")'
    print *, ''
    print *, '  call subplot(1, 3, 2)  ! Center'
    print *, '  call plot(x2, x2**2)'
    print *, '  call title("Quadratic")'
    print *, ''
    print *, '  call subplot(1, 3, 3)  ! Right'
    print *, '  call plot(x2, x2**3)'
    print *, '  call title("Cubic")'

    ! For now, create a single plot as placeholder
    call plot(x2, x2**2)
    call title('Placeholder - Will be 1x3 Subplot Layout')
    call xlabel('x')
    call ylabel('y')
    call savefig('output/example/fortran/subplot_demo/subplot_1x3_demo.png')

    print *, ''
    print *, 'Subplot demo complete!'
    print *, 'Output files (placeholders until subplot implementation):'
    print *, '  output/example/fortran/subplot_demo/subplot_2x2_demo.png'
    print *, '  output/example/fortran/subplot_demo/subplot_1x3_demo.png'
    print *, ''
    print *, 'Once subplot() is implemented, this example will demonstrate:'
    print *, '- Independent subplot titles and axis labels'
    print *, '- Proper subplot positioning and layout'
    print *, '- Multiple plot types within subplot grids'

end program subplot_demo