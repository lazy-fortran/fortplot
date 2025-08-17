program subplot_demo
    !! Demonstration of subplot functionality
    
    use iso_fortran_env, only: real64, wp => real64
    use fortplot, only: figure_t
    implicit none
    
    type(figure_t) :: fig
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
    
    ! Create 2x2 subplot grid
    call fig%initialize(800, 600)
    call fig%subplots(2, 2)
    
    ! Subplot (1,1) - Sine wave
    call fig%subplot_set_title(1, 1, 'Sine Wave')
    call fig%subplot_set_xlabel(1, 1, 'x')
    call fig%subplot_set_ylabel(1, 1, 'sin(x)')
    call fig%subplot_plot(1, 1, x, y, label='sin(x)')
    
    ! Subplot (1,2) - Cosine wave
    call fig%subplot_set_title(1, 2, 'Cosine Wave')
    call fig%subplot_set_xlabel(1, 2, 'x')
    call fig%subplot_set_ylabel(1, 2, 'cos(x)')
    call fig%subplot_plot(1, 2, x, cos(x), label='cos(x)')
    
    ! Subplot (2,1) - Damped oscillation
    call fig%subplot_set_title(2, 1, 'Damped Oscillation')
    call fig%subplot_set_xlabel(2, 1, 'x')
    call fig%subplot_set_ylabel(2, 1, 'y')
    call fig%subplot_plot(2, 1, x2, y2, label='exp(-x/2)*cos(2x)')
    
    ! Subplot (2,2) - Quadratic
    call fig%subplot_set_title(2, 2, 'Quadratic Function')
    call fig%subplot_set_xlabel(2, 2, 'x')
    call fig%subplot_set_ylabel(2, 2, 'y')
    call fig%subplot_plot(2, 2, x2, x2**2 / 50.0_wp, label='x^2')
    
    ! Save the figure
    call fig%savefig('build/example/subplot_demo/subplot_demo.png')
    
    ! Create another example with different layout
    call fig%initialize(800, 400)
    call fig%subplots(1, 3)
    
    ! Three plots side by side
    call fig%subplot_set_title(1, 1, 'Linear')
    call fig%subplot_plot(1, 1, x2, x2)
    
    call fig%subplot_set_title(1, 2, 'Quadratic')
    call fig%subplot_plot(1, 2, x2, x2**2)
    
    call fig%subplot_set_title(1, 3, 'Cubic')
    call fig%subplot_plot(1, 3, x2, x2**3)
    
    call fig%savefig('build/example/subplot_demo/subplot_layout.png')
    
end program subplot_demo