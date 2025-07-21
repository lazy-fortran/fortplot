program debug_subplot
    use iso_fortran_env, only: real64, wp => real64
    use fortplot, only: figure_t
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(10), y(10)
    integer :: i, j
    
    ! Simple test data
    do i = 1, 10
        x(i) = real(i, wp)
        y(i) = real(i, wp)
    end do
    
    ! Test 1: Simple 2x1 subplot
    call fig%initialize(800, 400)
    call fig%subplots(1, 2)
    
    ! Add plot to first subplot
    call fig%subplot_plot(1, 1, x, y, label='Linear')
    call fig%subplot_set_title(1, 1, 'First Plot')
    
    ! Add plot to second subplot  
    call fig%subplot_plot(1, 2, x, y**2, label='Quadratic')
    call fig%subplot_set_title(1, 2, 'Second Plot')
    
    ! Save the figure
    call fig%savefig('debug_subplot_test.png')
    
    ! Test 2: 2x2 subplot
    call fig%initialize(800, 800)
    call fig%subplots(2, 2)
    
    do i = 1, 2
        do j = 1, 2
            call fig%subplot_plot(i, j, x, x**(real(i+j-1, wp)), label='Power')
            call fig%subplot_set_title(i, j, 'Subplot ' // char(48+i) // ',' // char(48+j))
        end do
    end do
    
    call fig%savefig('debug_subplot_2x2.png')
    
end program debug_subplot