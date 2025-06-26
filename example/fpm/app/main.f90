program fortplotlib_example
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(10), y(10)
    integer :: i
    
    ! Generate simple data
    do i = 1, 10
        x(i) = real(i, wp)
        y(i) = real(i, wp)**2
    end do
    
    ! Create plot
    call fig%initialize(640, 480)
    call fig%add_plot(x, y)
    call fig%savefig('example.png')
    
    print *, "Created example.png"
end program fortplotlib_example