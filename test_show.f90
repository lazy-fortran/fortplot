program test_show
    use fortplot_figure, only: figure_t
    implicit none

    type(figure_t) :: fig
    real :: x(100), y(100)
    integer :: i

    ! Generate simple test data
    do i = 1, 100
        x(i) = real(i) / 10.0
        y(i) = sin(x(i))
    end do

    ! Test PNG backend with terminal display
    print *, "Testing PNG backend show()..."
    call fig%init(backend='PNG')
    call fig%plot(x, y)
    call fig%show()  ! Should launch viewer or log message

    ! Test PDF backend with terminal display
    print *, "Testing PDF backend show()..."
    call fig%init(backend='PDF')
    call fig%plot(x, y)
    call fig%show()  ! Should launch viewer or log message

    ! Test ASCII backend with terminal display
    print *, "Testing ASCII backend show()..."
    call fig%init(backend='ASCII')
    call fig%plot(x, y)
    call fig%show()  ! Should output to terminal

    print *, "Tests complete"

end program test_show