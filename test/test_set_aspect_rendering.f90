program test_set_aspect_rendering
    use fortplot_figure, only: figure_t
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    real(wp), parameter :: PI = 3.141592653589793_wp
    real(wp), allocatable :: theta(:), x(:), y(:)
    type(figure_t) :: fig
    integer :: i, n, status

    n = 100
    allocate(theta(n), x(n), y(n))
    do i = 1, n
        theta(i) = 2.0_wp * PI * real(i - 1, wp) / real(n - 1, wp)
        x(i) = cos(theta(i))
        y(i) = sin(theta(i))
    end do

    call fig%initialize(width=600, height=400)
    call fig%add_plot(x, y)
    call fig%set_title('Equal Aspect - Circle should appear circular')
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call fig%set_aspect('equal')
    call fig%savefig_with_status('test/output/test_aspect_equal.png', status)

    if (status /= 0) then
        print *, "FAIL: Could not save equal aspect PNG"
        stop 1
    end if

    call fig%clear()
    call fig%add_plot(x, y)
    call fig%set_title('Auto Aspect - Circle may appear elliptical')
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call fig%set_aspect('auto')
    call fig%savefig_with_status('test/output/test_aspect_auto.png', status)

    if (status /= 0) then
        print *, "FAIL: Could not save auto aspect PNG"
        stop 1
    end if

    call fig%clear()
    call fig%add_plot(x, y)
    call fig%set_title('Numeric Aspect (2.0) - Vertical stretch')
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call fig%set_aspect(2.0_wp)
    call fig%savefig_with_status('test/output/test_aspect_numeric.png', status)

    if (status /= 0) then
        print *, "FAIL: Could not save numeric aspect PNG"
        stop 1
    end if

    print *, "All set_aspect rendering tests passed!"
    print *, "Visual outputs saved to test/output/test_aspect_*.png"

end program test_set_aspect_rendering
