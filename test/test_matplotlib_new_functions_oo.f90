program test_matplotlib_new_functions_oo
    !! Validate OO API coverage for matplotlib-compatible helpers
    use fortplot
    use iso_fortran_env, only: real64
    implicit none

    type(figure_t) :: fig
    real(real64), allocatable :: x(:), y(:), z(:,:)
    real(real64), allocatable :: theta(:), r(:)
    real(real64), allocatable :: values(:)
    integer :: i, j

    print *, 'Testing OO matplotlib-compatible functions...'

    call fig%initialize()

    allocate(z(50, 50))
    do i = 1, 50
        do j = 1, 50
            z(i, j) = sin(real(i, real64) / 5.0_real64) * &
                      cos(real(j, real64) / 5.0_real64)
        end do
    end do
    call fig%imshow(z)
    call fig%set_title('OO imshow() test - 2D heatmap')
    call fig%savefig('test_imshow_oo.png')
    call fig%clear()

    allocate(values(5))
    values = [30.0_real64, 25.0_real64, 20.0_real64, 15.0_real64, 10.0_real64]
    call fig%pie(values)
    call fig%set_title('OO pie() test - Pie chart')
    call fig%savefig('test_pie_oo.png')
    call fig%clear()

    allocate(theta(100), r(100))
    do i = 1, 100
        theta(i) = 2.0_real64 * 3.14159_real64 * real(i - 1, real64) / 99.0_real64
        r(i) = 1.0_real64 + 0.5_real64 * sin(5.0_real64 * theta(i))
    end do
    call fig%polar(theta, r)
    call fig%set_title('OO polar() test - Polar plot')
    call fig%savefig('test_polar_oo.png')
    call fig%clear()

    allocate(x(20), y(20))
    do i = 1, 20
        x(i) = real(i, real64)
        y(i) = sin(real(i, real64) / 3.0_real64)
    end do
    call fig%step(x, y)
    call fig%set_title('OO step() test - Step plot')
    call fig%savefig('test_step_oo.png')
    call fig%clear()

    call fig%stem(x, y)
    call fig%set_title('OO stem() test - Stem plot')
    call fig%savefig('test_stem_oo.png')
    call fig%clear()

    call fig%fill(x, y)
    call fig%set_title('OO fill() test - Area under curve')
    call fig%savefig('test_fill_oo.png')
    call fig%clear()

    deallocate(x, y)
    allocate(x(50), y(50))
    do i = 1, 50
        x(i) = real(i - 25, real64) / 5.0_real64
        y(i) = x(i)**2
    end do
    call fig%fill_between(x, y1=y, y2=y * 0.5_real64)
    call fig%set_title('OO fill_between() test - Area between curves')
    call fig%savefig('test_fill_between_oo.png')
    call fig%clear()

    call fig%step(x, y)
    call fig%twinx()
    call fig%twiny()
    call fig%set_title('OO twin axis placeholder test')
    call fig%savefig('test_twin_axes_oo.png')

    print *, '✓ OO matplotlib-compatible functions executed without errors'

    deallocate(x, y, z, theta, r, values)

end program test_matplotlib_new_functions_oo
