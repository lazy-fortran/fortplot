program test_streamplot
    use fortplot
    use fortplot_streamplot_core, only: setup_streamplot_parameters
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none

    print *, "Starting streamplot tests with Windows compatibility..."

    call test_basic_streamplot()
    call test_streamplot_parameters()
    call test_streamplot_grid_validation()
    call test_streamplot_arrow_clearing()

    print *, "All streamplot tests passed!"

contains

    subroutine test_basic_streamplot()
        type(figure_t) :: fig
        real(real64), dimension(5) :: x = [0.0, 1.0, 2.0, 3.0, 4.0]
        real(real64), dimension(4) :: y = [0.0, 1.0, 2.0, 3.0]
        real(real64), dimension(5, 4) :: u, v
        integer :: i, j

        print *, "Test 1: Basic streamplot initialization"

        do j = 1, 4
            do i = 1, 5
                u(i, j) = 1.0
                v(i, j) = 0.0
            end do
        end do

        print *, "  Initializing figure [800x600]..."
        call fig%initialize(800, 600)

        print *, "  Creating streamplot..."
        call fig%streamplot(x, y, u, v)

        if (fig%plot_count == 0) then
            print *, "ERROR: No plots generated from streamplot"
            stop 1
        end if

        if (.not. allocated(fig%state%stream_arrows)) then
            print *, "ERROR: Expected streamplot arrows to be generated"
            stop 1
        end if

        if (size(fig%state%stream_arrows) == 0) then
            print *, "ERROR: Streamplot arrows array is empty"
            stop 1
        end if
        ! For now, just check that streamlines were allocated
    end subroutine

    subroutine test_streamplot_parameters()
        type(figure_t) :: fig
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(3) :: y = [0.0, 1.0, 2.0]
        real(real64), dimension(3, 3) :: u, v
        integer :: i, j, n_streamlines1, n_streamlines2
        real(real64) :: max_x_default, max_x_short
        integer :: n_points_strict, n_points_lenient

        do j = 1, 3
            do i = 1, 3
                u(i, j) = real(i)
                v(i, j) = real(j)
            end do
        end do

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, density=0.5_real64)
        n_streamlines1 = fig%plot_count

        ! Reset figure for second test
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, density=2.0_real64)
        n_streamlines2 = fig%plot_count

        if (n_streamlines1 <= 0 .or. n_streamlines2 <= 0) then
            print *, "ERROR: Expected streamplot to generate streamlines"
            stop 1
        end if

        if (n_streamlines2 <= n_streamlines1) then
            print *, "ERROR: Expected higher density to generate more streamlines"
            stop 1
        end if

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, linewidth=2.5_real64)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected streamplot with linewidth to generate plots"
            stop 1
        end if
        if (fig%plots(1)%line_width /= 2.5_real64) then
            print *, "ERROR: Expected streamplot linewidth to be stored per plot"
            stop 1
        end if

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected default streamplot to generate plots"
            stop 1
        end if
        max_x_default = maxval(fig%plots(1)%x)

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, max_time=0.2_real64)
        if (fig%state%has_error) then
            print *, "ERROR: Unexpected error for valid max_time"
            stop 1
        end if
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected streamplot with max_time to generate plots"
            stop 1
        end if
        max_x_short = maxval(fig%plots(1)%x)

        if (max_x_short >= max_x_default - 1.0e-6_real64) then
            print *, "ERROR: Expected max_time to shorten streamlines"
            stop 1
        end if

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, rtol=1.0e-9_real64, max_time=0.5_real64)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected strict rtol streamplot to generate plots"
            stop 1
        end if
        n_points_strict = size(fig%plots(1)%x)

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, rtol=1.0e-3_real64, max_time=0.5_real64)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected lenient rtol streamplot to generate plots"
            stop 1
        end if
        n_points_lenient = size(fig%plots(1)%x)

        if (n_points_strict <= n_points_lenient) then
            print *, "ERROR: Expected smaller rtol to increase integration resolution"
            stop 1
        end if
    end subroutine

    subroutine test_streamplot_grid_validation()
        type(figure_t) :: fig
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(4) :: y = [0.0, 1.0, 2.0, 3.0]
        real(real64), dimension(3, 3) :: u, v
        logical :: error_caught

        u = 1.0
        v = 0.0

        call fig%initialize(800, 600)

        error_caught = .false.
        call fig%streamplot(x, y, u, v)

        if (.not. fig%state%has_error) then
            print *, "ERROR: Should detect grid size mismatch"
            stop 1
        end if
    end subroutine

    subroutine test_streamplot_arrow_clearing()
        type(figure_t) :: fig
        real(real64), dimension(5) :: x = [0.0, 1.0, 2.0, 3.0, 4.0]
        real(real64), dimension(4) :: y = [0.0, 1.0, 2.0, 3.0]
        real(real64), dimension(5, 4) :: u, v
        integer :: i, j

        do j = 1, 4
            do i = 1, 5
                u(i, j) = 1.0_real64
                v(i, j) = 0.0_real64
            end do
        end do

        call fig%initialize(400, 300)
        call setup_streamplot_parameters(fig, x, y, u, v)
        if (.not. allocated(fig%state%stream_arrows)) then
            print *, "ERROR: Expected stream arrows after default streamplot"
            stop 1
        end if

        call setup_streamplot_parameters(fig, x, y, u, v, arrowsize=0.0_real64)
        if (allocated(fig%state%stream_arrows)) then
            print *, "ERROR: Stream arrows not cleared when arrowsize=0"
            stop 1
        end if
    end subroutine test_streamplot_arrow_clearing

end program test_streamplot
