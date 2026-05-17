program test_streamplot
    use fortplot_streamplot_core, only: setup_streamplot_parameters
    use fortplot, only: figure_t
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    call test_basic_streamplot()
    call test_streamplot_density_parameter()
    call test_streamplot_linewidth_parameter()
    call test_streamplot_max_time_parameter()
    call test_streamplot_rtol_parameter()
    call test_streamplot_grid_validation()
    call test_streamplot_arrow_clearing()

contains

    subroutine test_basic_streamplot()
        type(figure_t) :: fig
        real(dp), dimension(5) :: x = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
        real(dp), dimension(4) :: y = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp]
        real(dp), dimension(5, 4) :: u, v
        integer :: i, j
        integer :: plot_idx
        logical :: found_nonempty_streamline

        do j = 1, 4
            do i = 1, 5
                u(i, j) = 1.0_dp
                v(i, j) = 0.0_dp
            end do
        end do

        call fig%initialize(800, 600)

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

        found_nonempty_streamline = .false.
        do plot_idx = 1, fig%plot_count
            if (allocated(fig%plots(plot_idx)%x)) then
                if (size(fig%plots(plot_idx)%x) > 0) then
                    found_nonempty_streamline = .true.
                    exit
                end if
            end if
        end do

        if (.not. found_nonempty_streamline) then
            print *, "ERROR: Expected at least one non-empty streamline"
            stop 1
        end if
    end subroutine

    subroutine setup_test_grid_3x3(x, y, u, v)
        real(dp), dimension(3), intent(out) :: x, y
        real(dp), dimension(3, 3), intent(out) :: u, v
        integer :: i, j

        x = [0.0_dp, 1.0_dp, 2.0_dp]
        y = [0.0_dp, 1.0_dp, 2.0_dp]

        do j = 1, 3
            do i = 1, 3
                u(i, j) = real(i, dp)
                v(i, j) = real(j, dp)
            end do
        end do
    end subroutine setup_test_grid_3x3

    subroutine test_streamplot_density_parameter()
        type(figure_t) :: fig
        real(dp), dimension(3) :: x, y
        real(dp), dimension(3, 3) :: u, v
        integer :: n_streamlines1, n_streamlines2

        call setup_test_grid_3x3(x, y, u, v)

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, density=0.5_dp)
        n_streamlines1 = fig%plot_count

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, density=2.0_dp)
        n_streamlines2 = fig%plot_count

        if (n_streamlines1 <= 0 .or. n_streamlines2 <= 0) then
            print *, "ERROR: Expected streamplot to generate streamlines"
            stop 1
        end if

        if (n_streamlines2 <= n_streamlines1) then
            print *, "ERROR: Expected higher density to generate more streamlines"
            stop 1
        end if
    end subroutine test_streamplot_density_parameter

    subroutine test_streamplot_linewidth_parameter()
        type(figure_t) :: fig
        real(dp), dimension(3) :: x, y
        real(dp), dimension(3, 3) :: u, v

        call setup_test_grid_3x3(x, y, u, v)

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, linewidth=2.5_dp)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected streamplot with linewidth to generate plots"
            stop 1
        end if
        if (fig%plots(1)%line_width /= 2.5_dp) then
            print *, "ERROR: Expected streamplot linewidth to be stored per plot"
            stop 1
        end if

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected default streamplot to generate plots"
            stop 1
        end if
        if (fig%plots(1)%line_width > 0.0_dp) then
            print *, "ERROR: Expected streamplot linewidth to not leak into other plots"
            stop 1
        end if
    end subroutine test_streamplot_linewidth_parameter

    subroutine test_streamplot_max_time_parameter()
        type(figure_t) :: fig
        real(dp), dimension(3) :: x, y
        real(dp), dimension(3, 3) :: u, v
        integer :: plot_idx
        integer :: total_points_default, total_points_short

        call setup_test_grid_3x3(x, y, u, v)

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected default streamplot to generate plots"
            stop 1
        end if

        total_points_default = 0
        do plot_idx = 1, fig%plot_count
            if (.not. allocated(fig%plots(plot_idx)%x)) then
                print *, "ERROR: Expected streamline x data to be allocated"
                stop 1
            end if
            total_points_default = total_points_default + size(fig%plots(plot_idx)%x)
        end do

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, max_time=0.2_dp)
        if (fig%state%has_error) then
            print *, "ERROR: Unexpected error for valid max_time"
            stop 1
        end if
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected streamplot with max_time to generate plots"
            stop 1
        end if

        total_points_short = 0
        do plot_idx = 1, fig%plot_count
            if (.not. allocated(fig%plots(plot_idx)%x)) then
                print *, "ERROR: Expected streamline x data to be allocated"
                stop 1
            end if
            total_points_short = total_points_short + size(fig%plots(plot_idx)%x)
        end do

        if (total_points_short >= total_points_default) then
            print *, "ERROR: Expected max_time to reduce integrated streamline points"
            stop 1
        end if
    end subroutine test_streamplot_max_time_parameter

    subroutine test_streamplot_rtol_parameter()
        type(figure_t) :: fig
        real(dp), dimension(3) :: x, y
        real(dp), dimension(3, 3) :: u, v
        integer :: plot_idx
        integer :: total_points_strict, total_points_lenient

        call setup_test_grid_3x3(x, y, u, v)

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, rtol=1.0e-9_dp, max_time=0.5_dp)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected strict rtol streamplot to generate plots"
            stop 1
        end if

        total_points_strict = 0
        do plot_idx = 1, fig%plot_count
            if (.not. allocated(fig%plots(plot_idx)%x)) then
                print *, "ERROR: Expected streamline x data to be allocated"
                stop 1
            end if
            total_points_strict = total_points_strict + size(fig%plots(plot_idx)%x)
        end do

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, rtol=1.0e-3_dp, max_time=0.5_dp)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected lenient rtol streamplot to generate plots"
            stop 1
        end if

        total_points_lenient = 0
        do plot_idx = 1, fig%plot_count
            if (.not. allocated(fig%plots(plot_idx)%x)) then
                print *, "ERROR: Expected streamline x data to be allocated"
                stop 1
            end if
            total_points_lenient = total_points_lenient + size(fig%plots(plot_idx)%x)
        end do

        if (total_points_strict <= total_points_lenient) then
            print *, "ERROR: Expected smaller rtol to increase integration points"
            stop 1
        end if
    end subroutine test_streamplot_rtol_parameter

    subroutine test_streamplot_grid_validation()
        type(figure_t) :: fig
        real(dp), dimension(3) :: x = [0.0_dp, 1.0_dp, 2.0_dp]
        real(dp), dimension(4) :: y = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp]
        real(dp), dimension(3, 3) :: u, v

        u = 1.0_dp
        v = 0.0_dp

        call fig%initialize(800, 600)

        call fig%streamplot(x, y, u, v)

        if (.not. fig%state%has_error) then
            print *, "ERROR: Should detect grid size mismatch"
            stop 1
        end if
    end subroutine

    subroutine test_streamplot_arrow_clearing()
        type(figure_t) :: fig
        real(dp), dimension(5) :: x = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
        real(dp), dimension(4) :: y = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp]
        real(dp), dimension(5, 4) :: u, v
        integer :: i, j

        do j = 1, 4
            do i = 1, 5
                u(i, j) = 1.0_dp
                v(i, j) = 0.0_dp
            end do
        end do

        call fig%initialize(400, 300)
        call setup_streamplot_parameters(fig, x, y, u, v)
        if (.not. allocated(fig%state%stream_arrows)) then
            print *, "ERROR: Expected stream arrows after default streamplot"
            stop 1
        end if

        call setup_streamplot_parameters(fig, x, y, u, v, arrowsize=0.0_dp)
        if (allocated(fig%state%stream_arrows)) then
            print *, "ERROR: Stream arrows not cleared when arrowsize=0"
            stop 1
        end if
    end subroutine test_streamplot_arrow_clearing

end program test_streamplot
