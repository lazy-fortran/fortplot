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
    call test_streamplot_no_arrows_draws_lines()

contains

    subroutine test_basic_streamplot()
        !! Default streamplot generates arrows (not trajectory lines).
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

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v)

        ! Default streamplot generates arrows, not trajectory lines
        if (.not. allocated(fig%state%stream_arrows)) then
            print *, "ERROR: Expected streamplot arrows to be generated"
            stop 1
        end if

        if (size(fig%state%stream_arrows) == 0) then
            print *, "ERROR: Streamplot arrows array is empty"
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
        !! Streamplot with arrows at different densities generates different
        !! arrow counts.
        type(figure_t) :: fig
        real(dp), dimension(3) :: x, y
        real(dp), dimension(3, 3) :: u, v
        integer :: n_arrows1, n_arrows2

        call setup_test_grid_3x3(x, y, u, v)

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, density=0.5_dp)
        n_arrows1 = size(fig%state%stream_arrows)

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, density=2.0_dp)
        n_arrows2 = size(fig%state%stream_arrows)

        if (n_arrows1 <= 0 .or. n_arrows2 <= 0) then
            print *, "ERROR: Expected streamplot to generate streamlines"
            stop 1
        end if

        if (n_arrows2 <= n_arrows1) then
            print *, "ERROR: Expected higher density to generate more streamlines"
            stop 1
        end if
    end subroutine test_streamplot_density_parameter

    subroutine test_streamplot_linewidth_parameter()
        !! When arrowsize=0, streamplot draws trajectory lines with specified
        !! linewidth.
        type(figure_t) :: fig
        real(dp), dimension(3) :: x, y
        real(dp), dimension(3, 3) :: u, v

        call setup_test_grid_3x3(x, y, u, v)

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, linewidth=2.5_dp, arrowsize=0.0_dp)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected streamplot with linewidth to generate plots"
            stop 1
        end if
        if (fig%plots(1)%line_width /= 2.5_dp) then
            print *, "ERROR: Expected streamplot linewidth to be stored per plot"
            stop 1
        end if

        ! Default streamplot draws trajectory lines decorated with
        ! arrowheads, matching matplotlib.
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected default streamplot to draw trajectory lines"
            stop 1
        end if
    end subroutine test_streamplot_linewidth_parameter

    subroutine test_streamplot_max_time_parameter()
        !! When arrowsize=0, max_time controls trajectory point count.
        type(figure_t) :: fig
        real(dp), dimension(3) :: x, y
        real(dp), dimension(3, 3) :: u, v
        integer :: plot_idx
        integer :: total_points_default, total_points_short

        call setup_test_grid_3x3(x, y, u, v)

        ! Default (arrows) - no trajectory plots
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v)

        ! With max_time and arrowsize=0 - shorter trajectories
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, max_time=0.2_dp, arrowsize=0.0_dp)
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

        ! Longer max_time should produce more points
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, max_time=1.0_dp, arrowsize=0.0_dp)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected streamplot with longer max_time to generate plots"
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

        if (total_points_short >= total_points_default) then
            print *, "ERROR: Expected max_time to reduce integrated streamline points"
            stop 1
        end if
    end subroutine test_streamplot_max_time_parameter

    subroutine test_streamplot_rtol_parameter()
        !! When arrowsize=0, rtol controls per-trajectory sampling density.
        !! Trajectory acceptance follows matplotlib's arc-length minlength, so
        !! the count of accepted trajectories is governed by mask collisions,
        !! not rtol. The genuine rtol effect is finer per-trajectory sampling:
        !! a stricter rtol takes smaller steps and yields more points per
        !! streamline. Assert on points-per-trajectory, not the raw figure
        !! total (which the number of accepted trajectories confounds).
        type(figure_t) :: fig
        real(dp), dimension(3) :: x, y
        real(dp), dimension(3, 3) :: u, v
        real(dp) :: density_strict, density_lenient

        call setup_test_grid_3x3(x, y, u, v)

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, rtol=1.0e-9_dp, max_time=0.5_dp, &
                            arrowsize=0.0_dp)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected strict rtol streamplot to generate plots"
            stop 1
        end if
        density_strict = mean_points_per_trajectory(fig)

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, rtol=1.0e-3_dp, max_time=0.5_dp, &
                            arrowsize=0.0_dp)
        if (fig%plot_count <= 0) then
            print *, "ERROR: Expected lenient rtol streamplot to generate plots"
            stop 1
        end if
        density_lenient = mean_points_per_trajectory(fig)

        if (density_strict <= density_lenient) then
            print *, "ERROR: Expected smaller rtol to increase per-streamline points"
            stop 1
        end if
    end subroutine test_streamplot_rtol_parameter

    function mean_points_per_trajectory(fig) result(mean_points)
        !! Average number of points across accepted streamlines.
        type(figure_t), intent(in) :: fig
        real(dp) :: mean_points
        integer :: plot_idx, total_points

        total_points = 0
        do plot_idx = 1, fig%plot_count
            if (.not. allocated(fig%plots(plot_idx)%x)) then
                print *, "ERROR: Expected streamline x data to be allocated"
                stop 1
            end if
            total_points = total_points + size(fig%plots(plot_idx)%x)
        end do
        mean_points = real(total_points, dp) / real(fig%plot_count, dp)
    end function mean_points_per_trajectory

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

    subroutine test_streamplot_no_arrows_draws_lines()
        !! When arrowsize=0, streamplot draws trajectory lines (not arrows).
        type(figure_t) :: fig
        real(dp), dimension(5) :: x = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
        real(dp), dimension(4) :: y = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp]
        real(dp), dimension(5, 4) :: u, v
        integer :: plot_idx
        logical :: found_nonempty_streamline
        integer :: ii, jj

        do jj = 1, 4
            do ii = 1, 5
                u(ii, jj) = 1.0_dp
                v(ii, jj) = 0.0_dp
            end do
        end do

        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, arrowsize=0.0_dp)

        if (fig%plot_count == 0) then
            print *, "ERROR: No plots generated from streamplot (no arrows)"
            stop 1
        end if

        ! No arrows when arrowsize=0
        if (allocated(fig%state%stream_arrows) .and. size(fig%state%stream_arrows) > 0) then
            print *, "ERROR: Expected no streamplot arrows when arrowsize=0"
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
    end subroutine test_streamplot_no_arrows_draws_lines

end program test_streamplot
