module fortplot_figure_streamlines
    !! Figure streamline functionality module
    !!
    !! Single Responsibility: Handle streamline plotting functionality
    !! Extracted from fortplot_figure_core to improve modularity

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, arrow_data_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_streamplot_arrow_utils, only: compute_streamplot_arrows, &
                                               map_grid_index_to_coord, &
                                               replace_stream_arrows
    implicit none

    private
    public :: clear_streamline_data
    public :: streamplot_basic_validation, streamplot_figure

contains

    function streamplot_basic_validation(x, y, u, v) result(is_valid)
        !! Basic validation for streamplot inputs
        real(wp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
        logical :: is_valid

        is_valid = .true.

        ! Validate dimensions
        if (size(u, 1) /= size(x) .or. size(u, 2) /= size(y)) then
            is_valid = .false.
            return
        end if

        if (size(v, 1) /= size(x) .or. size(v, 2) /= size(y)) then
            is_valid = .false.
            return
        end if
    end function streamplot_basic_validation

    subroutine clear_streamline_data(streamlines)
        !! Clear streamline data
        type(plot_data_t), allocatable, intent(inout) :: streamlines(:)
        type(plot_data_t), allocatable :: old(:)
        if (allocated(streamlines)) then
            call move_alloc(streamlines, old)
        end if
    end subroutine clear_streamline_data

    subroutine streamplot_figure(plots, state, plot_count, x, y, u, v, &
                                 density, color, linewidth, rtol, atol, max_time)
                !! Add streamline plot to figure - direct streamline generation
        use fortplot_streamplot_matplotlib, only: streamplot_matplotlib
        use fortplot_plot_data, only: PLOT_TYPE_LINE

        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth, rtol, atol, max_time

        real(wp) :: plot_density, line_color(3), line_width_val
        real(wp), allocatable :: trajectories(:, :, :)
        integer :: n_trajectories, i, j, plot_idx
        integer, allocatable :: trajectory_lengths(:)
        type(arrow_data_t), allocatable :: computed_arrows(:)
        real(wp), parameter :: default_arrow_size = 1.0_wp
        character(len=2), parameter :: default_arrow_style = '->'

        ! Basic validation
        if (.not. streamplot_basic_validation(x, y, u, v)) then
            state%has_error = .true.
            return
        end if

        ! Set default parameters
        plot_density = 1.0_wp
        if (present(density)) plot_density = density

        line_width_val = -1.0_wp
        if (present(linewidth)) line_width_val = linewidth
        if (present(linewidth)) then
            if (linewidth <= 0.0_wp) then
                state%has_error = .true.
                return
            end if
        end if
        if (present(rtol)) then
            if (rtol <= 0.0_wp) then
                state%has_error = .true.
                return
            end if
        end if
        if (present(atol)) then
            if (atol <= 0.0_wp) then
                state%has_error = .true.
                return
            end if
        end if
        if (present(max_time)) then
            if (max_time <= 0.0_wp) then
                state%has_error = .true.
                return
            end if
        end if

        line_color = [0.0_wp, 0.447_wp, 0.698_wp]  ! Default blue
        if (present(color)) line_color = color

        ! Update data ranges for streamplot
        if (.not. state%xlim_set) then
            state%x_min = minval(x)
            state%x_max = maxval(x)
        end if
        if (.not. state%ylim_set) then
            state%y_min = minval(y)
            state%y_max = maxval(y)
        end if

        ! Generate streamlines using matplotlib algorithm
        call streamplot_matplotlib(x, y, u, v, plot_density, trajectories, &
                                   n_trajectories, trajectory_lengths, rtol, &
                                   atol, max_time)

        call compute_streamplot_arrows(trajectories, n_trajectories, &
                                       trajectory_lengths, x, y, default_arrow_size, &
                                       default_arrow_style, computed_arrows)

        call replace_stream_arrows(state, computed_arrows)

        ! Add each trajectory as a line plot
        do i = 1, n_trajectories
            if (trajectory_lengths(i) <= 1) cycle

            plot_count = plot_count + 1
            plot_idx = plot_count

            if (plot_idx > size(plots)) exit  ! Safety check

            ! Set plot type and data
            plots(plot_idx)%plot_type = PLOT_TYPE_LINE

            ! Store trajectory data
            allocate (plots(plot_idx)%x(trajectory_lengths(i)))
            allocate (plots(plot_idx)%y(trajectory_lengths(i)))
            do j = 1, trajectory_lengths(i)
                plots(plot_idx)%x(j) = map_grid_index_to_coord(trajectories(i, j, 1), x)
                plots(plot_idx)%y(j) = map_grid_index_to_coord(trajectories(i, j, 2), y)
            end do

            ! Set streamline properties
            plots(plot_idx)%linestyle = '-'
            plots(plot_idx)%marker = ''
            plots(plot_idx)%color = line_color
            plots(plot_idx)%line_width = line_width_val
        end do
    end subroutine streamplot_figure

end module fortplot_figure_streamlines
