module fortplot_figure_streamlines
    !! Figure streamline functionality module
    !!
    !! Single Responsibility: Handle streamline plotting functionality
    !! Extracted from fortplot_figure_core to improve modularity

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, PLOT_TYPE_LINE
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_scales, only: apply_scale_transform
    use fortplot_streamplot_arrow_utils, only: compute_streamplot_arrows, &
                                               map_grid_index_to_coord, &
                                               replace_stream_arrows, &
                                               validate_streamplot_arrow_parameters
    implicit none

    private
    public :: clear_streamline_data
    public :: streamplot_basic_validation, streamplot_figure

contains

    function streamplot_basic_validation(x, y, u, v) result(is_valid)
        !! Basic validation for streamplot inputs
        real(wp), contiguous, intent(in) :: x(:), y(:), u(:, :), v(:, :)
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
                                 density, color, linewidth, rtol, atol, max_time, &
                                 arrowsize, arrowstyle)
                !! Add streamline plot to figure - direct streamline generation
        !! Streamplot draws arrowheads along streamlines (not full trajectory lines).
        !! Arrow rendering happens in render_axes_and_plots after all plots.
        use fortplot_streamplot_matplotlib, only: streamplot_matplotlib

        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), contiguous, intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth, rtol, atol, max_time
        real(wp), intent(in), optional :: arrowsize
        character(len=*), intent(in), optional :: arrowstyle

        real(wp) :: plot_density, arrow_size_val
        real(wp), allocatable :: trajectories(:, :, :)
        integer :: n_trajectories
        integer, allocatable :: trajectory_lengths(:)
        type(arrow_data_t), allocatable :: computed_arrows(:)
        character(len=10) :: arrow_style_val
        logical :: arrow_params_error
        real(wp) :: line_color(3)
        real(wp) :: line_width_val
        integer :: i, j, traj_idx

        ! Basic validation
        if (.not. streamplot_basic_validation(x, y, u, v)) then
            state%has_error = .true.
            return
        end if

        ! Set default parameters
        plot_density = 1.0_wp
        if (present(density)) plot_density = density

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

        ! Validate and set arrow parameters
        call validate_streamplot_arrow_parameters(arrowsize, arrowstyle, &
                                                  arrow_size_val, arrow_style_val, &
                                                  arrow_params_error)
        if (arrow_params_error) then
            state%has_error = .true.
            return
        end if

        ! Update data ranges for streamplot.
        ! Arrow-only mode adds no plot entries, so the usual data-range update
        ! after add_plot never runs; mirror x_min/x_max into the transformed
        ! ranges that the rendering pipeline consumes.
        if (.not. state%xlim_set) then
            state%x_min = minval(x)
            state%x_max = maxval(x)
        end if
        if (.not. state%ylim_set) then
            state%y_min = minval(y)
            state%y_max = maxval(y)
        end if
        state%x_min_transformed = apply_scale_transform(state%x_min, &
            state%xscale, state%symlog_threshold)
        state%x_max_transformed = apply_scale_transform(state%x_max, &
            state%xscale, state%symlog_threshold)
        state%y_min_transformed = apply_scale_transform(state%y_min, &
            state%yscale, state%symlog_threshold)
        state%y_max_transformed = apply_scale_transform(state%y_max, &
            state%yscale, state%symlog_threshold)

        ! Generate streamlines using matplotlib algorithm
        call streamplot_matplotlib(x, y, u, v, plot_density, trajectories, &
                                   n_trajectories, trajectory_lengths, rtol, &
                                   atol, max_time)

        ! Generate arrows if requested
        if (arrow_size_val > 0.0_wp .and. n_trajectories > 0) then
            call compute_streamplot_arrows(trajectories, n_trajectories, &
                                           trajectory_lengths, x, y, arrow_size_val, &
                                           arrow_style_val, computed_arrows)
            call replace_stream_arrows(state, computed_arrows)
        end if

        ! Always draw trajectory lines; matplotlib's streamplot shows the
        ! lines and decorates them with arrowheads, it does not swap one
        ! for the other.
        line_width_val = -1.0_wp
        if (present(linewidth)) line_width_val = linewidth
        line_color = [0.0_wp, 0.447_wp, 0.698_wp]
        if (present(color)) line_color = color

        do i = 1, n_trajectories
            if (trajectory_lengths(i) <= 1) cycle

            plot_count = plot_count + 1
            traj_idx = plot_count

            if (traj_idx > size(plots)) exit

            plots(traj_idx)%plot_type = PLOT_TYPE_LINE

            allocate (plots(traj_idx)%x(trajectory_lengths(i)))
            allocate (plots(traj_idx)%y(trajectory_lengths(i)))
            do j = 1, trajectory_lengths(i)
                plots(traj_idx)%x(j) = map_grid_index_to_coord(trajectories(i, j, 1), x)
                plots(traj_idx)%y(j) = map_grid_index_to_coord(trajectories(i, j, 2), y)
            end do

            plots(traj_idx)%linestyle = '-'
            plots(traj_idx)%marker = ''
            plots(traj_idx)%color = line_color
            plots(traj_idx)%line_width = line_width_val
            plots(traj_idx)%is_streamline = .true.
        end do
    end subroutine streamplot_figure

end module fortplot_figure_streamlines
