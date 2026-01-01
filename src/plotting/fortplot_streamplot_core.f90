module fortplot_streamplot_core
    !! Streamplot implementation broken down for size compliance
    !!
    !! Refactored from 253-line function into focused, testable components
    !! following SOLID principles and size constraints.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: EPSILON_COMPARE
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: arrow_data_t
    use fortplot_streamplot_matplotlib, only: streamplot_matplotlib
    use fortplot_streamplot_arrow_utils, only: &
        validate_streamplot_arrow_parameters, compute_streamplot_arrows, &
        replace_stream_arrows, map_grid_index_to_coord

    implicit none

    private
    public :: setup_streamplot_parameters, generate_streamlines, &
              add_streamline_to_figure

contains

    subroutine setup_streamplot_parameters(self, x, y, u, v, density, color, &
                                           linewidth, rtol, atol, max_time, &
                                           arrowsize, &
                                           arrowstyle)
        !! Setup and validate streamplot parameters (validation logic only)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(wp), intent(in), optional :: density, linewidth, rtol, atol, &
                                          max_time, &
                                          arrowsize
        real(wp), intent(in), optional :: color(3)
        character(len=*), intent(in), optional :: arrowstyle

        real(wp) :: plot_density, arrow_size_val
        real(wp) :: line_width_val
        character(len=10) :: arrow_style_val
        real(wp), allocatable :: trajectories(:, :, :)
        integer :: n_trajectories
        integer, allocatable :: trajectory_lengths(:)
        logical :: arrow_params_error

        ! Validate input dimensions
        if (size(u, 1) /= size(x) .or. size(u, 2) /= size(y)) then
            self%state%has_error = .true.
            return
        end if

        if (size(v, 1) /= size(x) .or. size(v, 2) /= size(y)) then
            self%state%has_error = .true.
            return
        end if

        ! Set default parameters
        plot_density = 1.0_wp
        if (present(density)) plot_density = density

        line_width_val = -1.0_wp
        if (present(linewidth)) line_width_val = linewidth

        if (present(linewidth)) then
            if (linewidth <= 0.0_wp) then
                self%state%has_error = .true.
                return
            end if
        end if
        if (present(rtol)) then
            if (rtol <= 0.0_wp) then
                self%state%has_error = .true.
                return
            end if
        end if
        if (present(atol)) then
            if (atol <= 0.0_wp) then
                self%state%has_error = .true.
                return
            end if
        end if
        if (present(max_time)) then
            if (max_time <= 0.0_wp) then
                self%state%has_error = .true.
                return
            end if
        end if

        ! Validate and set arrow parameters
        call validate_streamplot_arrow_parameters(arrowsize, arrowstyle, &
                                                  arrow_size_val, arrow_style_val, &
                                                  arrow_params_error)
        if (arrow_params_error) then
            self%state%has_error = .true.
            return
        end if

        ! Update data ranges for streamplot
        call update_streamplot_ranges(self, x, y)

        ! Generate streamlines using matplotlib algorithm
        call generate_streamlines(x, y, u, v, plot_density, trajectories, &
                                  n_trajectories, trajectory_lengths, &
                                  rtol, atol, &
                                  max_time)

        ! Always clear queued arrows before recalculating
        call self%clear_backend_arrows()

        ! Generate arrows if requested
        if (arrow_size_val > 0.0_wp .and. n_trajectories > 0) then
            call generate_streamplot_arrows(self, trajectories, n_trajectories, &
                                            trajectory_lengths, x, y, arrow_size_val, &
                                            arrow_style_val)
        end if

        ! Add trajectories to figure
        call add_trajectories_to_figure(self, trajectories, n_trajectories, &
                                        trajectory_lengths, color, x, y, &
                                        line_width_val)
    end subroutine setup_streamplot_parameters

    subroutine update_streamplot_ranges(self, x, y)
        !! Update figure data ranges for streamplot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)

        if (.not. self%state%xlim_set) then
            self%state%x_min = minval(x)
            self%state%x_max = maxval(x)
        end if
        if (.not. self%state%ylim_set) then
            self%state%y_min = minval(y)
            self%state%y_max = maxval(y)
        end if
    end subroutine update_streamplot_ranges

    subroutine generate_streamlines(x, y, u, v, density, trajectories, &
                                    n_trajectories, &
                                    trajectory_lengths, rtol, atol, max_time)
                !! Generate streamlines using matplotlib-compatible algorithm
        real(wp), intent(in) :: x(:), y(:), u(:, :), v(:, :), density
        real(wp), allocatable, intent(out) :: trajectories(:, :, :)
        integer, intent(out) :: n_trajectories
        integer, allocatable, intent(out) :: trajectory_lengths(:)
        real(wp), intent(in), optional :: rtol, atol, max_time

        ! Delegate to matplotlib implementation
        call streamplot_matplotlib(x, y, u, v, density, trajectories, &
                                   n_trajectories, &
                                   trajectory_lengths, rtol, atol, max_time)
    end subroutine generate_streamlines

    subroutine generate_streamplot_arrows(fig, trajectories, n_trajectories, &
                                          trajectory_lengths, x_grid, y_grid, &
                                          arrow_size, arrow_style)
        !! Generate one arrow per streamline using midpoint placement
        class(figure_t), intent(inout) :: fig
        real(wp), intent(in) :: trajectories(:, :, :)
        integer, intent(in) :: n_trajectories, trajectory_lengths(:)
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        real(wp), intent(in) :: arrow_size
        character(len=*), intent(in) :: arrow_style

        type(arrow_data_t), allocatable :: computed(:)

        call compute_streamplot_arrows(trajectories, n_trajectories, &
                                       trajectory_lengths, x_grid, y_grid, arrow_size, &
                                       arrow_style, &
                                       computed)

        call replace_stream_arrows(fig%state, computed)
    end subroutine generate_streamplot_arrows

    subroutine add_trajectories_to_figure(fig, trajectories, &
                                          n_trajectories, lengths, &
                                          trajectory_color, x_grid, y_grid, &
                                          line_width)
                !! Add streamline trajectories to figure as regular plots
        class(figure_t), intent(inout) :: fig
        real(wp), intent(in) :: trajectories(:, :, :)
        integer, intent(in) :: n_trajectories, lengths(:)
        real(wp), intent(in), optional :: trajectory_color(3)
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        real(wp), intent(in) :: line_width

        integer :: i
        real(wp) :: line_color(3)

        ! Set default color (blue)
        line_color = [0.0_wp, 0.447_wp, 0.698_wp]
        if (present(trajectory_color)) line_color = trajectory_color

        do i = 1, n_trajectories
            call convert_and_add_trajectory(fig, trajectories, i, lengths(i), &
                                            line_color, x_grid, y_grid, &
                                            line_width)
        end do
    end subroutine add_trajectories_to_figure

    subroutine convert_and_add_trajectory(fig, trajectories, traj_idx, &
                                          n_points, &
                                          line_color, x_grid, y_grid, &
                                          line_width)
        ! Convert trajectory from grid to data coordinates
        class(figure_t), intent(inout) :: fig
        real(wp), intent(in) :: trajectories(:, :, :)
        integer, intent(in) :: traj_idx, n_points
        real(wp), intent(in) :: line_color(3), x_grid(:), y_grid(:)
        real(wp), intent(in) :: line_width

        integer :: j
        real(wp), allocatable :: traj_x(:), traj_y(:)

        if (n_points <= 1) return

        allocate (traj_x(n_points), traj_y(n_points))

        ! Convert from grid indices to data coordinates
        do j = 1, n_points
            ! trajectory coordinates are grid indices, convert to data coordinates
            traj_x(j) = map_grid_index_to_coord( &
                        trajectories(traj_idx, j, 1), x_grid)
            traj_y(j) = map_grid_index_to_coord( &
                        trajectories(traj_idx, j, 2), y_grid)
        end do

        ! Add trajectory as line plot to figure
        call add_streamline_to_figure(fig, traj_x, traj_y, line_color, &
                                      line_width)
    end subroutine convert_and_add_trajectory

    subroutine add_streamline_to_figure(fig, traj_x, traj_y, line_color, &
                                        line_width)
                !! Add streamline trajectory to figure as line plot
        use fortplot_plot_data, only: PLOT_TYPE_LINE

        class(figure_t), intent(inout) :: fig
        real(wp), intent(in) :: traj_x(:), traj_y(:)
        real(wp), intent(in) :: line_color(3)
        real(wp), intent(in) :: line_width

        integer :: plot_idx

        ! Get next plot index
        fig%plot_count = fig%plot_count + 1
        plot_idx = fig%plot_count

        ! Ensure plots array is allocated with enough space
        if (.not. allocated(fig%plots)) then
            allocate (fig%plots(fig%state%max_plots))
        else if (plot_idx > size(fig%plots)) then
            ! Reallocate if needed - this should not happen with proper max_plots
            return
        end if

        ! Set plot type and data
        fig%plots(plot_idx)%plot_type = PLOT_TYPE_LINE

        ! Store trajectory data
        allocate (fig%plots(plot_idx)%x(size(traj_x)))
        allocate (fig%plots(plot_idx)%y(size(traj_y)))
        fig%plots(plot_idx)%x = traj_x
        fig%plots(plot_idx)%y = traj_y

        ! Set streamline properties
        fig%plots(plot_idx)%linestyle = '-'
        fig%plots(plot_idx)%marker = ''
        fig%plots(plot_idx)%color = line_color
        fig%plots(plot_idx)%line_width = line_width
    end subroutine add_streamline_to_figure

end module fortplot_streamplot_core
