module fortplot_streamplot_core
    !! Streamplot implementation broken down for size compliance
    !! 
    !! Refactored from 253-line function into focused, testable components
    !! following SOLID principles and size constraints.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: EPSILON_COMPARE
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: arrow_data_t
    use fortplot_streamplot_matplotlib

    implicit none

    private
    public :: setup_streamplot_parameters, generate_streamlines, add_streamline_to_figure

contains

    subroutine setup_streamplot_parameters(self, x, y, u, v, density, color, linewidth, &
                                          rtol, atol, max_time, arrowsize, arrowstyle)
        !! Setup and validate streamplot parameters (focused on validation logic)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density, linewidth, rtol, atol, max_time, arrowsize
        real(wp), intent(in), optional :: color(3)
        character(len=*), intent(in), optional :: arrowstyle
        
        real(wp) :: plot_density, arrow_size_val
        real(wp) :: lw_dummy, rt_dummy, at_dummy, mt_dummy
        character(len=10) :: arrow_style_val
        real, allocatable :: trajectories(:,:,:)
        integer :: n_trajectories
        integer, allocatable :: trajectory_lengths(:)
        if (present(linewidth)) lw_dummy = linewidth
        if (present(rtol)) rt_dummy = rtol
        if (present(atol)) at_dummy = atol
        if (present(max_time)) mt_dummy = max_time
        
        ! Validate input dimensions
        if (size(u,1) /= size(x) .or. size(u,2) /= size(y)) then
            self%state%has_error = .true.
            return
        end if
        
        if (size(v,1) /= size(x) .or. size(v,2) /= size(y)) then
            self%state%has_error = .true.
            return
        end if
        
        ! Set default parameters
        plot_density = 1.0_wp
        if (present(density)) plot_density = density
        
        ! Validate and set arrow parameters
        call validate_arrow_parameters(self, arrowsize, arrowstyle, arrow_size_val, arrow_style_val)
        if (self%state%has_error) return
        
        ! Update data ranges for streamplot
        call update_streamplot_ranges(self, x, y)
        
        ! Generate streamlines using matplotlib algorithm
        call generate_streamlines(x, y, u, v, plot_density, trajectories, n_trajectories, trajectory_lengths)

        ! Always clear queued arrows before recalculating
        call self%clear_backend_arrows()

        ! Generate arrows if requested
        if (arrow_size_val > 0.0_wp .and. n_trajectories > 0) then
            call generate_streamplot_arrows(self, trajectories, n_trajectories, trajectory_lengths, &
                                          x, y, arrow_size_val, arrow_style_val)
        end if
        
        ! Add trajectories to figure
        call add_trajectories_to_figure(self, trajectories, n_trajectories, trajectory_lengths, color, x, y)
    end subroutine setup_streamplot_parameters

    subroutine validate_arrow_parameters(self, arrowsize, arrowstyle, arrow_size_val, arrow_style_val)
        !! Validate arrow parameters with proper error handling
        class(figure_t), intent(inout) :: self
        real(wp), intent(in), optional :: arrowsize
        character(len=*), intent(in), optional :: arrowstyle
        real(wp), intent(out) :: arrow_size_val
        character(len=10), intent(out) :: arrow_style_val
        
        ! Handle arrow size validation
        arrow_size_val = 1.0_wp  ! Default matplotlib-compatible arrow size
        if (present(arrowsize)) then
            if (arrowsize < 0.0_wp) then
                self%state%has_error = .true.
                return
            end if
            arrow_size_val = arrowsize
        end if
        
        ! Handle arrow style validation
        arrow_style_val = '->'  ! Default matplotlib-compatible arrow style
        if (present(arrowstyle)) then
            if (trim(arrowstyle) /= '->' .and. trim(arrowstyle) /= '-' .and. &
                trim(arrowstyle) /= '<-' .and. trim(arrowstyle) /= '<->') then
                self%state%has_error = .true.
                return
            end if
            arrow_style_val = trim(arrowstyle)
        end if
    end subroutine validate_arrow_parameters

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

    subroutine generate_streamlines(x, y, u, v, density, trajectories, n_trajectories, trajectory_lengths)
        !! Generate streamlines using matplotlib-compatible algorithm
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:), density
        real, allocatable, intent(out) :: trajectories(:,:,:)
        integer, intent(out) :: n_trajectories
        integer, allocatable, intent(out) :: trajectory_lengths(:)
        
        ! Delegate to matplotlib implementation
        call streamplot_matplotlib(x, y, u, v, density, trajectories, n_trajectories, trajectory_lengths)
    end subroutine generate_streamlines

    subroutine generate_streamplot_arrows(fig, trajectories, n_trajectories, trajectory_lengths, &
                                        x_grid, y_grid, arrow_size, arrow_style)
        !! Generate one arrow per streamline, matching matplotlib placement (midpoint arrow)
        class(figure_t), intent(inout) :: fig
        real, intent(in) :: trajectories(:,:,:)
        integer, intent(in) :: n_trajectories, trajectory_lengths(:)
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        real(wp), intent(in) :: arrow_size
        character(len=*), intent(in) :: arrow_style
        
        integer :: traj_idx, n_points, target_index, arrow_count
        real(wp), allocatable :: traj_x(:), traj_y(:)
        real(wp), allocatable :: arc_lengths(:)
        real(wp) :: total_length, half_length, segment_start_length, segment_length
        real(wp) :: position_t, arrow_x, arrow_y, segment_dx, segment_dy, direction_norm
        type(arrow_data_t), allocatable :: arrow_buffer(:), trimmed(:)
        logical :: had_existing

        arrow_count = 0

        had_existing = .false.
        if (allocated(fig%state%stream_arrows)) then
            had_existing = size(fig%state%stream_arrows) > 0
            deallocate(fig%state%stream_arrows)
        end if

        if (n_trajectories <= 0) return

        allocate(arrow_buffer(n_trajectories))

        do traj_idx = 1, n_trajectories
            n_points = trajectory_lengths(traj_idx)
            if (n_points < 2) cycle

            call extract_trajectory_data(trajectories, traj_idx, n_points, x_grid, y_grid, &
                                         traj_x, traj_y)

            call compute_segment_lengths(traj_x, traj_y, arc_lengths, total_length)
            if (total_length <= EPSILON_COMPARE) cycle

            half_length = 0.5_wp * total_length
            target_index = locate_half_length(arc_lengths, half_length)
            if (target_index < 1 .or. target_index >= n_points) cycle

            if (target_index == 1) then
                segment_start_length = 0.0_wp
            else
                segment_start_length = arc_lengths(target_index - 1)
            end if

            segment_length = arc_lengths(target_index) - segment_start_length
            if (segment_length <= EPSILON_COMPARE) cycle

            position_t = (half_length - segment_start_length) / segment_length
            position_t = max(0.0_wp, min(1.0_wp, position_t))

            segment_dx = traj_x(target_index + 1) - traj_x(target_index)
            segment_dy = traj_y(target_index + 1) - traj_y(target_index)

            direction_norm = sqrt(segment_dx*segment_dx + segment_dy*segment_dy)
            if (direction_norm <= EPSILON_COMPARE) cycle

            arrow_x = traj_x(target_index) + position_t * segment_dx
            arrow_y = traj_y(target_index) + position_t * segment_dy

            arrow_count = arrow_count + 1
            arrow_buffer(arrow_count)%x = arrow_x
            arrow_buffer(arrow_count)%y = arrow_y
            arrow_buffer(arrow_count)%dx = segment_dx / direction_norm
            arrow_buffer(arrow_count)%dy = segment_dy / direction_norm
            arrow_buffer(arrow_count)%size = arrow_size
            arrow_buffer(arrow_count)%style = arrow_style
        end do

        if (arrow_count > 0) then
            allocate(trimmed(arrow_count))
            trimmed = arrow_buffer(1:arrow_count)
            call move_alloc(trimmed, fig%state%stream_arrows)
            fig%state%rendered = .false.
        else
            if (had_existing) fig%state%rendered = .false.
        end if

        if (allocated(arrow_buffer)) deallocate(arrow_buffer)
        if (allocated(traj_x)) deallocate(traj_x)
        if (allocated(traj_y)) deallocate(traj_y)
        if (allocated(arc_lengths)) deallocate(arc_lengths)
    end subroutine generate_streamplot_arrows

    subroutine extract_trajectory_data(trajectories, traj_idx, n_points, x_grid, y_grid, &
                                      traj_x, traj_y)
        !! Convert stored grid indices into data coordinates for a trajectory
        real, intent(in) :: trajectories(:,:,:)
        integer, intent(in) :: traj_idx, n_points
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        real(wp), allocatable, intent(inout) :: traj_x(:), traj_y(:)

        integer :: j

        if (allocated(traj_x)) deallocate(traj_x)
        if (allocated(traj_y)) deallocate(traj_y)
        allocate(traj_x(n_points), traj_y(n_points))

        do j = 1, n_points
            traj_x(j) = map_grid_index_to_coord(real(trajectories(traj_idx, j, 1), wp), x_grid)
            traj_y(j) = map_grid_index_to_coord(real(trajectories(traj_idx, j, 2), wp), y_grid)
        end do
    end subroutine extract_trajectory_data

    subroutine compute_segment_lengths(traj_x, traj_y, arc_lengths, total_length)
        !! Compute cumulative arc lengths along a trajectory
        real(wp), intent(in) :: traj_x(:), traj_y(:)
        real(wp), allocatable, intent(inout) :: arc_lengths(:)
        real(wp), intent(out) :: total_length

        integer :: n_segments, i
        real(wp) :: segment_length

        n_segments = size(traj_x) - 1
        if (allocated(arc_lengths)) deallocate(arc_lengths)

        if (n_segments <= 0) then
            total_length = 0.0_wp
            return
        end if

        allocate(arc_lengths(n_segments))
        total_length = 0.0_wp

        do i = 1, n_segments
            segment_length = sqrt((traj_x(i + 1) - traj_x(i))**2 + &
                                  (traj_y(i + 1) - traj_y(i))**2)
            total_length = total_length + segment_length
            arc_lengths(i) = total_length
        end do
    end subroutine compute_segment_lengths

    integer function locate_half_length(arc_lengths, half_length) result(target_index)
        !! Locate the index of the point just before the halfway distance
        real(wp), intent(in) :: arc_lengths(:), half_length

        integer :: i

        target_index = size(arc_lengths)

        do i = 1, size(arc_lengths)
            if (arc_lengths(i) >= half_length) then
                target_index = i
                exit
            end if
        end do
    end function locate_half_length

    subroutine add_trajectories_to_figure(fig, trajectories, n_trajectories, lengths, trajectory_color, x_grid, y_grid)
        !! Add streamline trajectories to figure as regular plots
        class(figure_t), intent(inout) :: fig
        real, intent(in) :: trajectories(:,:,:)
        integer, intent(in) :: n_trajectories, lengths(:)
        real(wp), intent(in), optional :: trajectory_color(3)
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        
        integer :: i
        real(wp) :: line_color(3)
        
        ! Set default color (blue)
        line_color = [0.0_wp, 0.447_wp, 0.698_wp]
        if (present(trajectory_color)) line_color = trajectory_color
        
        do i = 1, n_trajectories
            call convert_and_add_trajectory(fig, trajectories, i, lengths(i), line_color, x_grid, y_grid)
        end do
    end subroutine add_trajectories_to_figure

    subroutine convert_and_add_trajectory(fig, trajectories, traj_idx, n_points, line_color, x_grid, y_grid)
        !! Convert single trajectory from grid to data coordinates and add to figure
        class(figure_t), intent(inout) :: fig
        real, intent(in) :: trajectories(:,:,:)
        integer, intent(in) :: traj_idx, n_points
        real(wp), intent(in) :: line_color(3), x_grid(:), y_grid(:)
        
        integer :: j
        real(wp), allocatable :: traj_x(:), traj_y(:)
        
        if (n_points <= 1) return
        
        allocate(traj_x(n_points), traj_y(n_points))
        
        ! Convert from grid indices to data coordinates
        do j = 1, n_points
            ! trajectory coordinates are grid indices, convert to data coordinates
            traj_x(j) = map_grid_index_to_coord(real(trajectories(traj_idx, j, 1), wp), &
                                               x_grid)
            traj_y(j) = map_grid_index_to_coord(real(trajectories(traj_idx, j, 2), wp), &
                                               y_grid)
        end do
        
        ! Add trajectory as line plot to figure
        call add_streamline_to_figure(fig, traj_x, traj_y, line_color)

        if (allocated(traj_x)) deallocate(traj_x)
        if (allocated(traj_y)) deallocate(traj_y)
    end subroutine convert_and_add_trajectory

    pure function map_grid_index_to_coord(grid_index, grid_values) result(coord)
        !! Convert matplotlib-style grid index to data coordinate
        real(wp), intent(in) :: grid_index
        real(wp), intent(in) :: grid_values(:)
        real(wp) :: coord
        real(wp) :: span, denom

        if (size(grid_values) <= 1) then
            coord = grid_values(1)
            return
        end if

        span = grid_values(size(grid_values)) - grid_values(1)
        denom = real(size(grid_values) - 1, wp)
        coord = grid_values(1) + grid_index * span / denom
    end function map_grid_index_to_coord

    subroutine add_streamline_to_figure(fig, traj_x, traj_y, line_color)
        !! Add streamline trajectory to figure as line plot
        use fortplot_plot_data, only: PLOT_TYPE_LINE
        
        class(figure_t), intent(inout) :: fig
        real(wp), intent(in) :: traj_x(:), traj_y(:)
        real(wp), intent(in) :: line_color(3)
        
        integer :: plot_idx
        
        ! Get next plot index
        fig%plot_count = fig%plot_count + 1
        plot_idx = fig%plot_count
        
        ! Ensure plots array is allocated with enough space
        if (.not. allocated(fig%plots)) then
            allocate(fig%plots(fig%state%max_plots))
        else if (plot_idx > size(fig%plots)) then
            ! Reallocate if needed - this shouldn't happen with proper max_plots
            return
        end if
        
        ! Set plot type and data
        fig%plots(plot_idx)%plot_type = PLOT_TYPE_LINE
        
        ! Store trajectory data
        allocate(fig%plots(plot_idx)%x(size(traj_x)))
        allocate(fig%plots(plot_idx)%y(size(traj_y)))
        fig%plots(plot_idx)%x = traj_x
        fig%plots(plot_idx)%y = traj_y
        
        ! Set streamline properties
        fig%plots(plot_idx)%linestyle = '-'
        fig%plots(plot_idx)%marker = ''
        fig%plots(plot_idx)%color = line_color
    end subroutine add_streamline_to_figure


end module fortplot_streamplot_core
