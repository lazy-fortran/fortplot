module fortplot_streamplot_core
    !! Streamplot implementation broken down for size compliance
    !! 
    !! Refactored from 253-line function into focused, testable components
    !! following SOLID principles and size constraints.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: EPSILON_COMPARE
    use fortplot_figure_base, only: figure_t
    use fortplot_plot_data, only: arrow_data_t
    use fortplot_streamplot_matplotlib
    use fortplot_logging, only: log_warning

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
        character(len=10) :: arrow_style_val
        real, allocatable :: trajectories(:,:,:)
        integer :: n_trajectories
        integer, allocatable :: trajectory_lengths(:)
        
        ! Validate input dimensions
        if (size(u,1) /= size(x) .or. size(u,2) /= size(y)) then
            self%has_error = .true.
            return
        end if
        
        if (size(v,1) /= size(x) .or. size(v,2) /= size(y)) then
            self%has_error = .true.
            return
        end if
        
        ! Set default parameters
        plot_density = 1.0_wp
        if (present(density)) plot_density = density
        
        ! Validate and set arrow parameters
        call validate_arrow_parameters(self, arrowsize, arrowstyle, arrow_size_val, arrow_style_val)
        if (self%has_error) return
        
        ! Update data ranges for streamplot
        call update_streamplot_ranges(self, x, y)
        
        ! Generate streamlines using matplotlib algorithm
        call generate_streamlines(x, y, u, v, plot_density, trajectories, n_trajectories, trajectory_lengths)
        
        ! Generate arrows if requested
        if (arrow_size_val > 0.0_wp .and. n_trajectories > 0) then
            call generate_streamplot_arrows(self, trajectories, n_trajectories, trajectory_lengths, &
                                          x, y, u, v, arrow_size_val, arrow_style_val)
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
                self%has_error = .true.
                return
            end if
            arrow_size_val = arrowsize
        end if
        
        ! Handle arrow style validation
        arrow_style_val = '->'  ! Default matplotlib-compatible arrow style
        if (present(arrowstyle)) then
            if (trim(arrowstyle) /= '->' .and. trim(arrowstyle) /= '-' .and. &
                trim(arrowstyle) /= '<-' .and. trim(arrowstyle) /= '<->') then
                self%has_error = .true.
                return
            end if
            arrow_style_val = trim(arrowstyle)
        end if
    end subroutine validate_arrow_parameters

    subroutine update_streamplot_ranges(self, x, y)
        !! Update figure data ranges for streamplot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        
        if (.not. self%xlim_set) then
            self%x_min = minval(x)
            self%x_max = maxval(x)
        end if
        if (.not. self%ylim_set) then
            self%y_min = minval(y)
            self%y_max = maxval(y)
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
                                        x_grid, y_grid, u_field, v_field, arrow_size, arrow_style)
        !! Generate arrows along streamlines using matplotlib-compatible placement algorithm
        class(figure_t), intent(inout) :: fig
        real, intent(in) :: trajectories(:,:,:)
        integer, intent(in) :: n_trajectories, trajectory_lengths(:)
        real(wp), intent(in) :: x_grid(:), y_grid(:), u_field(:,:), v_field(:,:)
        real(wp), intent(in) :: arrow_size
        character(len=*), intent(in) :: arrow_style
        
        integer :: max_arrows, arrow_count
        
        ! Calculate maximum possible arrows based on density and trajectory count
        max_arrows = max(1, min(500, n_trajectories * 3))  ! Limit to prevent memory issues
        
        ! Allocate arrow data array
        if (allocated(fig%arrow_data)) deallocate(fig%arrow_data)
        allocate(fig%arrow_data(max_arrows))
        
        arrow_count = 0
        
        ! Place arrows along trajectories
        call place_arrows_on_trajectories(fig, trajectories, n_trajectories, trajectory_lengths, &
                                        x_grid, y_grid, u_field, v_field, arrow_size, arrow_style, &
                                        max_arrows, arrow_count)
        
        ! Resize arrow array to actual count
        call finalize_arrow_array(fig, arrow_count)
    end subroutine generate_streamplot_arrows

    subroutine place_arrows_on_trajectories(fig, trajectories, n_trajectories, trajectory_lengths, &
                                          x_grid, y_grid, u_field, v_field, arrow_size, arrow_style, &
                                          max_arrows, arrow_count)
        !! Place arrows at regular intervals along each trajectory
        class(figure_t), intent(inout) :: fig
        real, intent(in) :: trajectories(:,:,:)
        integer, intent(in) :: n_trajectories, trajectory_lengths(:), max_arrows
        real(wp), intent(in) :: x_grid(:), y_grid(:), u_field(:,:), v_field(:,:), arrow_size
        character(len=*), intent(in) :: arrow_style
        integer, intent(inout) :: arrow_count
        
        integer :: traj_idx, arrow_interval, point_idx
        real(wp) :: arrow_x, arrow_y, arrow_dx, arrow_dy, speed_mag
        
        ! Place arrows along each trajectory at regular intervals
        do traj_idx = 1, n_trajectories
            if (trajectory_lengths(traj_idx) < 5) cycle  ! Skip very short trajectories
            
            ! Calculate arrow interval based on trajectory length (matplotlib-style)
            arrow_interval = max(1, trajectory_lengths(traj_idx) / 3)  ! ~3 arrows per trajectory
            
            ! Place arrows at intervals along the trajectory
            do point_idx = arrow_interval, trajectory_lengths(traj_idx) - 1, arrow_interval
                if (arrow_count >= max_arrows) exit
                
                ! Get arrow position and direction
                call calculate_arrow_properties(trajectories, traj_idx, point_idx, x_grid, y_grid, &
                                              u_field, v_field, arrow_x, arrow_y, arrow_dx, arrow_dy, speed_mag)
                
                ! Skip if velocity is too small
                if (speed_mag < EPSILON_COMPARE) cycle
                
                ! Store arrow data
                call store_arrow_data(fig, arrow_count, arrow_x, arrow_y, arrow_dx, arrow_dy, &
                                    speed_mag, arrow_size, arrow_style)
            end do
        end do
    end subroutine place_arrows_on_trajectories

    subroutine calculate_arrow_properties(trajectories, traj_idx, point_idx, x_grid, y_grid, &
                                        u_field, v_field, arrow_x, arrow_y, arrow_dx, arrow_dy, speed_mag)
        !! Calculate arrow position and direction at trajectory point
        real, intent(in) :: trajectories(:,:,:)
        integer, intent(in) :: traj_idx, point_idx
        real(wp), intent(in) :: x_grid(:), y_grid(:), u_field(:,:), v_field(:,:)
        real(wp), intent(out) :: arrow_x, arrow_y, arrow_dx, arrow_dy, speed_mag
        
        ! Get arrow position from trajectory
        arrow_x = real(trajectories(traj_idx, point_idx, 1), wp)
        arrow_y = real(trajectories(traj_idx, point_idx, 2), wp)
        
        ! Calculate arrow direction from velocity field at this position
        call interpolate_velocity_at_point(arrow_x, arrow_y, x_grid, y_grid, &
                                         u_field, v_field, arrow_dx, arrow_dy, speed_mag)
    end subroutine calculate_arrow_properties

    subroutine store_arrow_data(fig, arrow_count, arrow_x, arrow_y, arrow_dx, arrow_dy, &
                               speed_mag, arrow_size, arrow_style)
        !! Store normalized arrow data in figure
        class(figure_t), intent(inout) :: fig
        integer, intent(inout) :: arrow_count
        real(wp), intent(in) :: arrow_x, arrow_y, arrow_dx, arrow_dy, speed_mag, arrow_size
        character(len=*), intent(in) :: arrow_style
        
        ! Normalize direction vector
        real(wp) :: norm_dx, norm_dy
        
        norm_dx = arrow_dx / speed_mag
        norm_dy = arrow_dy / speed_mag
        
        ! Store arrow data
        arrow_count = arrow_count + 1
        fig%arrow_data(arrow_count)%x = arrow_x
        fig%arrow_data(arrow_count)%y = arrow_y
        fig%arrow_data(arrow_count)%dx = norm_dx
        fig%arrow_data(arrow_count)%dy = norm_dy
        fig%arrow_data(arrow_count)%size = arrow_size
        fig%arrow_data(arrow_count)%style = arrow_style
    end subroutine store_arrow_data

    subroutine finalize_arrow_array(fig, arrow_count)
        !! Resize arrow array to actual count or deallocate if empty
        class(figure_t), intent(inout) :: fig
        integer, intent(in) :: arrow_count
        
        if (arrow_count > 0) then
            fig%arrow_data = fig%arrow_data(1:arrow_count)
        else
            deallocate(fig%arrow_data)
        end if
    end subroutine finalize_arrow_array

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
        
        ! Convert from grid coordinates to data coordinates
        do j = 1, n_points
            ! Convert grid coords to data coords: grid2data transformation
            traj_x(j) = real(trajectories(traj_idx, j, 1), wp) * (x_grid(size(x_grid)) - x_grid(1)) / &
                       real(size(x_grid) - 1, wp) + x_grid(1)
            traj_y(j) = real(trajectories(traj_idx, j, 2), wp) * (y_grid(size(y_grid)) - y_grid(1)) / &
                       real(size(y_grid) - 1, wp) + y_grid(1)
        end do
        
        ! Add trajectory as line plot to figure
        call add_streamline_to_figure(fig, traj_x, traj_y, line_color)
        
        deallocate(traj_x, traj_y)
    end subroutine convert_and_add_trajectory

    subroutine add_streamline_to_figure(fig, traj_x, traj_y, line_color)
        !! Add streamline trajectory to figure as line plot
        use fortplot_plot_data, only: PLOT_TYPE_LINE
        
        class(figure_t), intent(inout) :: fig
        real(wp), intent(in) :: traj_x(:), traj_y(:)
        real(wp), intent(in) :: line_color(3)
        
        integer :: plot_idx, subplot_idx, color_idx
        
        ! Get current subplot
        subplot_idx = fig%current_subplot
        plot_idx = fig%subplots(subplot_idx)%plot_count + 1
        fig%subplots(subplot_idx)%plot_count = plot_idx
        
        ! Set plot type and data
        fig%subplots(subplot_idx)%plots(plot_idx)%plot_type = PLOT_TYPE_LINE
        
        ! Store trajectory data
        allocate(fig%subplots(subplot_idx)%plots(plot_idx)%x(size(traj_x)))
        allocate(fig%subplots(subplot_idx)%plots(plot_idx)%y(size(traj_y)))
        fig%subplots(subplot_idx)%plots(plot_idx)%x = traj_x
        fig%subplots(subplot_idx)%plots(plot_idx)%y = traj_y
        
        ! Set streamline properties
        fig%subplots(subplot_idx)%plots(plot_idx)%linestyle = '-'
        fig%subplots(subplot_idx)%plots(plot_idx)%marker = ''
        fig%subplots(subplot_idx)%plots(plot_idx)%color = line_color
    end subroutine add_streamline_to_figure

    subroutine interpolate_velocity_at_point(x_pos, y_pos, x_grid, y_grid, u_field, v_field, &
                                           u_interp, v_interp, speed_mag)
        !! Bilinear interpolation of velocity field at given position
        real(wp), intent(in) :: x_pos, y_pos
        real(wp), intent(in) :: x_grid(:), y_grid(:), u_field(:,:), v_field(:,:)
        real(wp), intent(out) :: u_interp, v_interp, speed_mag
        
        integer :: i, j, i_next, j_next
        real(wp) :: x_frac, y_frac
        
        ! Find grid indices
        call find_grid_indices(x_pos, y_pos, x_grid, y_grid, i, j, i_next, j_next)
        
        ! Calculate interpolation weights
        call calculate_interpolation_weights(x_pos, y_pos, x_grid, y_grid, i, j, i_next, j_next, x_frac, y_frac)
        
        ! Perform bilinear interpolation
        call perform_bilinear_interpolation(u_field, v_field, i, j, i_next, j_next, &
                                          x_frac, y_frac, u_interp, v_interp)
        
        ! Calculate speed magnitude
        speed_mag = sqrt(u_interp**2 + v_interp**2)
    end subroutine interpolate_velocity_at_point

    subroutine find_grid_indices(x_pos, y_pos, x_grid, y_grid, i, j, i_next, j_next)
        !! Find grid indices for interpolation
        real(wp), intent(in) :: x_pos, y_pos, x_grid(:), y_grid(:)
        integer, intent(out) :: i, j, i_next, j_next
        
        ! Find grid indices
        i = 1
        do while (i < size(x_grid) .and. x_grid(i) < x_pos)
            i = i + 1
        end do
        i = max(1, min(size(x_grid) - 1, i - 1))
        
        j = 1
        do while (j < size(y_grid) .and. y_grid(j) < y_pos)
            j = j + 1
        end do
        j = max(1, min(size(y_grid) - 1, j - 1))
        
        i_next = min(size(x_grid), i + 1)
        j_next = min(size(y_grid), j + 1)
    end subroutine find_grid_indices

    subroutine calculate_interpolation_weights(x_pos, y_pos, x_grid, y_grid, i, j, i_next, j_next, x_frac, y_frac)
        !! Calculate interpolation weights for bilinear interpolation
        real(wp), intent(in) :: x_pos, y_pos, x_grid(:), y_grid(:)
        integer, intent(in) :: i, j, i_next, j_next
        real(wp), intent(out) :: x_frac, y_frac
        
        ! Calculate interpolation weights
        if (i_next > i) then
            x_frac = (x_pos - x_grid(i)) / (x_grid(i_next) - x_grid(i))
        else
            x_frac = 0.0_wp
        end if
        
        if (j_next > j) then
            y_frac = (y_pos - y_grid(j)) / (y_grid(j_next) - y_grid(j))
        else
            y_frac = 0.0_wp
        end if
    end subroutine calculate_interpolation_weights

    subroutine perform_bilinear_interpolation(u_field, v_field, i, j, i_next, j_next, &
                                            x_frac, y_frac, u_interp, v_interp)
        !! Perform bilinear interpolation of velocity components
        real(wp), intent(in) :: u_field(:,:), v_field(:,:)
        integer, intent(in) :: i, j, i_next, j_next
        real(wp), intent(in) :: x_frac, y_frac
        real(wp), intent(out) :: u_interp, v_interp
        
        real(wp) :: w00, w01, w10, w11
        real(wp) :: u00, u01, u10, u11, v00, v01, v10, v11
        
        ! Bilinear interpolation weights
        w00 = (1.0_wp - x_frac) * (1.0_wp - y_frac)
        w01 = x_frac * (1.0_wp - y_frac)
        w10 = (1.0_wp - x_frac) * y_frac
        w11 = x_frac * y_frac
        
        ! Get velocity values at grid corners
        u00 = u_field(i, j)
        u01 = u_field(i_next, j)
        u10 = u_field(i, j_next)
        u11 = u_field(i_next, j_next)
        
        v00 = v_field(i, j)
        v01 = v_field(i_next, j)
        v10 = v_field(i, j_next)
        v11 = v_field(i_next, j_next)
        
        ! Interpolate velocity components
        u_interp = w00 * u00 + w01 * u01 + w10 * u10 + w11 * u11
        v_interp = w00 * v00 + w01 * v01 + w10 * v10 + w11 * v11
    end subroutine perform_bilinear_interpolation

end module fortplot_streamplot_core