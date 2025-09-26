module fortplot_streamplot_arrow_utils
    !! Shared utilities for streamplot arrow placement

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: EPSILON_COMPARE
    use fortplot_plot_data, only: arrow_data_t
    use fortplot_figure_initialization, only: figure_state_t

    implicit none
    private

    public :: validate_streamplot_arrow_parameters
    public :: compute_streamplot_arrows
    public :: replace_stream_arrows
    public :: map_grid_index_to_coord

contains

    subroutine validate_streamplot_arrow_parameters(arrowsize_opt, arrowstyle_opt, &
        & arrow_size_val, arrow_style_val, has_error)
        !! Validate arrow size and style inputs (matplotlib compatible)
        real(wp), intent(in), optional :: arrowsize_opt
        character(len=*), intent(in), optional :: arrowstyle_opt
        real(wp), intent(out) :: arrow_size_val
        character(len=10), intent(out) :: arrow_style_val
        logical, intent(out) :: has_error

        has_error = .false.
        arrow_size_val = 1.0_wp
        if (present(arrowsize_opt)) then
            if (arrowsize_opt < 0.0_wp) then
                has_error = .true.
                return
            end if
            arrow_size_val = arrowsize_opt
        end if

        arrow_style_val = '->'
        if (present(arrowstyle_opt)) then
            select case (trim(arrowstyle_opt))
            case ('->', '-', '<-', '<->')
                arrow_style_val = trim(arrowstyle_opt)
            case default
                has_error = .true.
                return
            end select
        end if
    end subroutine validate_streamplot_arrow_parameters

    subroutine compute_streamplot_arrows(trajectories, n_trajectories, &
        & trajectory_lengths, x_grid, y_grid, arrow_size, &
        & arrow_style, arrows)
        !! Compute arrow metadata for streamlines based on trajectory geometry
        real, intent(in) :: trajectories(:,:,:)
        integer, intent(in) :: n_trajectories
        integer, intent(in) :: trajectory_lengths(:)
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        real(wp), intent(in) :: arrow_size
        character(len=*), intent(in) :: arrow_style
        type(arrow_data_t), allocatable, intent(out) :: arrows(:)

        integer :: traj_idx, n_points, target_index
        integer :: arrow_count
        real(wp), allocatable :: traj_x(:), traj_y(:)
        real(wp), allocatable :: arc_lengths(:)
        real(wp) :: total_length, half_length
        real(wp) :: segment_start_length, segment_length
        real(wp) :: position_t, arrow_x, arrow_y
        real(wp) :: segment_dx, segment_dy, direction_norm
        type(arrow_data_t), allocatable :: buffer(:)

        if (allocated(arrows)) deallocate(arrows)

        if (n_trajectories <= 0) return
        if (arrow_size <= 0.0_wp) return

        allocate(buffer(n_trajectories))
        arrow_count = 0

        do traj_idx = 1, n_trajectories
            n_points = trajectory_lengths(traj_idx)
            if (n_points < 2) cycle

            call extract_trajectory_data(trajectories, traj_idx, n_points, &
                x_grid, y_grid, traj_x, traj_y)
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
            buffer(arrow_count)%x = arrow_x
            buffer(arrow_count)%y = arrow_y
            buffer(arrow_count)%dx = segment_dx / direction_norm
            buffer(arrow_count)%dy = segment_dy / direction_norm
            buffer(arrow_count)%size = arrow_size
            buffer(arrow_count)%style = arrow_style
        end do

        if (arrow_count > 0) then
            allocate(arrows(arrow_count))
            arrows = buffer(1:arrow_count)
        end if

        if (allocated(buffer)) deallocate(buffer)
        if (allocated(traj_x)) deallocate(traj_x)
        if (allocated(traj_y)) deallocate(traj_y)
        if (allocated(arc_lengths)) deallocate(arc_lengths)
    end subroutine compute_streamplot_arrows

    subroutine replace_stream_arrows(state, arrows)
        !! Replace figure state stream arrows and manage rendered flag
        type(figure_state_t), intent(inout) :: state
        type(arrow_data_t), allocatable, intent(inout) :: arrows(:)

        logical :: had_arrows

        had_arrows = .false.
        if (allocated(state%stream_arrows)) then
            had_arrows = size(state%stream_arrows) > 0
            deallocate(state%stream_arrows)
        end if

        if (allocated(arrows)) then
            call move_alloc(arrows, state%stream_arrows)
            state%rendered = .false.
        else
            if (had_arrows) state%rendered = .false.
        end if
    end subroutine replace_stream_arrows

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

    subroutine extract_trajectory_data(trajectories, traj_idx, n_points, &
        x_grid, y_grid, traj_x, traj_y)
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
            traj_x(j) = map_grid_index_to_coord( &
                real(trajectories(traj_idx, j, 1), wp), x_grid)
            traj_y(j) = map_grid_index_to_coord( &
                real(trajectories(traj_idx, j, 2), wp), y_grid)
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

end module fortplot_streamplot_arrow_utils
