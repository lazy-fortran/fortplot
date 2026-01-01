module fortplot_streamplot_matplotlib
    !! Complete matplotlib-compatible streamplot implementation
    !! Following matplotlib's streamplot.py EXACTLY
    use fortplot_streamline_placement
    use fortplot_streamline_integrator, only: integration_params_t
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    private

    public :: streamplot_matplotlib

contains

    subroutine streamplot_matplotlib(x, y, u, v, density, trajectories, &
                                     n_trajectories, &
                                     trajectory_lengths, rtol, atol, max_time)
        !! Matplotlib-compatible streamplot implementation
        !! Following the EXACT algorithm from matplotlib/streamplot.py
        real(wp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(wp), intent(in), optional :: density
        real, allocatable, intent(out) :: trajectories(:, :, :)
        ! (trajectory, point, x/y)
        integer, intent(out) :: n_trajectories
        integer, allocatable, intent(out) :: trajectory_lengths(:)
        ! Actual length of each trajectory
        real(wp), intent(in), optional :: rtol, atol, max_time

        ! Local variables following matplotlib structure
        type(stream_mask_t) :: mask
        type(coordinate_mapper_t) :: dmap
        real(wp) :: plot_density
        integer, allocatable :: spiral_seeds(:, :)
        integer :: n_spiral_seeds, xm, ym, i
        real(wp) :: xg, yg
        real, allocatable :: trajectory_x(:), trajectory_y(:)
        integer :: n_points
        logical :: success
        real(wp) :: local_rtol, local_atol, local_max_time
        real(wp) :: maxlength, maxerror, tol

        ! Pre-scaled velocity fields like matplotlib (lines 447-453)
        real(wp), allocatable :: u_grid(:, :), v_grid(:, :), speed_field(:, :)

        ! Set density (default 1.0 like matplotlib)
        plot_density = 1.0_wp
        if (present(density)) plot_density = density

        ! Initialize mask with 30x30 base scaled by density (EXACTLY like matplotlib)
        call mask%initialize(plot_density)

        ! Initialize coordinate mapper
        call dmap%initialize([x(1), x(size(x))], [y(1), y(size(y))], &
                             [size(x), size(y)], [mask%nx, mask%ny])

        local_rtol = 1.0e-6_wp
        if (present(rtol)) local_rtol = rtol
        local_atol = 1.0e-9_wp
        if (present(atol)) local_atol = atol
        tol = max(local_rtol, local_atol)

        maxerror = 0.003_wp
        if (tol > 0.0_wp) then
            maxerror = 0.003_wp*sqrt(tol/1.0e-6_wp)
            maxerror = min(0.1_wp, max(1.0e-6_wp, maxerror))
        end if

        maxlength = 4.0_wp
        if (present(max_time)) then
            local_max_time = max_time
            maxlength = local_max_time
        end if

        ! Pre-scale velocity field to grid coordinates like matplotlib (line 448)
        call rescale_velocity_to_grid_coordinates(x, y, u, v, u_grid, v_grid, &
                                                  speed_field)

        ! Generate spiral starting points (EXACTLY like matplotlib)
        call generate_spiral_seeds([mask%nx, mask%ny], spiral_seeds, n_spiral_seeds)

        ! Allocate trajectories array
        allocate (trajectories(n_spiral_seeds, 1000, 2))
        ! Max 1000 points per trajectory
        allocate (trajectory_lengths(n_spiral_seeds))
        trajectories = 0.0  ! Initialize to zero
        trajectory_lengths = 0
        n_trajectories = 0

        ! Main loop: EXACTLY like matplotlib lines 152-157
        do i = 1, n_spiral_seeds
            xm = spiral_seeds(1, i)
            ym = spiral_seeds(2, i)

            ! Check if mask position is free (matplotlib line 153)
            if (mask%is_free(xm, ym)) then
                ! Convert mask to grid coordinates (matplotlib line 154)
                call dmap%mask2grid(xm, ym, xg, yg)

                ! Integrate trajectory (matplotlib line 155) with pre-scaled velocity
                call integrate_matplotlib_style(xg, yg, u_grid, v_grid, speed_field, &
                                                dmap, mask, &
                                                maxlength, maxerror, trajectory_x, &
                                                trajectory_y, &
                                                n_points, success)

                ! Add trajectory if successful (matplotlib line 156-157)
                if (success .and. n_points > 5) then  ! Lower point requirement
                    n_trajectories = n_trajectories + 1

                    ! Store trajectory
                    trajectories(n_trajectories, 1:n_points, 1) = &
                        trajectory_x(1:n_points)
                    trajectories(n_trajectories, 1:n_points, 2) = &
                        trajectory_y(1:n_points)
                    trajectory_lengths(n_trajectories) = n_points

                    ! Store trajectory (figure addition handled by caller)
                end if
            end if
        end do

        if (allocated(spiral_seeds)) deallocate (spiral_seeds)
    end subroutine streamplot_matplotlib
    subroutine integrate_matplotlib_style(xg0, yg0, u_grid, v_grid, speed_field, &
                                          dmap, mask, &
                                          maxlength, maxerror, traj_x, traj_y, &
                                          n_points, &
                                          success)
        !! Integration following matplotlib's exact approach with pre-scaled velocity
        real(wp), intent(in) :: xg0, yg0
        real(wp), intent(in) :: u_grid(:, :), v_grid(:, :), speed_field(:, :)
        type(coordinate_mapper_t), intent(in) :: dmap
        type(stream_mask_t), intent(inout) :: mask
        real(wp), intent(in) :: maxlength, maxerror
        real, allocatable, intent(out) :: traj_x(:), traj_y(:)
        integer, intent(out) :: n_points
        logical, intent(out) :: success

        real, allocatable :: forward_x(:), forward_y(:), backward_x(:), backward_y(:)
        integer :: n_forward, n_backward, i
        real(wp) :: backward_length, forward_length, total_length

        ! For circular flows, streamlines need to be able to return to start
        ! but broken_streamlines=True prevents this
        success = .false.

        ! Allocate trajectory arrays (move from stack to heap for memory safety)
        allocate (forward_x(500), forward_y(500), backward_x(500), backward_y(500))

        ! Start trajectory in mask (like matplotlib line 485)
        call start_trajectory_in_mask(dmap, mask, xg0, yg0, success)
        if (.not. success) return

        ! Integrate backward (matplotlib lines 488-492)
        call integrate_direction(xg0, yg0, u_grid, v_grid, speed_field, dmap, mask, &
                                 -1.0_wp, maxlength/2.0_wp, maxerror, .true., &
                                 backward_x, &
                                 backward_y, n_backward, backward_length)

        ! Reset start point (matplotlib line 495)
        call reset_trajectory_start(dmap, mask, xg0, yg0)

        ! Integrate forward (matplotlib lines 496-499)
        call integrate_direction(xg0, yg0, u_grid, v_grid, speed_field, dmap, mask, &
                                 1.0_wp, maxlength/2.0_wp, maxerror, .true., &
                                 forward_x, forward_y, n_forward, forward_length)

        ! Total length is sum of both directions
        total_length = backward_length + forward_length

        ! Combine trajectories (backward reversed + forward)
        n_points = n_backward + n_forward - 1

        ! Use the actual accumulated path length from integration (like matplotlib)

        ! Reject short trajectories (minlength = 0.1)
        if (total_length < 0.1_wp) then
            success = .false.
            call mask%undo_trajectory()
            n_points = 0
            allocate (traj_x(1), traj_y(1))  ! Dummy allocation
            return
        end if

        allocate (traj_x(n_points), traj_y(n_points))

        ! Add backward trajectory (reversed)
        do i = 1, n_backward - 1
            traj_x(n_backward - i) = backward_x(i + 1)
            traj_y(n_backward - i) = backward_y(i + 1)
        end do

        ! Add forward trajectory
        do i = 1, n_forward
            traj_x(n_backward - 1 + i) = forward_x(i)
            traj_y(n_backward - 1 + i) = forward_y(i)
        end do

        success = .true.

        ! Clean up allocated arrays
        if (allocated(forward_x)) deallocate (forward_x)
        if (allocated(forward_y)) deallocate (forward_y)
        if (allocated(backward_x)) deallocate (backward_x)
        if (allocated(backward_y)) deallocate (backward_y)
    end subroutine integrate_matplotlib_style
    subroutine integrate_direction(xg0, yg0, u_grid, v_grid, speed_field, dmap, mask, &
                                   direction, maxlength, maxerror, broken_streamlines, &
                                   traj_x, traj_y, n_points, path_length)
        !! Integrate in one direction with RK12 adaptive step size exactly like
        !! matplotlib using pre-scaled velocity
        real(wp), intent(in) :: xg0, yg0, direction, maxlength, maxerror
        real(wp), intent(in) :: u_grid(:, :), v_grid(:, :), speed_field(:, :)
        type(coordinate_mapper_t), intent(in) :: dmap
        type(stream_mask_t), intent(inout) :: mask
        logical, intent(in) :: broken_streamlines
        real, intent(out) :: traj_x(500), traj_y(500)
        integer, intent(out) :: n_points
        real(wp), intent(out) :: path_length

        real(wp) :: xg, yg, ds, total_length, maxds
        real(wp) :: ug, vg, speed_ax
        real(wp) :: k1x, k1y, k2x, k2y, dx1, dy1, dx2, dy2, error
        integer :: step_count
        logical :: trajectory_updated

        ! Parameters matching matplotlib exactly
        maxds = min(1.0_wp/real(mask%nx, wp), 1.0_wp/real(mask%ny, wp), 0.1_wp)

        xg = xg0
        yg = yg0
        ds = maxds
        total_length = 0.0_wp
        n_points = 1

        traj_x(1) = real(xg)
        traj_y(1) = real(yg)

        do step_count = 1, 2000  ! Max steps like matplotlib
            ! Get pre-scaled velocity at current position (matplotlib lines 462-463)
            call interpolate_velocity_prescaled(xg, yg, u_grid, v_grid, speed_field, &
                                                ug, vg, speed_ax)
            if (speed_ax < 1e-10) exit  ! Stagnation point

            ! Calculate dt_ds like matplotlib (lines 458-461): dt_ds = 1/ds_dt where
            ! ds_dt is speed. Apply direction for integration direction (matplotlib
            ! forward_time/backward_time).
            k1x = direction*ug/speed_ax
            k1y = direction*vg/speed_ax

            ! RK12 second stage (matplotlib RK12 implementation)
            call interpolate_velocity_prescaled(xg + ds*k1x, yg + ds*k1y, u_grid, &
                                                v_grid, speed_field, ug, vg, speed_ax)
            if (speed_ax < 1e-10) exit

            k2x = direction*ug/speed_ax
            k2y = direction*vg/speed_ax

            ! Calculate two solutions (matplotlib lines 580-583)
            dx1 = ds*k1x  ! Euler step
            dy1 = ds*k1y
            dx2 = ds*0.5_wp*(k1x + k2x)  ! RK2 step
            dy2 = ds*0.5_wp*(k1y + k2y)

            ! Error estimate normalized to axes coordinates (matplotlib lines 586-587)
            error = sqrt(((dx2 - dx1)/(size(u_grid, 1) - 1))**2 + &
                         ((dy2 - dy1)/(size(u_grid, 2) - 1))**2)

            ! Accept step if error is acceptable (like matplotlib line 590)
            if (error < maxerror) then
                xg = xg + dx2
                yg = yg + dy2

                ! Check bounds
                if (xg < 0 .or. xg >= size(u_grid, 1) - 1 .or. yg < 0 .or. yg >= &
                    size(u_grid, 2) - 1) exit

                ! Update trajectory in mask exactly like matplotlib (line 594)
                trajectory_updated = update_trajectory_in_mask_with_broken_streams( &
                                     dmap, mask, xg, yg, broken_streamlines)
                if (.not. trajectory_updated) then
                    exit  ! Collision or mask full - stop integration like matplotlib
                end if

                ! Stop if next step exceeds maxlength
                if (total_length + ds > maxlength) exit

                ! Store point
                n_points = n_points + 1
                if (n_points > 500) exit
                traj_x(n_points) = real(xg)
                traj_y(n_points) = real(yg)

                ! Accumulate path length in axes coordinates like matplotlib (line 599)
                total_length = total_length + ds
            end if

            ! Adjust step size based on error (matplotlib lines 602-605)
            if (abs(error) <= epsilon(1.0_wp)) then
                ds = maxds
            else
                ds = min(maxds, 0.85_wp*ds*sqrt(maxerror/error))
            end if

        end do

        ! Return the accumulated path length
        path_length = total_length

    end subroutine integrate_direction

    subroutine rescale_velocity_to_grid_coordinates(x, y, u, v, u_grid, v_grid, &
                                                    speed_field)
 !! Pre-scale velocity field to grid coordinates exactly like matplotlib (lines 447-453)
        real(wp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(wp), allocatable, intent(out) :: u_grid(:, :), v_grid(:, :), &
                                              speed_field(:, :)

        integer :: nx, ny, i, j
        real(wp) :: u_ax, v_ax

        nx = size(x)
        ny = size(y)

        allocate (u_grid(nx, ny), v_grid(nx, ny), speed_field(nx, ny))

        ! Rescale velocity onto grid-coordinates like matplotlib dmap.data2grid(u, v)
        ! This converts from data coordinates to grid coordinates
        do j = 1, ny
            do i = 1, nx
                u_grid(i, j) = u(i, j)*real(nx - 1, wp)/(x(nx) - x(1))
                v_grid(i, j) = v(i, j)*real(ny - 1, wp)/(y(ny) - y(1))
            end do
        end do

        ! Calculate speed field in axes coordinates (matplotlib lines 451-453)
        do j = 1, ny
            do i = 1, nx
                u_ax = u_grid(i, j)/real(nx - 1, wp)
                v_ax = v_grid(i, j)/real(ny - 1, wp)
                speed_field(i, j) = sqrt(u_ax**2 + v_ax**2)
            end do
        end do

    end subroutine rescale_velocity_to_grid_coordinates

    subroutine interpolate_velocity_prescaled(xg, yg, u_grid, v_grid, speed_field, ug, &
                                              vg, speed_ax)
        !! Bilinear interpolation of pre-scaled velocity like matplotlib
        real(wp), intent(in) :: xg, yg
        real(wp), intent(in) :: u_grid(:, :), v_grid(:, :), speed_field(:, :)
        real(wp), intent(out) :: ug, vg, speed_ax

        integer :: i, j, i_next, j_next
        real(wp) :: xt, yt, a00_u, a01_u, a10_u, a11_u, a0_u, a1_u
        real(wp) :: a00_v, a01_v, a10_v, a11_v, a0_v, a1_v
        real(wp) :: a00_s, a01_s, a10_s, a11_s, a0_s, a1_s

        ! Convert grid coordinates to integer indices
        i = max(1, min(size(u_grid, 1) - 1, int(xg) + 1))
        j = max(1, min(size(u_grid, 2) - 1, int(yg) + 1))

        ! Get next indices with bounds checking
        if (i == size(u_grid, 1)) then
            i_next = i
        else
            i_next = i + 1
        end if

        if (j == size(u_grid, 2)) then
            j_next = j
        else
            j_next = j + 1
        end if

        ! Interpolation weights
        xt = xg - real(i - 1, wp)
        yt = yg - real(j - 1, wp)

        ! Bilinear interpolation for u velocity
        a00_u = u_grid(i, j)
        a01_u = u_grid(i_next, j)
        a10_u = u_grid(i, j_next)
        a11_u = u_grid(i_next, j_next)
        a0_u = a00_u*(1.0_wp - xt) + a01_u*xt
        a1_u = a10_u*(1.0_wp - xt) + a11_u*xt
        ug = a0_u*(1.0_wp - yt) + a1_u*yt

        ! Bilinear interpolation for v velocity
        a00_v = v_grid(i, j)
        a01_v = v_grid(i_next, j)
        a10_v = v_grid(i, j_next)
        a11_v = v_grid(i_next, j_next)
        a0_v = a00_v*(1.0_wp - xt) + a01_v*xt
        a1_v = a10_v*(1.0_wp - xt) + a11_v*xt
        vg = a0_v*(1.0_wp - yt) + a1_v*yt

        ! Bilinear interpolation for speed field
        a00_s = speed_field(i, j)
        a01_s = speed_field(i_next, j)
        a10_s = speed_field(i, j_next)
        a11_s = speed_field(i_next, j_next)
        a0_s = a00_s*(1.0_wp - xt) + a01_s*xt
        a1_s = a10_s*(1.0_wp - xt) + a11_s*xt
        speed_ax = a0_s*(1.0_wp - yt) + a1_s*yt

    end subroutine interpolate_velocity_prescaled

    subroutine start_trajectory_in_mask(dmap, mask, xg, yg, success)
        type(coordinate_mapper_t), intent(in) :: dmap
        type(stream_mask_t), intent(inout) :: mask
        real(wp), intent(in) :: xg, yg
        logical, intent(out) :: success

        integer :: xm, ym
        call dmap%grid2mask(xg, yg, xm, ym)
        if (mask%is_free(xm, ym)) then
            call mask%start_trajectory(xm, ym)
            success = .true.
        else
            success = .false.
        end if
    end subroutine start_trajectory_in_mask

    subroutine reset_trajectory_start(dmap, mask, xg, yg)
        type(coordinate_mapper_t), intent(in) :: dmap
        type(stream_mask_t), intent(inout) :: mask
        real(wp), intent(in) :: xg, yg

        integer :: xm, ym
        call dmap%grid2mask(xg, yg, xm, ym)
        mask%current_x = xm
        mask%current_y = ym
    end subroutine reset_trajectory_start

    logical function update_trajectory_in_mask_with_broken_streams(dmap, mask, xg, yg, &
                                                                   broken_streamlines) &
        result(success)
        !! Update trajectory in mask with broken_streamlines parameter exactly like
        !! matplotlib
        type(coordinate_mapper_t), intent(in) :: dmap
        type(stream_mask_t), intent(inout) :: mask
        real(wp), intent(in) :: xg, yg
        logical, intent(in) :: broken_streamlines

        integer :: xm, ym
        call dmap%grid2mask(xg, yg, xm, ym)

        if (broken_streamlines) then
            ! Standard matplotlib behavior: stop on collision
            success = mask%try_update_trajectory(xm, ym)
        else
            ! Non-broken streamlines: continue through collisions (pass through occupied
            ! areas)
            call mask%update_trajectory(xm, ym)
            success = .true.
        end if
    end function update_trajectory_in_mask_with_broken_streams

    ! add_trajectory_to_figure removed to avoid circular dependency

end module fortplot_streamplot_matplotlib
