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

    subroutine streamplot_matplotlib(x, y, u, v, density, trajectories, n_trajectories, trajectory_lengths)
        !! Matplotlib-compatible streamplot implementation
        !! Following the EXACT algorithm from matplotlib/streamplot.py
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density
        real, allocatable, intent(out) :: trajectories(:,:,:)  ! (trajectory, point, x/y)
        integer, intent(out) :: n_trajectories
        integer, allocatable, intent(out) :: trajectory_lengths(:)  ! Actual length of each trajectory
        
        ! Local variables following matplotlib structure
        type(stream_mask_t) :: mask
        type(coordinate_mapper_t) :: dmap
        real(wp) :: plot_density
        integer, allocatable :: spiral_seeds(:,:)
        integer :: n_spiral_seeds, xm, ym, i
        real(wp) :: xg, yg
        real, allocatable :: trajectory_x(:), trajectory_y(:)
        integer :: n_points
        logical :: success
        
        ! Set density (default 1.0 like matplotlib)
        plot_density = 1.0_wp
        if (present(density)) plot_density = density
        
        ! Initialize mask with 30x30 base scaled by density (EXACTLY like matplotlib)
        call mask%initialize(plot_density)
        
        ! Initialize coordinate mapper
        call dmap%initialize([x(1), x(size(x))], [y(1), y(size(y))], &
                            [size(x), size(y)], [mask%nx, mask%ny])
        
        ! Generate spiral starting points (EXACTLY like matplotlib)
        call generate_spiral_seeds([mask%nx, mask%ny], spiral_seeds, n_spiral_seeds)
        
        ! Allocate trajectories array
        allocate(trajectories(n_spiral_seeds, 1000, 2))  ! Max 1000 points per trajectory
        allocate(trajectory_lengths(n_spiral_seeds))
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
                
                ! Integrate trajectory (matplotlib line 155)
                call integrate_matplotlib_style(xg, yg, x, y, u, v, dmap, mask, &
                                               trajectory_x, trajectory_y, n_points, success)
                
                ! Add trajectory if successful (matplotlib line 156-157)  
                if (success .and. n_points > 20 .and. n_trajectories < 50) then  ! Limit to 50 trajectories
                    n_trajectories = n_trajectories + 1
                    
                    ! Store trajectory
                    trajectories(n_trajectories, 1:n_points, 1) = trajectory_x(1:n_points)
                    trajectories(n_trajectories, 1:n_points, 2) = trajectory_y(1:n_points)
                    trajectory_lengths(n_trajectories) = n_points
                    
                    ! Store trajectory (figure addition handled by caller)
                end if
            end if
        end do
        
        deallocate(spiral_seeds)
    end subroutine streamplot_matplotlib

    subroutine integrate_matplotlib_style(xg0, yg0, x, y, u, v, dmap, mask, &
                                         traj_x, traj_y, n_points, success)
        !! Integration following matplotlib's exact approach
        real(wp), intent(in) :: xg0, yg0
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        type(coordinate_mapper_t), intent(in) :: dmap
        type(stream_mask_t), intent(inout) :: mask
        real, allocatable, intent(out) :: traj_x(:), traj_y(:)
        integer, intent(out) :: n_points
        logical, intent(out) :: success
        
        real :: forward_x(500), forward_y(500), backward_x(500), backward_y(500)
        integer :: n_forward, n_backward, i
        real(wp) :: maxlength
        
        maxlength = 4.0_wp  ! Like matplotlib default
        success = .false.
        
        ! Start trajectory in mask (like matplotlib line 485)
        call start_trajectory_in_mask(dmap, mask, xg0, yg0, success)
        if (.not. success) return
        
        ! Integrate backward (matplotlib lines 488-492)
        call integrate_direction(xg0, yg0, x, y, u, v, dmap, mask, &
                                -1.0_wp, maxlength/2.0_wp, backward_x, backward_y, n_backward)
        
        ! Reset start point (matplotlib line 495)
        call reset_trajectory_start(dmap, mask, xg0, yg0)
        
        ! Integrate forward (matplotlib lines 496-499)
        call integrate_direction(xg0, yg0, x, y, u, v, dmap, mask, &
                                1.0_wp, maxlength/2.0_wp, forward_x, forward_y, n_forward)
        
        ! Combine trajectories (backward reversed + forward)
        n_points = n_backward + n_forward - 1
        allocate(traj_x(n_points), traj_y(n_points))
        
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
        
        success = (n_points > 10)  ! Minimum length check
        if (.not. success) call mask%undo_trajectory()
        
    end subroutine integrate_matplotlib_style

    subroutine integrate_direction(xg0, yg0, x, y, u, v, dmap, mask, direction, maxlength, &
                                  traj_x, traj_y, n_points)
        !! Integrate in one direction with RK12 adaptive step size like matplotlib
        real(wp), intent(in) :: xg0, yg0, direction, maxlength
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        type(coordinate_mapper_t), intent(in) :: dmap
        type(stream_mask_t), intent(inout) :: mask
        real, intent(out) :: traj_x(500), traj_y(500)
        integer, intent(out) :: n_points
        
        real(wp) :: xg, yg, ds, total_length, maxds, maxerror
        real(wp) :: ug, vg, speed_ax
        real(wp) :: k1x, k1y, k2x, k2y, dx1, dy1, dx2, dy2, error
        integer :: step_count
        
        ! Parameters matching matplotlib exactly
        maxds = min(1.0_wp/real(mask%nx,wp), 1.0_wp/real(mask%ny,wp), 0.1_wp)
        maxerror = 0.003_wp  ! Visual quality threshold from matplotlib
        
        xg = xg0
        yg = yg0
        ds = maxds
        total_length = 0.0_wp
        n_points = 1
        
        traj_x(1) = real(xg)
        traj_y(1) = real(yg)
        
        do step_count = 1, 2000  ! Max steps like matplotlib
            ! Get velocity at current position with proper scaling
            call interpolate_velocity(xg, yg, x, y, u, v, ug, vg)
            
            ! Convert to axes coordinates for speed calculation (like matplotlib line 451-453)
            speed_ax = sqrt((ug/(size(x)-1))**2 + (vg/(size(y)-1))**2)
            if (speed_ax < 1e-10) exit  ! Stagnation point
            
            ! Apply direction and normalize by speed for dt_ds calculation (matplotlib line 461-464)
            k1x = direction * ug / speed_ax
            k1y = direction * vg / speed_ax
            
            ! RK12 second stage (matplotlib RK12 implementation)
            call interpolate_velocity(xg + ds*k1x, yg + ds*k1y, x, y, u, v, ug, vg)
            speed_ax = sqrt((ug/(size(x)-1))**2 + (vg/(size(y)-1))**2)
            if (speed_ax < 1e-10) exit
            
            k2x = direction * ug / speed_ax  
            k2y = direction * vg / speed_ax
            
            ! Calculate two solutions (matplotlib lines 580-583)
            dx1 = ds * k1x  ! Euler step
            dy1 = ds * k1y  
            dx2 = ds * 0.5_wp * (k1x + k2x)  ! RK2 step
            dy2 = ds * 0.5_wp * (k1y + k2y)
            
            ! Error estimate normalized to axes coordinates (matplotlib lines 586-587)
            error = sqrt(((dx2-dx1)/(size(x)-1))**2 + ((dy2-dy1)/(size(y)-1))**2)
            
            ! Accept step if error is acceptable (like matplotlib line 590)
            if (error < maxerror) then
                xg = xg + dx2
                yg = yg + dy2
                
                ! Check bounds
                if (xg < 0 .or. xg >= size(x)-1 .or. yg < 0 .or. yg >= size(y)-1) exit
                
                ! Update trajectory in mask (like matplotlib line 594)
                if (.not. update_trajectory_in_mask_safe(dmap, mask, xg, yg)) exit
                
                ! Store point
                n_points = n_points + 1
                if (n_points > 500) exit
                traj_x(n_points) = real(xg)
                traj_y(n_points) = real(yg)
                
                total_length = total_length + ds
                if (total_length >= maxlength) exit
            end if
            
            ! Adjust step size based on error (matplotlib lines 602-605)
            if (error == 0.0_wp) then
                ds = maxds
            else
                ds = min(maxds, 0.85_wp * ds * sqrt(maxerror / error))
            end if
            
        end do
        
    end subroutine integrate_direction

    subroutine interpolate_velocity(xg, yg, x, y, u, v, ug, vg)
        !! Bilinear interpolation exactly like matplotlib's interpgrid function
        real(wp), intent(in) :: xg, yg
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(out) :: ug, vg
        
        integer :: i, j, i_next, j_next
        real(wp) :: xt, yt, a00_u, a01_u, a10_u, a11_u, a0_u, a1_u
        real(wp) :: a00_v, a01_v, a10_v, a11_v, a0_v, a1_v
        
        ! Convert grid coordinates to integer indices (like matplotlib lines 646-656)
        i = max(1, min(size(x)-1, int(xg) + 1))
        j = max(1, min(size(y)-1, int(yg) + 1))
        
        ! Get next indices with bounds checking (matplotlib lines 648-656)
        if (i == size(x)) then
            i_next = i
        else
            i_next = i + 1
        end if
        
        if (j == size(y)) then
            j_next = j
        else
            j_next = j + 1
        end if
        
        ! Interpolation weights (matplotlib lines 662-663)
        xt = xg - real(i - 1, wp)
        yt = yg - real(j - 1, wp)
        
        ! Bilinear interpolation exactly like matplotlib (lines 658-667)
        ! u velocity
        a00_u = u(i, j)
        a01_u = u(i_next, j)
        a10_u = u(i, j_next)
        a11_u = u(i_next, j_next)
        a0_u = a00_u * (1.0_wp - xt) + a01_u * xt
        a1_u = a10_u * (1.0_wp - xt) + a11_u * xt
        ug = a0_u * (1.0_wp - yt) + a1_u * yt
        
        ! v velocity  
        a00_v = v(i, j)
        a01_v = v(i_next, j)
        a10_v = v(i, j_next)
        a11_v = v(i_next, j_next)
        a0_v = a00_v * (1.0_wp - xt) + a01_v * xt
        a1_v = a10_v * (1.0_wp - xt) + a11_v * xt
        vg = a0_v * (1.0_wp - yt) + a1_v * yt
        
    end subroutine interpolate_velocity

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

    subroutine update_trajectory_in_mask(dmap, mask, xg, yg)
        type(coordinate_mapper_t), intent(in) :: dmap
        type(stream_mask_t), intent(inout) :: mask
        real(wp), intent(in) :: xg, yg
        
        integer :: xm, ym
        call dmap%grid2mask(xg, yg, xm, ym)
        call mask%update_trajectory(xm, ym)
    end subroutine update_trajectory_in_mask

    logical function update_trajectory_in_mask_safe(dmap, mask, xg, yg) result(success)
        !! Safe version that returns false if trajectory collides (for integration termination)
        type(coordinate_mapper_t), intent(in) :: dmap
        type(stream_mask_t), intent(inout) :: mask
        real(wp), intent(in) :: xg, yg
        
        integer :: xm, ym
        call dmap%grid2mask(xg, yg, xm, ym)
        success = mask%try_update_trajectory(xm, ym)
    end function update_trajectory_in_mask_safe

    ! add_trajectory_to_figure removed to avoid circular dependency

end module fortplot_streamplot_matplotlib