module fortplot_surface_rendering
    !! Surface plot rendering module extracted for file size compliance
    !! Contains 3D surface projection and wireframe generation
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    implicit none
    
    private
    public :: render_surface_plot
    
contains

    subroutine render_surface_plot(backend, plot, x_min_t, x_max_t, y_min_t, y_max_t, &
                                   xscale, yscale, symlog_threshold)
        !! Render a 3D surface plot using projected wireframe representation
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer :: nx, ny, max_points, i, j, m
        real(wp) :: x_min, x_max, y_min, y_max, z_min, z_max
        real(wp) :: range_x, range_y, range_z
        real(wp) :: azim, elev, dist
        real(wp) :: x_corners(8), y_corners(8), z_corners(8)
        real(wp) :: x_proj_corners(8), y_proj_corners(8)
        real(wp) :: proj_x_min, proj_x_max, proj_y_min, proj_y_max
        real(wp) :: denom_x, denom_y
        real(wp), allocatable :: x_vals(:), y_vals(:), z_vals(:)
        real(wp), allocatable :: x_norm(:), y_norm(:), z_norm(:)
        real(wp), allocatable :: x_proj(:), y_proj(:)
        real(wp), allocatable :: x_final(:), y_final(:)
        real(wp) :: line_color(3)
        logical :: transposed

        associate(unused_xt => x_min_t, unused_xx => x_max_t, unused_yt => y_min_t, unused_yx => y_max_t)
        end associate
        associate(unused_xs => xscale, unused_ys => yscale, unused_st => symlog_threshold)
        end associate

        if (.not. allocated(plot%z_data)) return

        nx = size(plot%z_data, 1)
        ny = size(plot%z_data, 2)
        if (nx < 2 .or. ny < 2) return

        call get_default_view_angles(azim, elev)
        dist = 10.0_wp

        x_min = minval(plot%x_grid)
        x_max = maxval(plot%x_grid)
        y_min = minval(plot%y_grid)
        y_max = maxval(plot%y_grid)
        z_min = minval(plot%z_data)
        z_max = maxval(plot%z_data)

        range_x = x_max - x_min
        range_y = y_max - y_min
        range_z = z_max - z_min

        if (range_x <= 0.0_wp) range_x = 1.0_wp
        if (range_y <= 0.0_wp) range_y = 1.0_wp
        if (range_z <= 0.0_wp) range_z = 1.0_wp

        x_corners = [x_min, x_max, x_max, x_min, x_min, x_max, x_max, x_min]
        y_corners = [y_min, y_min, y_max, y_max, y_min, y_min, y_max, y_max]
        z_corners = [z_min, z_min, z_min, z_min, z_max, z_max, z_max, z_max]

        do i = 1, 8
            call project_3d_to_2d((x_corners(i) - x_min) / range_x - 0.5_wp, &
                                  (y_corners(i) - y_min) / range_y - 0.5_wp, &
                                  (z_corners(i) - z_min) / range_z - 0.5_wp, &
                                  azim, elev, dist, &
                                  x_proj_corners(i), y_proj_corners(i))
        end do

        proj_x_min = minval(x_proj_corners)
        proj_x_max = maxval(x_proj_corners)
        proj_y_min = minval(y_proj_corners)
        proj_y_max = maxval(y_proj_corners)

        denom_x = proj_x_max - proj_x_min
        denom_y = proj_y_max - proj_y_min
        if (abs(denom_x) < 1.0e-10_wp) denom_x = 1.0_wp
        if (abs(denom_y) < 1.0e-10_wp) denom_y = 1.0_wp

        max_points = nx * ny
        allocate(x_vals(max_points), y_vals(max_points), z_vals(max_points))
        allocate(x_norm(max_points), y_norm(max_points), z_norm(max_points))
        allocate(x_proj(max_points), y_proj(max_points))
        allocate(x_final(max_points), y_final(max_points))

        m = 0
        transposed = (size(plot%x_grid) == ny .and. size(plot%y_grid) == nx)

        do j = 1, ny
            do i = 1, nx
                m = m + 1
                if (transposed) then
                    x_vals(m) = plot%x_grid(j)
                    y_vals(m) = plot%y_grid(i)
                else
                    x_vals(m) = plot%x_grid(i)
                    y_vals(m) = plot%y_grid(j)
                end if
                z_vals(m) = plot%z_data(i, j)
            end do
        end do

        x_norm = (x_vals - x_min) / range_x - 0.5_wp
        y_norm = (y_vals - y_min) / range_y - 0.5_wp
        z_norm = (z_vals - z_min) / range_z - 0.5_wp

        do i = 1, m
            call project_3d_to_2d(x_norm(i), y_norm(i), z_norm(i), azim, elev, dist, &
                                  x_proj(i), y_proj(i))
        end do

        x_final = (x_proj - proj_x_min) / denom_x
        y_final = (y_proj - proj_y_min) / denom_y

        line_color = [0.0_wp, 0.0_wp, 1.0_wp]
        call backend%set_color(line_color(1), line_color(2), line_color(3))

        call render_wireframe_surface(backend, x_final, y_final, nx, ny)

        deallocate(x_vals, y_vals, z_vals)
        deallocate(x_norm, y_norm, z_norm)
        deallocate(x_proj, y_proj)
        deallocate(x_final, y_final)
    end subroutine render_surface_plot

    subroutine render_wireframe_surface(backend, x_final, y_final, nx, ny)
        !! Render wireframe representation of surface
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x_final(:), y_final(:)
        integer, intent(in) :: nx, ny
        
        integer :: i, j, idx1, idx2
        real(wp) :: x_line(2), y_line(2)

        ! Draw horizontal lines
        do j = 1, ny
            do i = 1, nx - 1
                idx1 = (j - 1) * nx + i
                idx2 = (j - 1) * nx + i + 1
                if (idx1 <= size(x_final) .and. idx2 <= size(x_final)) then
                    x_line = [x_final(idx1), x_final(idx2)]
                    y_line = [y_final(idx1), y_final(idx2)]
                    call backend%draw_line(x_line, y_line)
                end if
            end do
        end do

        ! Draw vertical lines
        do i = 1, nx
            do j = 1, ny - 1
                idx1 = (j - 1) * nx + i
                idx2 = j * nx + i
                if (idx1 <= size(x_final) .and. idx2 <= size(x_final)) then
                    x_line = [x_final(idx1), x_final(idx2)]
                    y_line = [y_final(idx1), y_final(idx2)]
                    call backend%draw_line(x_line, y_line)
                end if
            end do
        end do
    end subroutine render_wireframe_surface

end module fortplot_surface_rendering