module fortplot_surface_rendering
    !! Surface rendering module for 3D surface plots
    !!
    !! Single Responsibility: Render filled and wireframe 3D surfaces
    !! Extracted from fortplot_figure_rendering_pipeline for size compliance

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    use fortplot_colormap, only: colormap_value_to_color
    implicit none

    private
    public :: render_surface_plot

contains

    subroutine render_surface_plot(backend, plot, x_min_t, x_max_t, y_min_t, &
                                   y_max_t, xscale, yscale, symlog_threshold)
        !! Render a 3D surface plot using wireframe or filled representation
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer :: nx, ny
        real(wp) :: x_min, x_max, y_min, y_max, z_min, z_max
        real(wp) :: range_x, range_y, range_z
        real(wp) :: azim, elev, dist
        real(wp) :: x_corners(8), y_corners(8), z_corners(8)
        real(wp) :: x_proj_corners(8), y_proj_corners(8)
        real(wp) :: proj_x_min, proj_x_max, proj_y_min, proj_y_max
        real(wp) :: denom_x, denom_y
        logical :: transposed
        character(len=20) :: cmap

        associate(unused_xt => x_min_t, unused_xx => x_max_t, &
                  unused_yt => y_min_t, unused_yx => y_max_t)
        end associate
        associate(unused_xs => xscale, unused_ys => yscale, &
                  unused_st => symlog_threshold)
        end associate

        if (.not. allocated(plot%x_grid)) return
        if (.not. allocated(plot%y_grid)) return
        if (.not. allocated(plot%z_grid)) return

        nx = size(plot%x_grid)
        ny = size(plot%y_grid)
        if (nx < 2 .or. ny < 2) return
        if (size(plot%z_grid, 1) == ny .and. size(plot%z_grid, 2) == nx) then
            transposed = .false.
        else if (size(plot%z_grid, 1) == nx .and. size(plot%z_grid, 2) == ny) then
            transposed = .true.
        else
            return
        end if

        x_min = minval(plot%x_grid)
        x_max = maxval(plot%x_grid)
        y_min = minval(plot%y_grid)
        y_max = maxval(plot%y_grid)
        z_min = minval(plot%z_grid)
        z_max = maxval(plot%z_grid)

        range_x = max(1.0e-9_wp, x_max - x_min)
        range_y = max(1.0e-9_wp, y_max - y_min)
        range_z = max(1.0e-9_wp, z_max - z_min)

        call get_default_view_angles(azim, elev, dist)

        x_corners = [0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, &
                     0.0_wp]
        y_corners = [0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, &
                     1.0_wp]
        z_corners = [0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, &
                     1.0_wp]
        call project_3d_to_2d(x_corners, y_corners, z_corners, azim, elev, &
                              dist, x_proj_corners, y_proj_corners)

        proj_x_min = minval(x_proj_corners)
        proj_x_max = maxval(x_proj_corners)
        proj_y_min = minval(y_proj_corners)
        proj_y_max = maxval(y_proj_corners)
        denom_x = max(1.0e-9_wp, proj_x_max - proj_x_min)
        denom_y = max(1.0e-9_wp, proj_y_max - proj_y_min)

        cmap = 'viridis'
        if (allocated(plot%surface_colormap)) then
            if (len_trim(plot%surface_colormap) > 0) then
                cmap = plot%surface_colormap
            end if
        end if

        if (plot%surface_filled) then
            call render_filled_surface(backend, plot, nx, ny, transposed, &
                                       x_min, y_min, z_min, range_x, range_y, &
                                       range_z, azim, elev, dist, proj_x_min, &
                                       proj_y_min, denom_x, denom_y, cmap)
        end if

        call render_wireframe_surface(backend, plot, nx, ny, transposed, &
                                      x_min, y_min, z_min, range_x, range_y, &
                                      range_z, azim, elev, dist, proj_x_min, &
                                      proj_y_min, denom_x, denom_y)
    end subroutine render_surface_plot

    subroutine render_filled_surface(backend, plot, nx, ny, transposed, &
                                     x_min, y_min, z_min, range_x, range_y, &
                                     range_z, azim, elev, dist, proj_x_min, &
                                     proj_y_min, denom_x, denom_y, cmap)
        !! Render filled surface quads using painters algorithm
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        integer, intent(in) :: nx, ny
        logical, intent(in) :: transposed
        real(wp), intent(in) :: x_min, y_min, z_min
        real(wp), intent(in) :: range_x, range_y, range_z
        real(wp), intent(in) :: azim, elev, dist
        real(wp), intent(in) :: proj_x_min, proj_y_min, denom_x, denom_y
        character(len=*), intent(in) :: cmap

        integer :: i, j, k, n_quads
        integer, allocatable :: sorted_idx(:)
        real(wp), allocatable :: quad_depth(:)
        real(wp) :: z00, z10, z01, z11, z_avg
        real(wp) :: x_norm(4), y_norm(4), z_norm(4)
        real(wp) :: x_proj(4), y_proj(4), x_final(4), y_final(4)
        real(wp) :: quad_color(3)

        n_quads = (nx - 1) * (ny - 1)
        if (n_quads <= 0) return

        allocate(quad_depth(n_quads), sorted_idx(n_quads))

        k = 0
        do j = 1, ny - 1
            do i = 1, nx - 1
                k = k + 1
                if (.not. transposed) then
                    z00 = plot%z_grid(j, i)
                    z10 = plot%z_grid(j, i + 1)
                    z01 = plot%z_grid(j + 1, i)
                    z11 = plot%z_grid(j + 1, i + 1)
                else
                    z00 = plot%z_grid(i, j)
                    z10 = plot%z_grid(i + 1, j)
                    z01 = plot%z_grid(i, j + 1)
                    z11 = plot%z_grid(i + 1, j + 1)
                end if
                quad_depth(k) = (z00 + z10 + z01 + z11) / 4.0_wp
            end do
        end do

        call sort_indices_by_depth(quad_depth, sorted_idx, n_quads)

        do k = 1, n_quads
            call index_to_ij(sorted_idx(k), nx - 1, i, j)

            if (.not. transposed) then
                z00 = plot%z_grid(j, i)
                z10 = plot%z_grid(j, i + 1)
                z01 = plot%z_grid(j + 1, i)
                z11 = plot%z_grid(j + 1, i + 1)
            else
                z00 = plot%z_grid(i, j)
                z10 = plot%z_grid(i + 1, j)
                z01 = plot%z_grid(i, j + 1)
                z11 = plot%z_grid(i + 1, j + 1)
            end if
            z_avg = (z00 + z10 + z01 + z11) / 4.0_wp

            x_norm(1) = (plot%x_grid(i) - x_min) / range_x
            x_norm(2) = (plot%x_grid(i + 1) - x_min) / range_x
            x_norm(3) = (plot%x_grid(i + 1) - x_min) / range_x
            x_norm(4) = (plot%x_grid(i) - x_min) / range_x

            y_norm(1) = (plot%y_grid(j) - y_min) / range_y
            y_norm(2) = (plot%y_grid(j) - y_min) / range_y
            y_norm(3) = (plot%y_grid(j + 1) - y_min) / range_y
            y_norm(4) = (plot%y_grid(j + 1) - y_min) / range_y

            z_norm(1) = (z00 - z_min) / range_z
            z_norm(2) = (z10 - z_min) / range_z
            z_norm(3) = (z11 - z_min) / range_z
            z_norm(4) = (z01 - z_min) / range_z

            call project_3d_to_2d(x_norm, y_norm, z_norm, azim, elev, dist, &
                                  x_proj, y_proj)

            x_final(1) = x_min + (x_proj(1) - proj_x_min) / denom_x * range_x
            x_final(2) = x_min + (x_proj(2) - proj_x_min) / denom_x * range_x
            x_final(3) = x_min + (x_proj(3) - proj_x_min) / denom_x * range_x
            x_final(4) = x_min + (x_proj(4) - proj_x_min) / denom_x * range_x

            y_final(1) = y_min + (y_proj(1) - proj_y_min) / denom_y * range_y
            y_final(2) = y_min + (y_proj(2) - proj_y_min) / denom_y * range_y
            y_final(3) = y_min + (y_proj(3) - proj_y_min) / denom_y * range_y
            y_final(4) = y_min + (y_proj(4) - proj_y_min) / denom_y * range_y

            call colormap_value_to_color(z_avg, z_min, z_min + range_z, cmap, &
                                         quad_color)

            if (plot%surface_alpha < 1.0_wp) then
                quad_color = plot%surface_alpha * quad_color + &
                            (1.0_wp - plot%surface_alpha) * 1.0_wp
            end if

            call backend%color(quad_color(1), quad_color(2), quad_color(3))
            call backend%fill_quad(x_final, y_final)
        end do

        deallocate(quad_depth, sorted_idx)
    end subroutine render_filled_surface

    subroutine render_wireframe_surface(backend, plot, nx, ny, transposed, &
                                        x_min, y_min, z_min, range_x, range_y, &
                                        range_z, azim, elev, dist, proj_x_min, &
                                        proj_y_min, denom_x, denom_y)
        !! Render wireframe lines for surface plot
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        integer, intent(in) :: nx, ny
        logical, intent(in) :: transposed
        real(wp), intent(in) :: x_min, y_min, z_min
        real(wp), intent(in) :: range_x, range_y, range_z
        real(wp), intent(in) :: azim, elev, dist
        real(wp), intent(in) :: proj_x_min, proj_y_min, denom_x, denom_y

        integer :: i, j, m, max_points
        real(wp), allocatable :: x_vals(:), y_vals(:), z_vals(:)
        real(wp), allocatable :: x_norm(:), y_norm(:), z_norm(:)
        real(wp), allocatable :: x_proj(:), y_proj(:)
        real(wp), allocatable :: x_final(:), y_final(:)
        real(wp) :: line_color(3)

        max_points = max(nx, ny)
        allocate(x_vals(max_points), y_vals(max_points), z_vals(max_points))
        allocate(x_norm(max_points), y_norm(max_points), z_norm(max_points))
        allocate(x_proj(max_points), y_proj(max_points))
        allocate(x_final(max_points), y_final(max_points))

        line_color = plot%surface_edgecolor
        if (plot%surface_alpha < 1.0_wp) then
            line_color = plot%surface_alpha * line_color + &
                        (1.0_wp - plot%surface_alpha) * 1.0_wp
        end if
        call backend%color(line_color(1), line_color(2), line_color(3))
        call backend%set_line_style('-')
        call backend%set_line_width(plot%surface_linewidth)

        do j = 1, ny
            m = nx
            x_vals(1:m) = plot%x_grid
            y_vals(1:m) = plot%y_grid(j)
            if (.not. transposed) then
                z_vals(1:m) = plot%z_grid(j, :)
            else
                z_vals(1:m) = plot%z_grid(:, j)
            end if

            x_norm(1:m) = (x_vals(1:m) - x_min) / range_x
            y_norm(1:m) = (y_vals(1:m) - y_min) / range_y
            z_norm(1:m) = (z_vals(1:m) - z_min) / range_z

            call project_3d_to_2d(x_norm(1:m), y_norm(1:m), z_norm(1:m), &
                                  azim, elev, dist, x_proj(1:m), y_proj(1:m))

            do i = 1, m
                x_final(i) = x_min + (x_proj(i) - proj_x_min) / denom_x * &
                             range_x
                y_final(i) = y_min + (y_proj(i) - proj_y_min) / denom_y * &
                             range_y
            end do

            do i = 1, m - 1
                call backend%line(x_final(i), y_final(i), x_final(i+1), &
                                  y_final(i+1))
            end do
        end do

        do i = 1, nx
            m = ny
            x_vals(1:m) = plot%x_grid(i)
            y_vals(1:m) = plot%y_grid
            if (.not. transposed) then
                z_vals(1:m) = plot%z_grid(:, i)
            else
                z_vals(1:m) = plot%z_grid(i, :)
            end if

            x_norm(1:m) = (x_vals(1:m) - x_min) / range_x
            y_norm(1:m) = (y_vals(1:m) - y_min) / range_y
            z_norm(1:m) = (z_vals(1:m) - z_min) / range_z

            call project_3d_to_2d(x_norm(1:m), y_norm(1:m), z_norm(1:m), &
                                  azim, elev, dist, x_proj(1:m), y_proj(1:m))

            do j = 1, m
                x_final(j) = x_min + (x_proj(j) - proj_x_min) / denom_x * &
                             range_x
                y_final(j) = y_min + (y_proj(j) - proj_y_min) / denom_y * &
                             range_y
            end do

            do j = 1, m - 1
                call backend%line(x_final(j), y_final(j), x_final(j+1), &
                                  y_final(j+1))
            end do
        end do

        deallocate(x_vals, y_vals, z_vals, x_norm, y_norm, z_norm, x_proj, &
                   y_proj, x_final, y_final)
    end subroutine render_wireframe_surface

    subroutine sort_indices_by_depth(depths, indices, n)
        !! Sort indices by depth (back to front for painters algorithm)
        real(wp), intent(in) :: depths(:)
        integer, intent(out) :: indices(:)
        integer, intent(in) :: n

        integer :: i, j, min_idx, temp_idx
        real(wp), allocatable :: temp_depths(:)

        allocate(temp_depths(n))
        temp_depths = depths(1:n)

        do i = 1, n
            indices(i) = i
        end do

        do i = 1, n - 1
            min_idx = i
            do j = i + 1, n
                if (temp_depths(j) < temp_depths(min_idx)) then
                    min_idx = j
                end if
            end do
            if (min_idx /= i) then
                temp_idx = indices(i)
                indices(i) = indices(min_idx)
                indices(min_idx) = temp_idx

                temp_depths(min_idx) = temp_depths(i)
            end if
        end do

        deallocate(temp_depths)
    end subroutine sort_indices_by_depth

    subroutine index_to_ij(idx, row_size, i, j)
        !! Convert linear index to i,j indices (1-based)
        integer, intent(in) :: idx, row_size
        integer, intent(out) :: i, j

        j = (idx - 1) / row_size + 1
        i = mod(idx - 1, row_size) + 1
    end subroutine index_to_ij

end module fortplot_surface_rendering
