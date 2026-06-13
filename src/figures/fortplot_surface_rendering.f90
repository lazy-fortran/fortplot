module fortplot_surface_rendering
    !! Surface rendering module for 3D surface plots
    !!
    !! Single Responsibility: Render filled and wireframe 3D surfaces
    !! Extracted from fortplot_figure_rendering_pipeline for size compliance

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_projection, only: project_3d_to_2d
    use fortplot_colormap, only: colormap_value_to_color
    implicit none

    private
    public :: render_surface_plot

contains

    subroutine render_surface_plot(backend, plot, x_min_t, x_max_t, y_min_t, &
                                   y_max_t, z_min_t, z_max_t, xscale, yscale, &
                                   symlog_threshold)
        !! Render a 3D surface plot using wireframe or filled representation
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        real(wp), intent(in) :: z_min_t, z_max_t
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

        associate (unused_xs => xscale, unused_ys => yscale, &
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

        x_min = x_min_t
        x_max = x_max_t
        y_min = y_min_t
        y_max = y_max_t
        z_min = z_min_t
        z_max = z_max_t
        if (x_max <= x_min) then
            x_min = minval(plot%x_grid)
            x_max = maxval(plot%x_grid)
        end if
        if (y_max <= y_min) then
            y_min = minval(plot%y_grid)
            y_max = maxval(plot%y_grid)
        end if
        if (z_max <= z_min) then
            z_min = minval(plot%z_grid)
            z_max = maxval(plot%z_grid)
        end if

        range_x = max(1.0e-9_wp, x_max - x_min)
        range_y = max(1.0e-9_wp, y_max - y_min)
        range_z = max(1.0e-9_wp, z_max - z_min)

        azim = backend%view_azim
        elev = backend%view_elev
        dist = backend%view_dist

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
                                       proj_y_min, denom_x, denom_y, cmap, &
                                       plot%surface_edgecolor, &
                                       plot%surface_linewidth)
        else
            call render_wireframe_surface(backend, plot, nx, ny, transposed, &
                                          x_min, y_min, z_min, range_x, range_y, &
                                          range_z, azim, elev, dist, proj_x_min, &
                                          proj_y_min, denom_x, denom_y)
        end if
    end subroutine render_surface_plot

    subroutine render_filled_surface(backend, plot, nx, ny, transposed, &
                                     x_min, y_min, z_min, range_x, range_y, &
                                     range_z, azim, elev, dist, proj_x_min, &
                                     proj_y_min, denom_x, denom_y, cmap, &
                                     edge_color, edge_linewidth)
        !! Render filled surface quads using painters algorithm with interleaved
        !! wireframe edges for correct depth ordering
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        integer, intent(in) :: nx, ny
        logical, intent(in) :: transposed
        real(wp), intent(in) :: x_min, y_min, z_min
        real(wp), intent(in) :: range_x, range_y, range_z
        real(wp), intent(in) :: azim, elev, dist
        real(wp), intent(in) :: proj_x_min, proj_y_min, denom_x, denom_y
        character(len=*), intent(in) :: cmap
        real(wp), intent(in) :: edge_color(3)
        real(wp), intent(in) :: edge_linewidth

        integer :: i, j, k, n_quads
        integer, allocatable :: sorted_idx(:)
        real(wp), allocatable :: quad_depth(:)

        n_quads = (nx - 1)*(ny - 1)
        if (n_quads <= 0) return

        allocate (quad_depth(n_quads), sorted_idx(n_quads))

        k = 0
        do j = 1, ny - 1
            do i = 1, nx - 1
                k = k + 1
                quad_depth(k) = quad_view_depth(plot, i, j, transposed, &
                                                x_min, y_min, z_min, range_x, &
                                                range_y, range_z, azim, elev)
            end do
        end do

        call sort_indices_by_depth(quad_depth, sorted_idx, n_quads)

        do k = 1, n_quads
            call index_to_ij(sorted_idx(k), nx - 1, i, j)
            call render_filled_quad(backend, plot, i, j, transposed, x_min, &
                                    y_min, z_min, range_x, range_y, range_z, &
                                    azim, elev, dist, proj_x_min, proj_y_min, &
                                    denom_x, denom_y, cmap, edge_color, &
                                    edge_linewidth)
        end do
    end subroutine render_filled_surface

    subroutine quad_corner_norm(plot, i, j, transposed, x_min, y_min, z_min, &
                                range_x, range_y, range_z, x_norm, y_norm, &
                                z_norm, z_avg)
        !! Normalized cube-space corners of the (i,j) quad and its mean height
        type(plot_data_t), intent(in) :: plot
        integer, intent(in) :: i, j
        logical, intent(in) :: transposed
        real(wp), intent(in) :: x_min, y_min, z_min
        real(wp), intent(in) :: range_x, range_y, range_z
        real(wp), intent(out) :: x_norm(4), y_norm(4), z_norm(4)
        real(wp), intent(out) :: z_avg

        real(wp) :: z00, z10, z01, z11

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
        z_avg = (z00 + z10 + z01 + z11)/4.0_wp

        x_norm(1) = (plot%x_grid(i) - x_min)/range_x
        x_norm(2) = (plot%x_grid(i + 1) - x_min)/range_x
        x_norm(3) = (plot%x_grid(i + 1) - x_min)/range_x
        x_norm(4) = (plot%x_grid(i) - x_min)/range_x

        y_norm(1) = (plot%y_grid(j) - y_min)/range_y
        y_norm(2) = (plot%y_grid(j) - y_min)/range_y
        y_norm(3) = (plot%y_grid(j + 1) - y_min)/range_y
        y_norm(4) = (plot%y_grid(j + 1) - y_min)/range_y

        z_norm(1) = (z00 - z_min)/range_z
        z_norm(2) = (z10 - z_min)/range_z
        z_norm(3) = (z11 - z_min)/range_z
        z_norm(4) = (z01 - z_min)/range_z
    end subroutine quad_corner_norm

    function quad_view_depth(plot, i, j, transposed, x_min, y_min, z_min, &
                             range_x, range_y, range_z, azim, elev) &
        result(depth)
        !! Mean camera-space depth of the (i,j) quad for painter ordering
        type(plot_data_t), intent(in) :: plot
        integer, intent(in) :: i, j
        logical, intent(in) :: transposed
        real(wp), intent(in) :: x_min, y_min, z_min
        real(wp), intent(in) :: range_x, range_y, range_z
        real(wp), intent(in) :: azim, elev
        real(wp) :: depth

        real(wp) :: x_norm(4), y_norm(4), z_norm(4), z_avg

        call quad_corner_norm(plot, i, j, transposed, x_min, y_min, z_min, &
                              range_x, range_y, range_z, x_norm, y_norm, &
                              z_norm, z_avg)
        depth = mean_view_depth(x_norm, y_norm, z_norm, azim, elev)
    end function quad_view_depth

    subroutine render_filled_quad(backend, plot, i, j, transposed, x_min, &
                                  y_min, z_min, range_x, range_y, range_z, &
                                  azim, elev, dist, proj_x_min, proj_y_min, &
                                  denom_x, denom_y, cmap, edge_color, &
                                  edge_linewidth)
        !! Project, shade, fill, and edge a single surface quad
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        integer, intent(in) :: i, j
        logical, intent(in) :: transposed
        real(wp), intent(in) :: x_min, y_min, z_min
        real(wp), intent(in) :: range_x, range_y, range_z
        real(wp), intent(in) :: azim, elev, dist
        real(wp), intent(in) :: proj_x_min, proj_y_min, denom_x, denom_y
        character(len=*), intent(in) :: cmap
        real(wp), intent(in) :: edge_color(3)
        real(wp), intent(in) :: edge_linewidth

        real(wp) :: x_norm(4), y_norm(4), z_norm(4), z_avg
        real(wp) :: x_proj(4), y_proj(4), x_final(4), y_final(4)
        real(wp) :: quad_color(3), edge_rgb(3), shade

        call quad_corner_norm(plot, i, j, transposed, x_min, y_min, z_min, &
                              range_x, range_y, range_z, x_norm, y_norm, &
                              z_norm, z_avg)

        call project_3d_to_2d(x_norm, y_norm, z_norm, azim, elev, dist, &
                              x_proj, y_proj)

        x_final = x_min + (x_proj - proj_x_min)/denom_x*range_x
        y_final = y_min + (y_proj - proj_y_min)/denom_y*range_y

        call colormap_value_to_color(z_avg, z_min, z_min + range_z, cmap, &
                                     quad_color)

        ! matplotlib plot_surface default applies light-source shading
        ! (shade=True, azdeg=315, altdeg=45) so adjacent facets vary
        ! smoothly instead of reading as flat color bands.
        shade = surface_shade_factor(x_norm, y_norm, z_norm)
        quad_color = shade*quad_color

        if (plot%surface_alpha < 1.0_wp) then
            quad_color = plot%surface_alpha*quad_color + &
                         (1.0_wp - plot%surface_alpha)*1.0_wp
        end if

        call backend%color(quad_color(1), quad_color(2), quad_color(3))
        call backend%fill_quad(x_final, y_final)

        if (edge_linewidth > 0.0_wp) then
            edge_rgb = edge_color
        else
            ! Mirror matplotlib's antialiased per-quad seams: thin edges in
            ! the facet's own (shaded) color knit the mesh together without
            ! the heavy dark grid of an explicit wireframe.
            edge_rgb = quad_color
        end if
        call backend%color(edge_rgb(1), edge_rgb(2), edge_rgb(3))
        call backend%set_line_style('-')
        call backend%set_line_width(max(edge_linewidth, 0.25_wp))
        call backend%line(x_final(1), y_final(1), x_final(2), y_final(2))
        call backend%line(x_final(2), y_final(2), x_final(3), y_final(3))
        call backend%line(x_final(3), y_final(3), x_final(4), y_final(4))
        call backend%line(x_final(4), y_final(4), x_final(1), y_final(1))
    end subroutine render_filled_quad

    function surface_shade_factor(x_norm, y_norm, z_norm) result(shade)
        !! Light-source brightness factor for one surface quad, matching the
        !! default matplotlib plot_surface light (azdeg=315, altdeg=45). Returns
        !! a multiplier in [0.55, 1.0]: faces tilted toward the light stay bright,
        !! faces tilted away darken, producing a smooth shaded surface.
        real(wp), intent(in) :: x_norm(4), y_norm(4), z_norm(4)
        real(wp) :: shade
        real(wp), parameter :: light(3) = [-0.5_wp, 0.5_wp, &
                                            0.7071067811865476_wp]
        real(wp) :: e1(3), e2(3), nrm(3), nlen, intensity

        ! Two spanning edges of the quad, then the face normal via cross product.
        e1 = [x_norm(2) - x_norm(1), y_norm(2) - y_norm(1), z_norm(2) - z_norm(1)]
        e2 = [x_norm(4) - x_norm(1), y_norm(4) - y_norm(1), z_norm(4) - z_norm(1)]
        nrm(1) = e1(2)*e2(3) - e1(3)*e2(2)
        nrm(2) = e1(3)*e2(1) - e1(1)*e2(3)
        nrm(3) = e1(1)*e2(2) - e1(2)*e2(1)
        nlen = sqrt(sum(nrm**2))
        if (nlen <= 1.0e-12_wp) then
            shade = 1.0_wp
            return
        end if
        nrm = nrm/nlen

        ! Use the absolute dot so both surface orientations are lit consistently.
        intensity = abs(dot_product(nrm, light))
        shade = 0.55_wp + 0.45_wp*intensity
    end function surface_shade_factor

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
        allocate (x_vals(max_points), y_vals(max_points), z_vals(max_points))
        allocate (x_norm(max_points), y_norm(max_points), z_norm(max_points))
        allocate (x_proj(max_points), y_proj(max_points))
        allocate (x_final(max_points), y_final(max_points))

        line_color = plot%surface_edgecolor
        if (plot%surface_alpha < 1.0_wp) then
            line_color = plot%surface_alpha*line_color + &
                         (1.0_wp - plot%surface_alpha)*1.0_wp
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

            x_norm(1:m) = (x_vals(1:m) - x_min)/range_x
            y_norm(1:m) = (y_vals(1:m) - y_min)/range_y
            z_norm(1:m) = (z_vals(1:m) - z_min)/range_z

            call project_3d_to_2d(x_norm(1:m), y_norm(1:m), z_norm(1:m), &
                                  azim, elev, dist, x_proj(1:m), y_proj(1:m))

            do i = 1, m
                x_final(i) = x_min + (x_proj(i) - proj_x_min)/denom_x* &
                             range_x
                y_final(i) = y_min + (y_proj(i) - proj_y_min)/denom_y* &
                             range_y
            end do

            do i = 1, m - 1
                call backend%line(x_final(i), y_final(i), x_final(i + 1), &
                                  y_final(i + 1))
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

            x_norm(1:m) = (x_vals(1:m) - x_min)/range_x
            y_norm(1:m) = (y_vals(1:m) - y_min)/range_y
            z_norm(1:m) = (z_vals(1:m) - z_min)/range_z

            call project_3d_to_2d(x_norm(1:m), y_norm(1:m), z_norm(1:m), &
                                  azim, elev, dist, x_proj(1:m), y_proj(1:m))

            do j = 1, m
                x_final(j) = x_min + (x_proj(j) - proj_x_min)/denom_x* &
                             range_x
                y_final(j) = y_min + (y_proj(j) - proj_y_min)/denom_y* &
                             range_y
            end do

            do j = 1, m - 1
                call backend%line(x_final(j), y_final(j), x_final(j + 1), &
                                  y_final(j + 1))
            end do
        end do
    end subroutine render_wireframe_surface

    function mean_view_depth(x_norm, y_norm, z_norm, azim, elev) result(depth)
        !! Camera-space depth after the same rotations used for projection.
        real(wp), intent(in) :: x_norm(4), y_norm(4), z_norm(4)
        real(wp), intent(in) :: azim, elev
        real(wp) :: depth
        real(wp) :: cos_azim, sin_azim, cos_elev, sin_elev
        real(wp) :: planar(4), z_view(4)

        cos_azim = cos(azim)
        sin_azim = sin(azim)
        cos_elev = cos(elev)
        sin_elev = sin(elev)

        ! Match project_3d_to_2d's matplotlib-style camera depth (toward viewer).
        planar = x_norm*cos_azim + y_norm*sin_azim
        z_view = planar*cos_elev + z_norm*sin_elev
        depth = sum(z_view)/4.0_wp
    end function mean_view_depth

    subroutine sort_indices_by_depth(depths, indices, n)
        !! Sort indices by depth (back to front for painters algorithm)
        real(wp), contiguous, intent(in) :: depths(:)
        integer, intent(out) :: indices(:)
        integer, intent(in) :: n

        integer :: i, j, min_idx, temp_idx
        real(wp) :: temp_depth
        real(wp), allocatable :: temp_depths(:)

        allocate (temp_depths(n))
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

                temp_depth = temp_depths(i)
                temp_depths(i) = temp_depths(min_idx)
                temp_depths(min_idx) = temp_depth
            end if
        end do
    end subroutine sort_indices_by_depth

    subroutine index_to_ij(idx, row_size, i, j)
        !! Convert linear index to i,j indices (1-based)
        integer, intent(in) :: idx, row_size
        integer, intent(out) :: i, j

        j = (idx - 1)/row_size + 1
        i = mod(idx - 1, row_size) + 1
    end subroutine index_to_ij

end module fortplot_surface_rendering
