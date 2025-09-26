module fortplot_figure_rendering_pipeline
    !! Figure rendering pipeline module
    !! 
    !! Single Responsibility: Coordinate the complete rendering pipeline
    !! Extracted from fortplot_figure_core to improve modularity
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_figure_data_ranges, only: calculate_figure_data_ranges
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_LINE, &
                                  PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                  PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
                                  PLOT_TYPE_BOXPLOT, PLOT_TYPE_ERRORBAR, &
                                  PLOT_TYPE_SURFACE, PLOT_TYPE_PIE, &
                                  PLOT_TYPE_BAR
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    use fortplot_rendering, only: render_line_plot, render_contour_plot, &
                                 render_pcolormesh_plot, render_fill_between_plot, &
                                 render_markers, render_boxplot_plot, render_errorbar_plot, &
                                 render_pie_plot, render_bar_plot
    use fortplot_legend, only: legend_t
    implicit none
    
    private
    public :: calculate_figure_data_ranges, setup_coordinate_system
    public :: render_figure_background, render_figure_axes, render_all_plots
    public :: render_figure_axes_labels_only
    
contains
    
    
    subroutine setup_coordinate_system(backend, x_min_transformed, x_max_transformed, &
                                      y_min_transformed, y_max_transformed)
        !! Setup the coordinate system for rendering
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x_min_transformed, x_max_transformed
        real(wp), intent(in) :: y_min_transformed, y_max_transformed
        
        ! Set data ranges directly on backend
        backend%x_min = x_min_transformed
        backend%x_max = x_max_transformed
        backend%y_min = y_min_transformed
        backend%y_max = y_max_transformed
    end subroutine setup_coordinate_system
    
    subroutine render_figure_background(backend)
        !! Render figure background
        class(plot_context), intent(inout) :: backend
        ! Background clearing is handled by backend-specific rendering
    end subroutine render_figure_background
    
    subroutine render_figure_axes(backend, xscale, yscale, symlog_threshold, &
                                 x_min, x_max, y_min, y_max, title, xlabel, ylabel, &
                                 plots, plot_count)
        !! Render figure axes and labels
        !! For raster backends, split rendering to prevent label overlap issues
        use fortplot_raster, only: raster_context
        class(plot_context), intent(inout) :: backend
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in) :: title, xlabel, ylabel
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical :: has_3d
        real(wp) :: zmin, zmax

        call detect_3d_extent(plots, plot_count, has_3d, zmin, zmax)
        ! Check if this is a raster backend and use split rendering if so
        select type (backend)
        class is (raster_context)
            if (has_3d) then
                ! For 3D, delegate full axes (3D frame + labels) to backend
                call backend%draw_axes_and_labels_backend(xscale, yscale, &
                                                         symlog_threshold, &
                                                         x_min, x_max, &
                                                         y_min, y_max, &
                                                         title, xlabel, &
                                                         ylabel, &
                                                         z_min=zmin, z_max=zmax, &
                                                         has_3d_plots=.true.)
            else
                ! For raster backends, only draw axes lines and tick marks here
                ! Labels will be drawn later after plots to prevent overlap
                call backend%draw_axes_lines_and_ticks(xscale, yscale, &
                                                      symlog_threshold, &
                                                      x_min, x_max, &
                                                      y_min, y_max)
            end if
        class default
            ! For non-raster backends, use standard rendering
            call backend%draw_axes_and_labels_backend(xscale, yscale, &
                                                     symlog_threshold, &
                                                     x_min, x_max, &
                                                     y_min, y_max, &
                                                     title, xlabel, &
                                                     ylabel, &
                                                     z_min=zmin, z_max=zmax, &
                                                     has_3d_plots=has_3d)
        end select
    end subroutine render_figure_axes

    subroutine render_figure_axes_labels_only(backend, xscale, yscale, symlog_threshold, &
                                             x_min, x_max, y_min, y_max, &
                                             title, xlabel, ylabel, &
                                             plots, plot_count)
        !! Render ONLY axis labels (for raster backends after plots are drawn)
        use fortplot_raster, only: raster_context
        class(plot_context), intent(inout) :: backend
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in) :: title, xlabel, ylabel
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical :: has_3d
        real(wp) :: zmin_dummy, zmax_dummy
        call detect_3d_extent(plots, plot_count, has_3d, zmin_dummy, zmax_dummy)
        
        ! Only render labels for raster backends
        select type (backend)
        class is (raster_context)
            if (.not. has_3d) then
                call backend%draw_axis_labels_only(xscale, yscale, &
                                                  symlog_threshold, &
                                                  x_min, x_max, &
                                                  y_min, y_max, &
                                                  title, xlabel, &
                                                  ylabel)
            end if
        end select
    end subroutine render_figure_axes_labels_only

    subroutine detect_3d_extent(plots, plot_count, has_3d, zmin, zmax)
        !! Detect if any plot is 3D and compute z-range
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(out) :: has_3d
        real(wp), intent(out) :: zmin, zmax
        integer :: i
        logical :: first

        has_3d = .false.
        first = .true.
        zmin = 0.0_wp
        zmax = 1.0_wp
        do i = 1, plot_count
            if (plots(i)%is_3d()) then
                has_3d = .true.
                if (allocated(plots(i)%z)) then
                    if (size(plots(i)%z) > 0) then
                        if (first) then
                            zmin = minval(plots(i)%z)
                            zmax = maxval(plots(i)%z)
                            first = .false.
                        else
                            zmin = min(zmin, minval(plots(i)%z))
                            zmax = max(zmax, maxval(plots(i)%z))
                        end if
                    end if
                end if
                if (allocated(plots(i)%z_grid)) then
                    if (size(plots(i)%z_grid) > 0) then
                        if (first) then
                            zmin = minval(plots(i)%z_grid)
                            zmax = maxval(plots(i)%z_grid)
                            first = .false.
                        else
                            zmin = min(zmin, minval(plots(i)%z_grid))
                            zmax = max(zmax, maxval(plots(i)%z_grid))
                        end if
                    end if
                end if
            end if
        end do
    end subroutine detect_3d_extent

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

        x_corners = [0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp]
        y_corners = [0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp]
        z_corners = [0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]
        call project_3d_to_2d(x_corners, y_corners, z_corners, azim, elev, dist, &
                              x_proj_corners, y_proj_corners)

        proj_x_min = minval(x_proj_corners)
        proj_x_max = maxval(x_proj_corners)
        proj_y_min = minval(y_proj_corners)
        proj_y_max = maxval(y_proj_corners)
        denom_x = max(1.0e-9_wp, proj_x_max - proj_x_min)
        denom_y = max(1.0e-9_wp, proj_y_max - proj_y_min)

        max_points = max(nx, ny)
        allocate(x_vals(max_points), y_vals(max_points), z_vals(max_points))
        allocate(x_norm(max_points), y_norm(max_points), z_norm(max_points))
        allocate(x_proj(max_points), y_proj(max_points))
        allocate(x_final(max_points), y_final(max_points))

        line_color = plot%surface_edgecolor
        if (plot%surface_alpha < 1.0_wp) then
            line_color = plot%surface_alpha * line_color + (1.0_wp - plot%surface_alpha) * 1.0_wp
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

            if (range_x > 1.0e-9_wp) then
                x_norm(1:m) = (x_vals(1:m) - x_min) / range_x
            else
                x_norm(1:m) = 0.0_wp
            end if
            if (range_y > 1.0e-9_wp) then
                y_norm(1:m) = (y_vals(1:m) - y_min) / range_y
            else
                y_norm(1:m) = 0.0_wp
            end if
            if (range_z > 1.0e-9_wp) then
                z_norm(1:m) = (z_vals(1:m) - z_min) / range_z
            else
                z_norm(1:m) = 0.0_wp
            end if

            call project_3d_to_2d(x_norm(1:m), y_norm(1:m), z_norm(1:m), azim, elev, dist, &
                                  x_proj(1:m), y_proj(1:m))

            do i = 1, m
                x_final(i) = x_min + (x_proj(i) - proj_x_min) / denom_x * range_x
                y_final(i) = y_min + (y_proj(i) - proj_y_min) / denom_y * range_y
            end do

            do i = 1, m - 1
                call backend%line(x_final(i), y_final(i), x_final(i+1), y_final(i+1))
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

            if (range_x > 1.0e-9_wp) then
                x_norm(1:m) = (x_vals(1:m) - x_min) / range_x
            else
                x_norm(1:m) = 0.0_wp
            end if
            if (range_y > 1.0e-9_wp) then
                y_norm(1:m) = (y_vals(1:m) - y_min) / range_y
            else
                y_norm(1:m) = 0.0_wp
            end if
            if (range_z > 1.0e-9_wp) then
                z_norm(1:m) = (z_vals(1:m) - z_min) / range_z
            else
                z_norm(1:m) = 0.0_wp
            end if

            call project_3d_to_2d(x_norm(1:m), y_norm(1:m), z_norm(1:m), azim, elev, dist, &
                                  x_proj(1:m), y_proj(1:m))

            do j = 1, m
                x_final(j) = x_min + (x_proj(j) - proj_x_min) / denom_x * range_x
                y_final(j) = y_min + (y_proj(j) - proj_y_min) / denom_y * range_y
            end do

            do j = 1, m - 1
                call backend%line(x_final(j), y_final(j), x_final(j+1), y_final(j+1))
            end do
        end do

        deallocate(x_vals, y_vals, z_vals, x_norm, y_norm, z_norm, x_proj, y_proj, x_final, y_final)
    end subroutine render_surface_plot
    
    subroutine render_all_plots(backend, plots, plot_count, &
                               x_min_transformed, x_max_transformed, &
                               y_min_transformed, y_max_transformed, &
                               xscale, yscale, symlog_threshold, &
                               width, height, margin_left, margin_right, &
                               margin_bottom, margin_top)
        !! Render all plots in the figure
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        real(wp), intent(in) :: x_min_transformed, x_max_transformed
        real(wp), intent(in) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top
        
        integer :: i
        
        do i = 1, plot_count
            select case (plots(i)%plot_type)
            case (PLOT_TYPE_LINE)
                call render_line_plot(backend, plots(i), &
                                    xscale, yscale, symlog_threshold)
                
                if (allocated(plots(i)%marker)) then
                    call render_markers(backend, plots(i), &
                                      x_min_transformed, x_max_transformed, &
                                      y_min_transformed, y_max_transformed, &
                                      xscale, yscale, symlog_threshold)
                end if
                
            case (PLOT_TYPE_SCATTER)
                ! Scatter plots render only markers (no connecting line)
                if (allocated(plots(i)%marker)) then
                    call render_markers(backend, plots(i), &
                                      x_min_transformed, x_max_transformed, &
                                      y_min_transformed, y_max_transformed, &
                                      xscale, yscale, symlog_threshold)
                end if

            case (PLOT_TYPE_CONTOUR)
                call render_contour_plot(backend, plots(i), &
                                       x_min_transformed, x_max_transformed, &
                                       y_min_transformed, y_max_transformed, &
                                       xscale, yscale, symlog_threshold, &
                                       width, height, &
                                       margin_left, margin_right, &
                                       margin_bottom, margin_top)

            case (PLOT_TYPE_PCOLORMESH)
                call render_pcolormesh_plot(backend, plots(i), &
                                          x_min_transformed, x_max_transformed, &
                                          y_min_transformed, y_max_transformed, &
                                          xscale, yscale, symlog_threshold, &
                                          width, height, margin_right)

            case (PLOT_TYPE_SURFACE)
                call render_surface_plot(backend, plots(i), &
                                         x_min_transformed, x_max_transformed, &
                                         y_min_transformed, y_max_transformed, &
                                         xscale, yscale, symlog_threshold)

            case (PLOT_TYPE_FILL)
                call render_fill_between_plot(backend, plots(i), xscale, yscale, &
                                              symlog_threshold)

            case (PLOT_TYPE_BAR)
                call render_bar_plot(backend, plots(i), xscale, yscale, &
                                     symlog_threshold)

            case (PLOT_TYPE_PIE)
                call render_pie_plot(backend, plots(i), xscale, yscale, symlog_threshold)

            case (PLOT_TYPE_BOXPLOT)
                call render_boxplot_plot(backend, plots(i), xscale, yscale, &
                                         symlog_threshold)

            case (PLOT_TYPE_ERRORBAR)
                call render_errorbar_plot(backend, plots(i), xscale, yscale, &
                                          symlog_threshold)
                ! Always attempt to render markers for errorbar plots; 
                ! render_markers internally validates presence/emptiness.
                call render_markers(backend, plots(i), &
                                   x_min_transformed, x_max_transformed, &
                                   y_min_transformed, y_max_transformed, &
                                   xscale, yscale, symlog_threshold)

            end select
        end do
    end subroutine render_all_plots

end module fortplot_figure_rendering_pipeline
