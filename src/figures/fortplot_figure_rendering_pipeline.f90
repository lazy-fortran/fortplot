module fortplot_figure_rendering_pipeline
    !! Figure rendering pipeline module
    !! 
    !! Single Responsibility: Coordinate the complete rendering pipeline
    !! Extracted from fortplot_figure_core to improve modularity
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_figure_data_ranges, only: calculate_figure_data_ranges
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, PLOT_TYPE_LINE, &
                                  PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                  PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
                                  PLOT_TYPE_BOXPLOT, PLOT_TYPE_ERRORBAR, &
                                  PLOT_TYPE_SURFACE, PLOT_TYPE_PIE, &
                                  PLOT_TYPE_BAR, PLOT_TYPE_REFLINE, &
                                  AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_raster_axes, only: raster_draw_secondary_y_axis, &
                                     raster_draw_secondary_x_axis_top
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
    public :: render_streamplot_arrows
    public :: render_figure_axes_labels_only, render_title_only
    
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
                                 plots, plot_count, has_twinx, twinx_y_min, twinx_y_max, &
                                 twinx_ylabel, twinx_yscale, has_twiny, twiny_x_min, twiny_x_max, &
                                 twiny_xlabel, twiny_xscale)
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
        logical, intent(in), optional :: has_twinx, has_twiny
        real(wp), intent(in), optional :: twinx_y_min, twinx_y_max
        real(wp), intent(in), optional :: twiny_x_min, twiny_x_max
        character(len=:), allocatable, intent(in), optional :: twinx_ylabel, twiny_xlabel
        character(len=*), intent(in), optional :: twinx_yscale, twiny_xscale
        logical :: has_3d
        real(wp) :: zmin, zmax
        logical :: has_twinx_local, has_twiny_local
        real(wp) :: twinx_y_min_local, twinx_y_max_local
        real(wp) :: twiny_x_min_local, twiny_x_max_local
        character(len=16) :: twinx_scale_local, twiny_scale_local

        call detect_3d_extent(plots, plot_count, has_3d, zmin, zmax)

        has_twinx_local = .false.
        has_twiny_local = .false.
        twinx_scale_local = yscale
        twiny_scale_local = xscale

        if (present(has_twinx)) then
            has_twinx_local = has_twinx
            if (has_twinx_local) then
                if (present(twinx_y_min) .and. present(twinx_y_max)) then
                    twinx_y_min_local = twinx_y_min
                    twinx_y_max_local = twinx_y_max
                else
                    has_twinx_local = .false.
                end if
                if (has_twinx_local .and. present(twinx_yscale)) twinx_scale_local = twinx_yscale
            end if
        end if

        if (present(has_twiny)) then
            has_twiny_local = has_twiny
            if (has_twiny_local) then
                if (present(twiny_x_min) .and. present(twiny_x_max)) then
                    twiny_x_min_local = twiny_x_min
                    twiny_x_max_local = twiny_x_max
                else
                    has_twiny_local = .false.
                end if
                if (has_twiny_local .and. present(twiny_xscale)) twiny_scale_local = twiny_xscale
            end if
        end if
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
                                             plots, plot_count, has_twinx, twinx_y_min, twinx_y_max, &
                                             twinx_ylabel, twinx_yscale, has_twiny, twiny_x_min, twiny_x_max, &
                                             twiny_xlabel, twiny_xscale)
        !! Render ONLY axis labels (for raster backends after plots are drawn)
        use fortplot_raster, only: raster_context
        class(plot_context), intent(inout) :: backend
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in) :: title, xlabel, ylabel
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in), optional :: has_twinx, has_twiny
        real(wp), intent(in), optional :: twinx_y_min, twinx_y_max
        real(wp), intent(in), optional :: twiny_x_min, twiny_x_max
        character(len=:), allocatable, intent(in), optional :: twinx_ylabel, twiny_xlabel
        character(len=*), intent(in), optional :: twinx_yscale, twiny_xscale
        logical :: has_3d
        real(wp) :: zmin_dummy, zmax_dummy
        logical :: has_twinx_local, has_twiny_local
        real(wp) :: twinx_y_min_local, twinx_y_max_local
        real(wp) :: twiny_x_min_local, twiny_x_max_local
        character(len=16) :: twinx_scale_local, twiny_scale_local
        call detect_3d_extent(plots, plot_count, has_3d, zmin_dummy, zmax_dummy)

        has_twinx_local = .false.
        has_twiny_local = .false.
        twinx_scale_local = yscale
        twiny_scale_local = xscale

        if (present(has_twinx)) then
            has_twinx_local = has_twinx
            if (has_twinx_local) then
                if (present(twinx_y_min) .and. present(twinx_y_max)) then
                    twinx_y_min_local = twinx_y_min
                    twinx_y_max_local = twinx_y_max
                else
                    has_twinx_local = .false.
                end if
                if (has_twinx_local .and. present(twinx_yscale)) twinx_scale_local = twinx_yscale
            end if
        end if

        if (present(has_twiny)) then
            has_twiny_local = has_twiny
            if (has_twiny_local) then
                if (present(twiny_x_min) .and. present(twiny_x_max)) then
                    twiny_x_min_local = twiny_x_min
                    twiny_x_max_local = twiny_x_max
                else
                    has_twiny_local = .false.
                end if
                if (has_twiny_local .and. present(twiny_xscale)) twiny_scale_local = twiny_xscale
            end if
        end if
        
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
                if (has_twinx_local) then
                    if (present(twinx_ylabel)) then
                        call raster_draw_secondary_y_axis(backend%raster, backend%width, backend%height, &
                                                          backend%plot_area, twinx_scale_local, symlog_threshold, &
                                                          twinx_y_min_local, twinx_y_max_local, ylabel=twinx_ylabel)
                    else
                        call raster_draw_secondary_y_axis(backend%raster, backend%width, backend%height, &
                                                          backend%plot_area, twinx_scale_local, symlog_threshold, &
                                                          twinx_y_min_local, twinx_y_max_local)
                    end if
                end if
                if (has_twiny_local) then
                    if (present(twiny_xlabel)) then
                        call raster_draw_secondary_x_axis_top(backend%raster, backend%width, backend%height, &
                                                             backend%plot_area, twiny_scale_local, symlog_threshold, &
                                                             twiny_x_min_local, twiny_x_max_local, xlabel=twiny_xlabel)
                    else
                        call raster_draw_secondary_x_axis_top(backend%raster, backend%width, backend%height, &
                                                             backend%plot_area, twiny_scale_local, symlog_threshold, &
                                                             twiny_x_min_local, twiny_x_max_local)
                    end if
                end if
            end if
        end select
    end subroutine render_figure_axes_labels_only

    subroutine render_title_only(backend, title, x_min, x_max, y_min, y_max)
        !! Render only the figure title without drawing axes
        use fortplot_raster, only: raster_context
        use fortplot_raster_labels, only: render_title_centered
        use fortplot_pdf, only: pdf_context
        use fortplot_pdf_core, only: PDF_TITLE_SIZE
        use fortplot_pdf_text, only: estimate_pdf_text_width
        use fortplot_ascii, only: ascii_context
        class(plot_context), intent(inout) :: backend
        character(len=:), allocatable, intent(in) :: title
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp) :: y_span, y_pos, x_pos

        if (.not. allocated(title)) return
        if (len_trim(title) == 0) return

        select type (backend)
        class is (raster_context)
            call render_title_centered(backend%raster, backend%width, backend%height, &
                                       backend%plot_area, trim(title))
            return
        class is (pdf_context)
            block
                real(wp) :: area_width
                real(wp) :: area_height
                real(wp) :: title_width_pdf
                real(wp) :: x_range
                real(wp) :: y_range
                real(wp) :: x_fraction
                real(wp) :: x_start
                real(wp) :: y_offset

                area_width = max(1.0e-9_wp, real(backend%plot_area%width, wp))
                area_height = max(1.0e-9_wp, real(backend%plot_area%height, wp))
                x_range = max(1.0e-9_wp, x_max - x_min)
                y_range = max(1.0e-9_wp, y_max - y_min)

                title_width_pdf = estimate_pdf_text_width(trim(title), PDF_TITLE_SIZE)
                x_fraction = min(1.0_wp, title_width_pdf / area_width)
                x_start = x_min + 0.5_wp * x_range * (1.0_wp - x_fraction)

                y_offset = (20.0_wp / area_height) * y_range
                call backend%color(0.0_wp, 0.0_wp, 0.0_wp)
                call backend%text(x_start, y_max + y_offset, trim(title))
            end block
            return
        class is (ascii_context)
            call backend%set_title(trim(title))
            return
        class default
            call backend%color(0.0_wp, 0.0_wp, 0.0_wp)
        end select

        y_span = max(1.0e-6_wp, y_max - y_min)
        x_pos = 0.5_wp * (x_min + x_max)
        y_pos = y_max + 0.08_wp * y_span
        call backend%text(x_pos, y_pos, trim(title))
    end subroutine render_title_only

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
                               margin_bottom, margin_top, state)
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
        type(figure_state_t), intent(in), optional :: state

        integer :: i
        logical :: has_state, restore_needed
        real(wp) :: x_min_curr, x_max_curr, y_min_curr, y_max_curr
        character(len=10) :: xscale_curr, yscale_curr
        real(wp) :: primary_x_min, primary_x_max, primary_y_min, primary_y_max
        real(wp) :: default_line_width

        has_state = present(state)
        if (has_state) then
            primary_x_min = state%x_min_transformed
            primary_x_max = state%x_max_transformed
            primary_y_min = state%y_min_transformed
            primary_y_max = state%y_max_transformed
            default_line_width = state%current_line_width
        else
            default_line_width = 1.0_wp
        end if

        do i = 1, plot_count
            if (has_state) then
                select case (plots(i)%axis)
                case (AXIS_TWINX)
                    x_min_curr = state%x_min_transformed
                    x_max_curr = state%x_max_transformed
                    y_min_curr = state%twinx_y_min_transformed
                    y_max_curr = state%twinx_y_max_transformed
                    xscale_curr = state%xscale
                    yscale_curr = state%twinx_yscale
                    restore_needed = .true.
                case (AXIS_TWINY)
                    x_min_curr = state%twiny_x_min_transformed
                    x_max_curr = state%twiny_x_max_transformed
                    y_min_curr = state%y_min_transformed
                    y_max_curr = state%y_max_transformed
                    xscale_curr = state%twiny_xscale
                    yscale_curr = state%yscale
                    restore_needed = .true.
                case default
                    x_min_curr = primary_x_min
                    x_max_curr = primary_x_max
                    y_min_curr = primary_y_min
                    y_max_curr = primary_y_max
                    xscale_curr = state%xscale
                    yscale_curr = state%yscale
                    restore_needed = .false.
                end select
                if (restore_needed) then
                    call backend%set_coordinates(x_min_curr, x_max_curr, y_min_curr, y_max_curr)
                end if
            else
                x_min_curr = x_min_transformed
                x_max_curr = x_max_transformed
                y_min_curr = y_min_transformed
                y_max_curr = y_max_transformed
                xscale_curr = xscale
                yscale_curr = yscale
                restore_needed = .false.
            end if

            select case (plots(i)%plot_type)
            case (PLOT_TYPE_LINE)
                call render_line_plot(backend, plots(i), &
                                    xscale_curr, yscale_curr, symlog_threshold)
                
                if (allocated(plots(i)%marker)) then
                    call render_markers(backend, plots(i), &
                                      x_min_curr, x_max_curr, &
                                      y_min_curr, y_max_curr, &
                                      xscale_curr, yscale_curr, symlog_threshold)
                end if
                
            case (PLOT_TYPE_SCATTER)
                ! Scatter plots render only markers (no connecting line)
                if (allocated(plots(i)%marker)) then
                    call render_markers(backend, plots(i), &
                                      x_min_curr, x_max_curr, &
                                      y_min_curr, y_max_curr, &
                                      xscale_curr, yscale_curr, symlog_threshold)
                end if

            case (PLOT_TYPE_CONTOUR)
                call render_contour_plot(backend, plots(i), &
                                       x_min_curr, x_max_curr, &
                                       y_min_curr, y_max_curr, &
                                       xscale_curr, yscale_curr, symlog_threshold, &
                                       width, height, &
                                       margin_left, margin_right, &
                                       margin_bottom, margin_top)

            case (PLOT_TYPE_PCOLORMESH)
                call render_pcolormesh_plot(backend, plots(i), &
                                          x_min_curr, x_max_curr, &
                                          y_min_curr, y_max_curr, &
                                          xscale_curr, yscale_curr, symlog_threshold, &
                                          width, height, margin_right)

            case (PLOT_TYPE_SURFACE)
                call render_surface_plot(backend, plots(i), &
                                         x_min_curr, x_max_curr, &
                                         y_min_curr, y_max_curr, &
                                         xscale_curr, yscale_curr, symlog_threshold)

            case (PLOT_TYPE_FILL)
                call render_fill_between_plot(backend, plots(i), xscale_curr, yscale_curr, &
                                              symlog_threshold)

            case (PLOT_TYPE_BAR)
                call render_bar_plot(backend, plots(i), xscale_curr, yscale_curr, &
                                     symlog_threshold)

            case (PLOT_TYPE_PIE)
                call render_pie_plot(backend, plots(i), xscale_curr, yscale_curr, symlog_threshold)

            case (PLOT_TYPE_BOXPLOT)
                call render_boxplot_plot(backend, plots(i), xscale_curr, yscale_curr, &
                                        symlog_threshold)

            case (PLOT_TYPE_ERRORBAR)
                call render_errorbar_plot(backend, plots(i), xscale_curr, yscale_curr, &
                                          symlog_threshold, default_line_width)
                ! Always attempt to render markers for errorbar plots;
                ! render_markers internally validates presence/emptiness.
                call render_markers(backend, plots(i), &
                                   x_min_curr, x_max_curr, &
                                   y_min_curr, y_max_curr, &
                                   xscale_curr, yscale_curr, symlog_threshold)

            case (PLOT_TYPE_REFLINE)
                call render_refline_plot(backend, plots(i), &
                                        x_min_curr, x_max_curr, &
                                        y_min_curr, y_max_curr, &
                                        xscale_curr, yscale_curr, symlog_threshold)

            end select

            if (has_state .and. restore_needed) then
                call backend%set_coordinates(primary_x_min, primary_x_max, primary_y_min, primary_y_max)
            end if
        end do

        if (has_state) then
            call backend%set_coordinates(primary_x_min, primary_x_max, primary_y_min, primary_y_max)
        end if
    end subroutine render_all_plots

    subroutine render_refline_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                   xscale, yscale, symlog_threshold)
        !! Render a reference line (horizontal or vertical)
        !! Reference lines store normalized coordinates for axis-spanning lines
        !! or actual data coordinates for hlines/vlines
        use fortplot_scales, only: apply_scale_transform
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        real(wp) :: x1, y1, x2, y2
        real(wp) :: x1_scaled, y1_scaled, x2_scaled, y2_scaled
        logical :: is_normalized_x, is_normalized_y

        if (.not. allocated(plot%x) .or. .not. allocated(plot%y)) return
        if (size(plot%x) < 2 .or. size(plot%y) < 2) return

        call backend%color(plot%color(1), plot%color(2), plot%color(3))

        if (allocated(plot%linestyle) .and. len_trim(plot%linestyle) > 0) then
            call backend%set_line_style(trim(plot%linestyle))
        else
            call backend%set_line_style('-')
        end if

        x1 = plot%x(1)
        x2 = plot%x(2)
        y1 = plot%y(1)
        y2 = plot%y(2)

        ! Detect if coordinates are normalized (0-1 range for axis-spanning)
        ! axhline/axvline store xmin/xmax or ymin/ymax as 0-1 normalized values
        is_normalized_x = (x1 >= 0.0_wp .and. x1 <= 1.0_wp .and. &
                          x2 >= 0.0_wp .and. x2 <= 1.0_wp .and. &
                          abs(x1 - x2) < 1.0e-9_wp .or. &
                          abs(y1 - y2) < 1.0e-9_wp)
        is_normalized_y = (y1 >= 0.0_wp .and. y1 <= 1.0_wp .and. &
                          y2 >= 0.0_wp .and. y2 <= 1.0_wp .and. &
                          abs(y1 - y2) < 1.0e-9_wp .or. &
                          abs(x1 - x2) < 1.0e-9_wp)

        ! For horizontal lines (y1 == y2), x values may be normalized
        if (abs(y1 - y2) < 1.0e-9_wp) then
            ! Horizontal line: check if x values are normalized (0-1)
            if (x1 >= 0.0_wp .and. x1 <= 1.0_wp .and. &
                x2 >= 0.0_wp .and. x2 <= 1.0_wp .and. &
                (x1 < 0.01_wp .or. x2 > 0.99_wp)) then
                ! Convert normalized x to data coordinates
                x1 = x_min + x1 * (x_max - x_min)
                x2 = x_min + x2 * (x_max - x_min)
            end if
        end if

        ! For vertical lines (x1 == x2), y values may be normalized
        if (abs(x1 - x2) < 1.0e-9_wp) then
            ! Vertical line: check if y values are normalized (0-1)
            if (y1 >= 0.0_wp .and. y1 <= 1.0_wp .and. &
                y2 >= 0.0_wp .and. y2 <= 1.0_wp .and. &
                (y1 < 0.01_wp .or. y2 > 0.99_wp)) then
                ! Convert normalized y to data coordinates
                y1 = y_min + y1 * (y_max - y_min)
                y2 = y_min + y2 * (y_max - y_min)
            end if
        end if

        ! Apply scale transformations
        x1_scaled = apply_scale_transform(x1, xscale, symlog_threshold)
        x2_scaled = apply_scale_transform(x2, xscale, symlog_threshold)
        y1_scaled = apply_scale_transform(y1, yscale, symlog_threshold)
        y2_scaled = apply_scale_transform(y2, yscale, symlog_threshold)

        ! Draw the line
        call backend%line(x1_scaled, y1_scaled, x2_scaled, y2_scaled)
    end subroutine render_refline_plot

    subroutine render_streamplot_arrows(backend, arrows)
        !! Render queued streamplot arrows after plot lines are drawn
        class(plot_context), intent(inout) :: backend
        type(arrow_data_t), intent(in) :: arrows(:)
        integer :: i

        if (size(arrows) <= 0) return

        do i = 1, size(arrows)
            call backend%draw_arrow(arrows(i)%x, arrows(i)%y, arrows(i)%dx, arrows(i)%dy, &
                                    arrows(i)%size, arrows(i)%style)
        end do
    end subroutine render_streamplot_arrows

end module fortplot_figure_rendering_pipeline
