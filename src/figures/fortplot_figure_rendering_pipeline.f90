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
                                  PLOT_TYPE_QUIVER, &
                                  AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_raster_axes, only: raster_draw_secondary_y_axis, &
                                     raster_draw_secondary_x_axis_top, &
                                     raster_draw_x_minor_ticks, &
                                     raster_draw_y_minor_ticks
    use fortplot_tick_calculation, only: calculate_minor_tick_positions, &
                                         calculate_log_minor_tick_positions
    use fortplot_axes, only: compute_scale_ticks, MAX_TICKS
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    use fortplot_rendering, only: render_line_plot, render_contour_plot, &
                                 render_pcolormesh_plot, render_fill_between_plot, &
                                 render_markers, render_boxplot_plot, render_errorbar_plot, &
                                 render_pie_plot, render_bar_plot
    use fortplot_legend, only: legend_t
    use fortplot_surface_rendering, only: render_surface_plot
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
                                 twiny_xlabel, twiny_xscale, state)
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
        type(figure_state_t), intent(in), optional :: state
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
                ! Draw minor ticks if enabled
                if (present(state)) then
                    call render_minor_ticks_raster(backend, xscale, yscale, &
                                                   symlog_threshold, &
                                                   x_min, x_max, y_min, y_max, &
                                                   state)
                end if
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

    subroutine render_minor_ticks_raster(backend, xscale, yscale, symlog_threshold, &
                                          x_min, x_max, y_min, y_max, state)
        !! Render minor ticks for raster backends when enabled
        use fortplot_raster, only: raster_context
        type(raster_context), intent(inout) :: backend
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(figure_state_t), intent(in) :: state

        real(wp) :: major_x(MAX_TICKS), major_y(MAX_TICKS)
        real(wp) :: minor_x(MAX_TICKS*10), minor_y(MAX_TICKS*10)
        integer :: num_major_x, num_major_y, num_minor_x, num_minor_y

        if (.not. state%minor_ticks_x .and. .not. state%minor_ticks_y) return

        ! Get major tick positions
        if (state%minor_ticks_x) then
            call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, &
                                     major_x, num_major_x)
            if (num_major_x >= 2) then
                if (trim(xscale) == 'log') then
                    call calculate_log_minor_tick_positions(major_x, num_major_x, &
                                                            x_min, x_max, &
                                                            minor_x, num_minor_x)
                else
                    call calculate_minor_tick_positions(major_x, num_major_x, &
                                                        state%minor_tick_count, &
                                                        x_min, x_max, &
                                                        minor_x, num_minor_x)
                end if
                if (num_minor_x > 0) then
                    call raster_draw_x_minor_ticks(backend%raster, backend%width, &
                                                   backend%height, backend%plot_area, &
                                                   xscale, symlog_threshold, &
                                                   minor_x(1:num_minor_x), &
                                                   x_min, x_max)
                end if
            end if
        end if

        if (state%minor_ticks_y) then
            call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, &
                                     major_y, num_major_y)
            if (num_major_y >= 2) then
                if (trim(yscale) == 'log') then
                    call calculate_log_minor_tick_positions(major_y, num_major_y, &
                                                            y_min, y_max, &
                                                            minor_y, num_minor_y)
                else
                    call calculate_minor_tick_positions(major_y, num_major_y, &
                                                        state%minor_tick_count, &
                                                        y_min, y_max, &
                                                        minor_y, num_minor_y)
                end if
                if (num_minor_y > 0) then
                    call raster_draw_y_minor_ticks(backend%raster, backend%width, &
                                                   backend%height, backend%plot_area, &
                                                   yscale, symlog_threshold, &
                                                   minor_y(1:num_minor_y), &
                                                   y_min, y_max)
                end if
            end if
        end if
    end subroutine render_minor_ticks_raster

    subroutine render_figure_axes_labels_only(backend, xscale, yscale, symlog_threshold, &
                                             x_min, x_max, y_min, y_max, &
                                             title, xlabel, ylabel, &
                                             plots, plot_count, has_twinx, twinx_y_min, twinx_y_max, &
                                             twinx_ylabel, twinx_yscale, has_twiny, twiny_x_min, twiny_x_max, &
                                             twiny_xlabel, twiny_xscale, &
                                             custom_xticks, custom_xtick_labels, &
                                             custom_yticks, custom_ytick_labels)
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
        real(wp), intent(in), optional :: custom_xticks(:), custom_yticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)
        character(len=*), intent(in), optional :: custom_ytick_labels(:)
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
                                                  ylabel, &
                                                  custom_xticks, custom_xtick_labels, &
                                                  custom_yticks, custom_ytick_labels)
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

            case (PLOT_TYPE_QUIVER)
                call render_quiver_plot(backend, plots(i), &
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

    subroutine render_quiver_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                  xscale, yscale, symlog_threshold)
        !! Render quiver plot (discrete vector arrows)
        !! Draws arrows at each (x,y) position with direction (u,v)
        use fortplot_scales, only: apply_scale_transform
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer :: i, n
        real(wp) :: x_scaled, y_scaled, u_scaled, v_scaled
        real(wp) :: scale, arrow_size, mag, max_mag
        real(wp) :: x_range, y_range, data_scale

        if (.not. allocated(plot%x) .or. .not. allocated(plot%y)) return
        if (.not. allocated(plot%quiver_u) .or. .not. allocated(plot%quiver_v)) return

        n = size(plot%x)
        if (n == 0) return
        if (size(plot%y) /= n .or. size(plot%quiver_u) /= n .or. &
            size(plot%quiver_v) /= n) return

        call backend%color(plot%color(1), plot%color(2), plot%color(3))
        call backend%set_line_style('-')

        scale = plot%quiver_scale
        x_range = max(1.0e-9_wp, x_max - x_min)
        y_range = max(1.0e-9_wp, y_max - y_min)

        max_mag = 0.0_wp
        do i = 1, n
            mag = sqrt(plot%quiver_u(i)**2 + plot%quiver_v(i)**2)
            if (mag > max_mag) max_mag = mag
        end do
        if (max_mag < 1.0e-12_wp) max_mag = 1.0_wp

        data_scale = min(x_range, y_range) * 0.05_wp * scale / max_mag
        arrow_size = 1.0_wp

        do i = 1, n
            x_scaled = apply_scale_transform(plot%x(i), xscale, symlog_threshold)
            y_scaled = apply_scale_transform(plot%y(i), yscale, symlog_threshold)

            u_scaled = plot%quiver_u(i) * data_scale
            v_scaled = plot%quiver_v(i) * data_scale

            if (abs(u_scaled) < 1.0e-12_wp .and. abs(v_scaled) < 1.0e-12_wp) cycle

            call backend%draw_arrow(x_scaled, y_scaled, u_scaled, v_scaled, &
                                    arrow_size, '->')
        end do
    end subroutine render_quiver_plot

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
