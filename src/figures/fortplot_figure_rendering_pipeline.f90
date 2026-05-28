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
                                  PLOT_TYPE_QUIVER, PLOT_TYPE_POLAR, &
                                  AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_raster, only: raster_context
    use fortplot_raster_axes, only: raster_draw_x_minor_ticks, &
                                    raster_draw_y_minor_ticks
    use fortplot_tick_calculation, only: calculate_minor_tick_positions, &
                                         calculate_log_minor_tick_positions
    use fortplot_axes, only: compute_scale_ticks, MAX_TICKS
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    use fortplot_rendering, only: render_line_plot, render_contour_plot, &
                                  render_pcolormesh_plot, render_fill_between_plot, &
                                  render_markers, render_boxplot_plot, &
                                  render_errorbar_plot, &
                                  render_pie_plot, render_bar_plot
    use fortplot_legend, only: legend_t
    use fortplot_surface_rendering, only: render_surface_plot
    use fortplot_polar_rendering, only: render_polar_data, render_polar_boundary, &
                                        render_polar_radial_gridlines, &
                                        render_polar_angular_gridlines, &
                                        render_polar_angular_ticks
    use fortplot_twin_axes_rendering, only: setup_twin_axes_state, &
                                            render_twin_labels
    ! Plot dispatch and renderers extracted to submodules
    use fortplot_figure_plot_dispatch, only: render_all_plots
    use fortplot_figure_plot_renderers, only: render_streamplot_arrows, &
                                              render_polar_axes
    implicit none

    private
    public :: calculate_figure_data_ranges, setup_coordinate_system
    public :: render_figure_background, render_figure_axes, render_all_plots
    public :: render_streamplot_arrows
    public :: render_figure_axes_labels_only, render_title_only
    public :: render_polar_axes

    real(wp), parameter :: DATA_RANGE_MARGIN = 0.05_wp

contains

    subroutine setup_coordinate_system(backend, x_min_transformed, x_max_transformed, &
                                       y_min_transformed, y_max_transformed, &
                                       sticky_x_min, sticky_x_max, &
                                       sticky_y_min, sticky_y_max)
        !! Setup the coordinate system for rendering
        !! Adds a small margin to data ranges to prevent boundary data from
        !! being clipped by the plot frame stroke. Sticky sides (bar baselines)
        !! are left unexpanded so bars sit flush on the axis like matplotlib.
        use fortplot_pdf, only: pdf_context
        use fortplot_raster, only: raster_context
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x_min_transformed, x_max_transformed
        real(wp), intent(in) :: y_min_transformed, y_max_transformed
        logical, intent(in), optional :: sticky_x_min, sticky_x_max
        logical, intent(in), optional :: sticky_y_min, sticky_y_max

        real(wp) :: x_min_adj, x_max_adj, y_min_adj, y_max_adj

        ! Apply small margin to prevent boundary clipping
        select type (bk => backend)
        class is (pdf_context)
            call expand_data_range(x_min_transformed, x_max_transformed, &
                                   x_min_adj, x_max_adj, sticky_x_min, sticky_x_max)
            call expand_data_range(y_min_transformed, y_max_transformed, &
                                   y_min_adj, y_max_adj, sticky_y_min, sticky_y_max)
            call bk%set_coordinates(x_min_adj, x_max_adj, y_min_adj, y_max_adj)
        class is (raster_context)
            call expand_data_range(x_min_transformed, x_max_transformed, &
                                   x_min_adj, x_max_adj, sticky_x_min, sticky_x_max)
            call expand_data_range(y_min_transformed, y_max_transformed, &
                                   y_min_adj, y_max_adj, sticky_y_min, sticky_y_max)
            call bk%set_coordinates(x_min_adj, x_max_adj, y_min_adj, y_max_adj)
        class default
            call backend%set_coordinates(x_min_transformed, x_max_transformed, &
                                         y_min_transformed, y_max_transformed)
        end select
    end subroutine setup_coordinate_system

    subroutine expand_data_range(data_min, data_max, expanded_min, expanded_max, &
                                 sticky_min, sticky_max)
        !! Expand a data range by DATA_RANGE_MARGIN (5%) of the span on each
        !! side, matching matplotlib's default axes margin (rcParams
        !! axes.{x,y}margin = 0.05). Also keeps markers at exact boundaries
        !! clear of the plot frame stroke.
        !!
        !! A side flagged sticky is left unexpanded, matching matplotlib's
        !! sticky edges: bar baselines pin to 0 with no margin beyond them.
        real(wp), intent(in) :: data_min, data_max
        real(wp), intent(out) :: expanded_min, expanded_max
        logical, intent(in), optional :: sticky_min, sticky_max
        real(wp) :: span
        logical :: pin_min, pin_max

        pin_min = .false.
        pin_max = .false.
        if (present(sticky_min)) pin_min = sticky_min
        if (present(sticky_max)) pin_max = sticky_max

        if (data_max <= data_min) then
            expanded_min = data_min
            expanded_max = data_max
            return
        end if

        span = data_max - data_min
        if (pin_min) then
            expanded_min = data_min
        else
            expanded_min = data_min - DATA_RANGE_MARGIN*span
        end if
        if (pin_max) then
            expanded_max = data_max
        else
            expanded_max = data_max + DATA_RANGE_MARGIN*span
        end if
    end subroutine expand_data_range

    subroutine render_figure_background(backend)
        !! Render figure background
        class(plot_context), intent(inout) :: backend
        ! Background clearing is handled by backend-specific rendering
    end subroutine render_figure_background

 subroutine render_figure_axes(backend, xscale, yscale, symlog_threshold, &
                                x_min, x_max, y_min, y_max, title, xlabel, ylabel, &
                                plots, plot_count, has_twinx, twinx_y_min, &
                                twinx_y_max, &
                                twinx_ylabel, twinx_yscale, has_twiny, twiny_x_min, &
                                twiny_x_max, &
                                twiny_xlabel, twiny_xscale, state)
        !! Render figure axes and labels
        !! For raster backends, split rendering to prevent label overlap issues
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
        character(len=:), allocatable, intent(in), optional :: twinx_ylabel, &
                                                               twiny_xlabel
        character(len=*), intent(in), optional :: twinx_yscale, twiny_xscale
        type(figure_state_t), intent(in), optional :: state

        logical :: has_3d
        real(wp) :: zmin, zmax

        call detect_3d_extent(plots, plot_count, has_3d, zmin, zmax)
        call resolve_twin_axes_params(has_twinx, twinx_y_min, twinx_y_max, twinx_yscale, &
                                      has_twiny, twiny_x_min, twiny_x_max, twiny_xscale, &
                                      state, xscale, yscale)

        call dispatch_backend_axes_rendering(backend, xscale, yscale, symlog_threshold, &
                                             x_min, x_max, y_min, y_max, title, xlabel, ylabel, &
                                             zmin, zmax, has_3d, state)

        ! For raster backends without 3D, draw minor ticks after axes lines
        select type (backend)
        class is (raster_context)
            if (.not. has_3d) then
                if (present(state)) then
                    call render_minor_ticks_raster(backend, xscale, yscale, &
                                                   symlog_threshold, &
                                                   x_min, x_max, y_min, y_max, &
                                                   state)
                end if
            end if
        end select
    end subroutine render_figure_axes

    subroutine resolve_twin_axes_params(has_twinx, twinx_y_min, twinx_y_max, twinx_yscale, &
                                        has_twiny, twiny_x_min, twiny_x_max, twiny_xscale, &
                                        state, default_xscale, default_yscale)
        !! Resolve optional twin axes parameters (no-op stub for future use)
        logical, intent(in), optional :: has_twinx, has_twiny
        real(wp), intent(in), optional :: twinx_y_min, twinx_y_max
        real(wp), intent(in), optional :: twiny_x_min, twiny_x_max
        character(len=*), intent(in), optional :: twinx_yscale, twiny_xscale
        type(figure_state_t), intent(in), optional :: state
        character(len=*), intent(in) :: default_xscale, default_yscale
    end subroutine resolve_twin_axes_params

    subroutine dispatch_backend_axes_rendering(backend, xscale, yscale, symlog_threshold, &
                                                x_min, x_max, y_min, y_max, title, xlabel, ylabel, &
                                                zmin, zmax, has_3d, state)
        !! Dispatch axes rendering to backend-specific implementation
        class(plot_context), intent(inout) :: backend
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in) :: title, xlabel, ylabel
        real(wp), intent(in) :: zmin, zmax
        logical, intent(in) :: has_3d
        type(figure_state_t), intent(in), optional :: state

        character(len=64) :: xfmt, yfmt
        character(len=:), allocatable :: t_title, t_xlabel, t_ylabel

        xfmt = ''
        yfmt = ''
        if (present(state)) then
            if (allocated(state%xaxis_date_format)) xfmt = state%xaxis_date_format
            if (allocated(state%yaxis_date_format)) yfmt = state%yaxis_date_format
        end if

        ! Workaround for gfortran bug: unallocated allocatable characters passed
        ! to optional arguments cause segfaults. Allocate temporaries with empty
        ! strings when the originals are unallocated.
        t_title = ''
        t_xlabel = ''
        t_ylabel = ''
        if (allocated(title)) t_title = title
        if (allocated(xlabel)) t_xlabel = xlabel
        if (allocated(ylabel)) t_ylabel = ylabel

        select type (backend)
        class is (raster_context)
            if (has_3d) then
                call backend%draw_axes_and_labels_backend( &
                    xscale, yscale, symlog_threshold, x_min, x_max, y_min, y_max, &
                    t_title, t_xlabel, t_ylabel, x_date_format=trim(xfmt), &
                    y_date_format=trim(yfmt), z_min=zmin, z_max=zmax, &
                    has_3d_plots=.true.)
            else
                call backend%draw_axes_lines_and_ticks(xscale, yscale, &
                                                       symlog_threshold, &
                                                       x_min, x_max, &
                                                       y_min, y_max)
            end if
        class default
            call backend%draw_axes_and_labels_backend( &
                xscale, yscale, symlog_threshold, x_min, x_max, y_min, y_max, &
                t_title, t_xlabel, t_ylabel, x_date_format=xfmt, y_date_format=yfmt, &
                z_min=zmin, z_max=zmax, has_3d_plots=has_3d)
        end select
    end subroutine dispatch_backend_axes_rendering

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

    subroutine render_figure_axes_labels_only(backend, xscale, yscale, &
                                              symlog_threshold, &
                                              x_min, x_max, y_min, y_max, &
                                              title, xlabel, ylabel, &
                                              plots, plot_count, has_twinx, &
                                              twinx_y_min, twinx_y_max, &
                                              twinx_ylabel, twinx_yscale, has_twiny, &
                                              twiny_x_min, twiny_x_max, &
                                              twiny_xlabel, twiny_xscale, &
                                              custom_xticks, custom_xtick_labels, &
                                              custom_yticks, custom_ytick_labels, &
                                              x_date_format, y_date_format, &
                                              twinx_y_date_format, twiny_x_date_format)
        !! Render ONLY axis labels (for raster and PDF backends after plots are drawn)
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
        character(len=:), allocatable, intent(in), optional :: twinx_ylabel, &
                                                               twiny_xlabel
        character(len=*), intent(in), optional :: twinx_yscale, twiny_xscale
        real(wp), intent(in), optional :: custom_xticks(:), custom_yticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)
        character(len=*), intent(in), optional :: custom_ytick_labels(:)
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        character(len=*), intent(in), optional :: twinx_y_date_format, &
                                                  twiny_x_date_format
        logical :: has_3d
        real(wp) :: zmin_dummy, zmax_dummy
        logical :: has_twinx_local, has_twiny_local
        real(wp) :: twinx_y_min_local, twinx_y_max_local
        real(wp) :: twiny_x_min_local, twiny_x_max_local
        character(len=16) :: twinx_scale_local, twiny_scale_local

        call detect_3d_extent(plots, plot_count, has_3d, zmin_dummy, zmax_dummy)
        if (has_3d) return

        call setup_twin_axes_state(has_twinx, has_twiny, twinx_y_min, twinx_y_max, &
                                   twiny_x_min, twiny_x_max, twinx_yscale, &
                                   twiny_xscale, twinx_ylabel, twiny_xlabel, &
                                   xscale, yscale, has_twinx_local, has_twiny_local, &
                                   twinx_y_min_local, twinx_y_max_local, &
                                   twiny_x_min_local, twiny_x_max_local, &
                                   twinx_scale_local, twiny_scale_local)

        select type (backend)
        class is (raster_context)
            ! The title is drawn here, before the secondary top axis ticks set
            ! raster%last_x_tick_max_height_top. Seed an estimate so the title's
            ! twiny branch lifts it above the top-axis block; the exact value is
            ! filled in later for the top xlabel placement.
            if (has_twiny_local) call seed_top_tick_height(backend)
            call backend%draw_axis_labels_only(xscale, yscale, symlog_threshold, &
                                               x_min, x_max, y_min, y_max, title, &
                                               xlabel, ylabel, custom_xticks, &
                                               custom_xtick_labels, custom_yticks, &
                                               custom_ytick_labels, &
                                               x_date_format=x_date_format, &
                                               y_date_format=y_date_format)
        class default
        end select

        call render_twin_labels(backend, xscale, yscale, symlog_threshold, &
                                x_min, x_max, y_min, y_max, title, xlabel, &
                                ylabel, custom_xticks, custom_xtick_labels, &
                                custom_yticks, custom_ytick_labels, &
                                has_twinx_local, has_twiny_local, &
                                twinx_scale_local, twiny_scale_local, &
                                twinx_y_min_local, twinx_y_max_local, &
                                twiny_x_min_local, twiny_x_max_local, &
                                x_date_format, y_date_format, &
                                twinx_y_date_format, twiny_x_date_format, &
                                twinx_ylabel, twiny_xlabel, draw_primary_labels=.false.)
    end subroutine render_figure_axes_labels_only

    subroutine seed_top_tick_height(backend)
        !! Pre-seed the top tick-label height used to position the title above a
        !! twiny block. Matches raster_draw_x_axis_ticks_top, which measures tick
        !! labels with calculate_text_height; '0' is a representative digit.
        use fortplot_text, only: calculate_text_height
        type(raster_context), intent(inout) :: backend
        integer :: h
        h = calculate_text_height('0')
        if (h <= 0) h = 1
        backend%raster%last_x_tick_max_height_top = &
            max(backend%raster%last_x_tick_max_height_top, h)
    end subroutine seed_top_tick_height

    subroutine render_title_only(backend, title, x_min, x_max, y_min, y_max, &
                                 custom_title_font_size)
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
        real(wp), intent(in), optional :: custom_title_font_size
        real(wp) :: y_span, y_pos, x_pos

        if (.not. allocated(title)) return
        if (len_trim(title) == 0) return

        select type (backend)
        class is (raster_context)
            call render_title_centered(backend%raster, backend%width, &
                                       backend%height, backend%plot_area, trim(title), &
                                       custom_title_font_size)
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
                x_fraction = min(1.0_wp, title_width_pdf/area_width)
                x_start = x_min + 0.5_wp*x_range*(1.0_wp - x_fraction)

                y_offset = (20.0_wp/area_height)*y_range
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
        x_pos = 0.5_wp*(x_min + x_max)
        y_pos = y_max + 0.08_wp*y_span
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

end module fortplot_figure_rendering_pipeline
