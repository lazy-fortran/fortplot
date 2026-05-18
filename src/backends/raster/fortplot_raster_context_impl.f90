submodule (fortplot_raster) fortplot_raster_context_impl
    !! Context wrapper implementations for raster_context.
    !! Contains: save_dummy, fill_quad, fill_heatmap, legend, rgb/png, 3d,
    !!           ylabel, axes, axes_lines, axis_labels, coordinates, render_axes.

    use fortplot_raster_rendering, only: raster_fill_quad, fill_triangle
    use fortplot_3d_axes, only: draw_3d_axes
    use fortplot_plot_data, only: plot_data_t

contains

    !! ── Context wrapper methods (delegate to specialized modules) ──────

    module subroutine raster_save_dummy(this, filename)
        !! Dummy save method - see issue #496 for implementation improvement roadmap
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        associate (dfl => len_trim(filename)); end associate

        ! This is a dummy implementation - concrete backends like PNG will override this
        ! Implementation improvement needed - see issue #496
        call log_error("raster_context cannot save files directly. Use a PNG backend.")
        ! Return instead of stopping - this allows graceful error handling
        return
    end subroutine raster_save_dummy

    module subroutine raster_fill_quad_context(this, x_quad, y_quad)
        !! Fill quadrilateral with current color - delegate to specialized module
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)

        call raster_fill_quad(this%raster, this%width, this%height, this%plot_area, &
                              this%x_min, this%x_max, this%y_min, this%y_max, &
                              x_quad, y_quad)
    end subroutine raster_fill_quad_context

    module subroutine raster_fill_heatmap_context(this, x_grid, y_grid, z_grid, z_min, z_max, colormap_name)
        !! Fill contour plot - delegate to specialized rendering module
        class(raster_context), intent(inout) :: this
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in) :: z_min, z_max
        character(len=*), intent(in), optional :: colormap_name

        call raster_fill_heatmap(this%raster, this%width, this%height, this%plot_area, &
                                 this%x_min, this%x_max, this%y_min, this%y_max, &
                                 x_grid, y_grid, z_grid, z_min, z_max, colormap_name)
    end subroutine raster_fill_heatmap_context

    module subroutine raster_render_legend_specialized_context(this, legend, &
                                                        legend_x, legend_y)
        class(raster_context), intent(inout) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(in) :: legend_x, legend_y

        call raster_render_legend_specialized(legend, legend_x, legend_y)
    end subroutine raster_render_legend_specialized_context

    module subroutine raster_calculate_legend_dimensions_context(this, legend, legend_width, &
                                                          legend_height)
        class(raster_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: legend_width, legend_height

        call raster_calculate_legend_dimensions(legend, legend_width, legend_height, &
                                                  this%raster%dpi)
    end subroutine raster_calculate_legend_dimensions_context

    module subroutine raster_set_legend_border_width_context(this)
        class(raster_context), intent(inout) :: this

        call this%set_line_width(0.1_wp)  ! Thin border for PNG like axes
    end subroutine raster_set_legend_border_width_context

    module subroutine raster_calculate_legend_position_context(this, legend, x, y)
        class(raster_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: x, y

        call raster_calculate_legend_position(legend, x, y)
    end subroutine raster_calculate_legend_position_context

    module subroutine raster_extract_rgb_data_context(this, width, height, rgb_data)
        class(raster_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(wp), intent(out) :: rgb_data(width, height, 3)

        call raster_extract_rgb_data(this%raster, width, height, rgb_data)
    end subroutine raster_extract_rgb_data_context

    module subroutine raster_get_png_data_context(this, width, height, png_data, status)
        class(raster_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status

        call raster_get_png_data(width, height, png_data, status)
    end subroutine raster_get_png_data_context

    module subroutine raster_prepare_3d_data_context(this, plots)
        class(raster_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)

        ! PNG backend does not use 3D data preparation; keep a benign reference
        ! to arguments to avoid unused warnings under stricter toolchains.
        if (this%width < 0) return
        if (size(plots) == 0) return
    end subroutine raster_prepare_3d_data_context

    module subroutine raster_render_ylabel_context(this, ylabel)
        !! Render rotated Y-axis label - delegate to axes module
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel

        call raster_render_ylabel(this%raster, this%width, this%height, &
                                  this%plot_area, ylabel)
    end subroutine raster_render_ylabel_context

    module subroutine raster_draw_axes_and_labels_context(this, xscale, yscale, &
                                                          symlog_threshold, &
                                                          x_min, x_max, y_min, y_max, &
                                                          title, xlabel, ylabel, &
                                                          x_date_format, y_date_format, &
                                                          z_min, z_max, has_3d_plots)
        !! Draw axes and labels - delegate to specialized axes module
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        character(len=:), allocatable :: title_str, xlabel_str, ylabel_str

        this%last_xscale = trim(xscale)
        this%last_yscale = trim(yscale)
        this%last_symlog_threshold = symlog_threshold

        ! Set color to black for axes and text
        call this%color(0.0_wp, 0.0_wp, 0.0_wp)

        if (has_3d_plots) then
            ! Draw simplified 3D axes frame using current data ranges
            call draw_3d_axes(this, x_min, x_max, y_min, y_max, &
                              merge(z_min, 0.0_wp, present(z_min)), &
                              merge(z_max, 1.0_wp, present(z_max)))
            ! Draw title/xlabel/ylabel without re-drawing 2D tick labels
            if (present(title) .or. present(xlabel) .or. present(ylabel)) then
                title_str = ""
                xlabel_str = ""
                ylabel_str = ""

                if (present(title)) then
                    if (allocated(title)) title_str = title
                end if
                if (present(xlabel)) then
                    if (allocated(xlabel)) xlabel_str = xlabel
                end if
                if (present(ylabel)) then
                    if (allocated(ylabel)) ylabel_str = ylabel
                end if

                call raster_draw_axis_labels(this%raster, this%width, this%height, &
                                             this%plot_area, &
                                             title_str, xlabel_str, ylabel_str)
            end if
        else
            ! Delegate to standard 2D axes module
            call raster_draw_axes_and_labels(this%raster, this%width, this%height, &
                                             this%plot_area, &
                                             xscale, yscale, symlog_threshold, &
                                             x_min, x_max, y_min, y_max, &
                                             title, xlabel, ylabel, &
                                             x_date_format=x_date_format, &
                                             y_date_format=y_date_format)
        end if
    end subroutine raster_draw_axes_and_labels_context

    module subroutine raster_draw_axes_lines_and_ticks_context(this, xscale, yscale, &
                                                                symlog_threshold, &
                                                                x_min, x_max, y_min, y_max)
        !! Draw axes lines and tick marks WITHOUT labels (for proper drawing order)
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        this%last_xscale = trim(xscale)
        this%last_yscale = trim(yscale)
        this%last_symlog_threshold = symlog_threshold

        ! Set color to black for axes and ticks
        call this%color(0.0_wp, 0.0_wp, 0.0_wp)

        ! Delegate to axes module
        call raster_draw_axes_lines_and_ticks(this%raster, this%width, this%height, &
                                              this%plot_area, &
                                              xscale, yscale, symlog_threshold, &
                                              x_min, x_max, y_min, y_max)
    end subroutine raster_draw_axes_lines_and_ticks_context

    module subroutine raster_draw_axis_labels_only_context(this, xscale, yscale, &
                                                           symlog_threshold, &
                                                           x_min, x_max, y_min, y_max, &
                                                           title, xlabel, ylabel, &
                                                           custom_xticks, &
                                                           custom_xtick_labels, &
                                                           custom_yticks, &
                                                           custom_ytick_labels, &
                                                           x_date_format, y_date_format)
        !! Draw ONLY axis labels and tick labels (for proper drawing order)
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in), optional :: custom_xticks(:), custom_yticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)
        character(len=*), intent(in), optional :: custom_ytick_labels(:)
        character(len=*), intent(in), optional :: x_date_format, y_date_format

        this%last_xscale = trim(xscale)
        this%last_yscale = trim(yscale)
        this%last_symlog_threshold = symlog_threshold

        ! Set color to black for text
        call this%color(0.0_wp, 0.0_wp, 0.0_wp)

        ! Delegate to axes module
        call raster_draw_axis_labels_only(this%raster, this%width, this%height, &
                                          this%plot_area, &
                                          xscale, yscale, symlog_threshold, &
                                          x_min, x_max, y_min, y_max, &
                                          title, xlabel, ylabel, &
                                          custom_xticks, custom_xtick_labels, &
                                          custom_yticks, custom_ytick_labels, &
                                          x_date_format=x_date_format, &
                                          y_date_format=y_date_format)
    end subroutine raster_draw_axis_labels_only_context

    !! ── Coordinate management ──────────────────────────────────────────

    module subroutine raster_save_coordinates(this, x_min, x_max, y_min, y_max)
        !! Save current coordinate system
        class(raster_context), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max

        x_min = this%x_min
        x_max = this%x_max
        y_min = this%y_min
        y_max = this%y_max
    end subroutine raster_save_coordinates

    module subroutine raster_set_coordinates(this, x_min, x_max, y_min, y_max)
        !! Set coordinate system
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        this%x_min = x_min
        this%x_max = x_max
        this%y_min = y_min
        this%y_max = y_max
    end subroutine raster_set_coordinates

    module subroutine raster_render_axes(this, title_text, xlabel_text, ylabel_text)
        !! Render axes for raster context using the most recently configured scales.
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text
        character(len=:), allocatable :: t, xl, yl
        real(wp) :: x_min, x_max, y_min, y_max

        t = ''
        xl = ''
        yl = ''
        if (present(title_text)) t = title_text
        if (present(xlabel_text)) xl = xlabel_text
        if (present(ylabel_text)) yl = ylabel_text

        call this%save_coordinates(x_min, x_max, y_min, y_max)
        call this%draw_axes_and_labels_backend(this%last_xscale, this%last_yscale, &
                                               this%last_symlog_threshold, &
                                               x_min, x_max, y_min, y_max, &
                                               t, xl, yl, has_3d_plots=.false.)
    end subroutine raster_render_axes

end submodule fortplot_raster_context_impl
