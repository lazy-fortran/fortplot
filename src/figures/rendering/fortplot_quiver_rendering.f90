module fortplot_quiver_rendering
    !! Shared quiver geometry for PNG, PDF, SVG and terminal arrows.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortplot_context, only: plot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_raster, only: raster_context
    use fortplot_pdf, only: pdf_context
    use fortplot_ascii, only: ascii_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_colormap, only: colormap_value_to_color
    use fortplot_quiver_geometry, only: prepare_quiver_geometry, quiver_arrow_vertices
    use fortplot_quiver_polygons, only: fill_quiver_polygon
    implicit none
    private
    public :: render_quiver_plot

contains

    subroutine render_quiver_plot(backend, plot, x_min, x_max, y_min, y_max, &
            xscale, yscale, symlog_threshold, data_extent)
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, symlog_threshold
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in), optional :: data_extent
        real(wp), allocatable :: lengths(:), angles(:)
        real(wp) :: bounds(4), canvas(2), units(2), dpi, width, rgba(4)
        real(wp) :: x(8), y(8), origin(2)
        integer :: n, i

        if (.not. allocated(plot%x)) return
        if (.not. allocated(plot%y)) return
        if (.not. allocated(plot%quiver_u)) return
        if (.not. allocated(plot%quiver_v)) return
        n = size(plot%x)
        if (n == 0) return
        if (size(plot%y) /= n .or. size(plot%quiver_u) /= n .or. &
            size(plot%quiver_v) /= n) return
        bounds = [x_min, x_max, y_min, y_max]
        if (x_max <= x_min .or. y_max <= y_min) return
        call quiver_canvas(backend, bounds, canvas, dpi)
        units = canvas/[x_max - x_min, y_max - y_min]
        allocate (lengths(n), angles(n))
        call prepare_quiver_geometry(plot, bounds, canvas, dpi, xscale, yscale, &
            symlog_threshold, lengths, angles, width, data_extent)
        do i = 1, n
            if (.not. ieee_is_finite(lengths(i))) cycle
            if (.not. ieee_is_finite(angles(i))) cycle
            origin = [apply_scale_transform(plot%x(i), xscale, symlog_threshold), &
                apply_scale_transform(plot%y(i), yscale, symlog_threshold)]
            if (.not. all(ieee_is_finite(origin))) cycle
            call quiver_arrow_vertices(lengths(i), angles(i), width, &
                plot%quiver_headwidth, plot%quiver_headlength, &
                plot%quiver_pivot, x, y)
            x = origin(1) + x/units(1)
            y = origin(2) + y/units(2)
            call quiver_color(plot, i, n, rgba)
            select type (backend)
            class is (ascii_context)
                call render_text_quiver(backend, origin, lengths(i), angles(i), &
                    units, plot%quiver_pivot, rgba)
            class default
                call fill_quiver_polygon(backend, x, y, rgba, bounds)
            end select
        end do
    end subroutine render_quiver_plot

    subroutine quiver_canvas(backend, bounds, canvas, dpi)
        class(plot_context), intent(in) :: backend
        real(wp), intent(in) :: bounds(4)
        real(wp), intent(out) :: canvas(2), dpi

        canvas = [backend%get_width_scale()*(bounds(2) - bounds(1)), &
            backend%get_height_scale()*(bounds(4) - bounds(3))]
        dpi = 100.0_wp
        select type (backend)
        class is (raster_context)
            canvas = real([backend%plot_area%width, backend%plot_area%height], wp)
            dpi = backend%raster%dpi
        class is (pdf_context)
            canvas = real([backend%plot_area%width, backend%plot_area%height], wp)
            dpi = 72.0_wp
        class is (ascii_context)
            ! Terminal cells are approximately twice as tall as they are wide.
            canvas(2) = canvas(2)*2.0_wp
        end select
        canvas = max(canvas, 1.0_wp)
    end subroutine quiver_canvas

    subroutine quiver_color(plot, i, n, rgba)
        type(plot_data_t), intent(in) :: plot
        integer, intent(in) :: i, n
        real(wp), intent(out) :: rgba(4)
        character(len=:), allocatable :: cmap

        rgba = [plot%color, plot%marker_face_alpha]
        if (.not. allocated(plot%scatter_colors)) return
        if (size(plot%scatter_colors) /= n) return
        cmap = 'viridis'
        if (allocated(plot%quiver_colormap)) then
            if (len_trim(plot%quiver_colormap) > 0) cmap = plot%quiver_colormap
        end if
        call colormap_value_to_color(plot%scatter_colors(i), &
            minval(plot%scatter_colors), &
            maxval(plot%scatter_colors), &
            cmap, rgba(:3))
    end subroutine quiver_color

    subroutine render_text_quiver(backend, origin, length, angle, units, pivot, rgba)
        class(ascii_context), intent(inout) :: backend
        real(wp), intent(in) :: origin(2), length, angle, units(2), rgba(4)
        character(len=*), intent(in) :: pivot
        real(wp) :: vector(2), start(2), offset

        if (rgba(4) <= 0.0_wp) return
        vector = length*[cos(angle), sin(angle)]/units
        offset = 0.0_wp
        if (pivot == 'mid' .or. pivot == 'middle') offset = 0.5_wp
        if (pivot == 'tip') offset = 1.0_wp
        start = origin - offset*vector
        call backend%color(rgba(1), rgba(2), rgba(3))
        call backend%draw_quiver_arrow(start(1), start(2), vector(1), vector(2))
    end subroutine render_text_quiver

end module fortplot_quiver_rendering
