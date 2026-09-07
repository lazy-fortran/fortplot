module fortplot_quiver_geometry
    !! Matplotlib quiver.py: _init, _make_verts and _h_arrows geometry.
    !! Arrow widths use display units; explicit scale is an inverse length.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortplot_plot_data, only: plot_data_t
    use fortplot_scales, only: apply_scale_transform, apply_inverse_scale_transform
    implicit none
    private
    public :: prepare_quiver_geometry, quiver_arrow_vertices

contains

    subroutine prepare_quiver_geometry(plot, bounds, canvas, dpi, xscale, yscale, &
            threshold, lengths, angles, width, data_extent)
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: bounds(4), canvas(2), dpi, threshold
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(out) :: lengths(:), angles(:), width
        real(wp), intent(in), optional :: data_extent
        real(wp) :: unit_pixels, length_pixels, span, scale, mean_length, view_bounds(4)
        integer :: i, count_valid

        do i = 1, 2
            view_bounds(i) = apply_inverse_scale_transform(bounds(i), xscale, threshold)
            view_bounds(i + 2) = apply_inverse_scale_transform(bounds(i + 2), &
                yscale, threshold)
        end do
        unit_pixels = quiver_unit_pixels(plot%quiver_units, canvas, view_bounds, dpi)
        span = canvas(1)/unit_pixels
        width = plot%quiver_width*unit_pixels
        if (width <= 0.0_wp) then
            width = min(25.0_wp, max(8.0_wp, sqrt(real(size(lengths), wp))))
            width = 0.06_wp*canvas(1)/width
        end if
        call measure_quiver_vectors(plot, bounds, canvas, xscale, yscale, &
            threshold, lengths, angles, data_extent)
        length_pixels = unit_pixels
        if (len_trim(plot%quiver_scale_units) > 0) then
            length_pixels = quiver_unit_pixels(plot%quiver_scale_units, &
                canvas, view_bounds, dpi)
            if (plot%quiver_scale_units == 'xy') length_pixels = 1.0_wp
        end if
        scale = plot%quiver_scale
        if (scale <= 0.0_wp) then
            mean_length = 0.0_wp
            count_valid = 0
            do i = 1, size(lengths)
                if (.not. ieee_is_finite(lengths(i))) cycle
                mean_length = mean_length + lengths(i)
                count_valid = count_valid + 1
            end do
            mean_length = mean_length/max(1, count_valid)
            scale = max(10.0_wp, sqrt(real(size(lengths), wp)))
            scale = 1.8_wp*mean_length*scale/span*length_pixels/unit_pixels
        end if
        if (scale <= tiny(1.0_wp)) scale = 1.0_wp
        lengths = lengths*length_pixels/scale
    end subroutine prepare_quiver_geometry

    subroutine measure_quiver_vectors(plot, bounds, canvas, xscale, yscale, &
            threshold, lengths, angles, data_extent)
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: bounds(4), canvas(2), threshold
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(out) :: lengths(:), angles(:)
        real(wp), intent(in), optional :: data_extent
        real(wp) :: dx, dy, eps, sx, sy
        integer :: i
        logical :: xy_angles, xy_lengths

        xy_angles = plot%quiver_angles == 'xy'
        xy_lengths = plot%quiver_scale_units == 'xy'
        eps = 1.0_wp
        if (xy_angles .neqv. xy_lengths) then
            eps = max(maxval(abs(plot%x)), maxval(abs(plot%y)))*0.001_wp
            if (present(data_extent)) eps = 0.001_wp*data_extent
            eps = max(eps, 1.0e-12_wp)
        end if
        sx = canvas(1)/(bounds(2) - bounds(1))
        sy = canvas(2)/(bounds(4) - bounds(3))
        do i = 1, size(lengths)
            dx = plot%quiver_u(i)
            dy = plot%quiver_v(i)
            lengths(i) = hypot(dx, dy)
            angles(i) = atan2(dy, dx)
            if (.not. xy_angles .and. .not. xy_lengths) cycle
            dx = (apply_scale_transform(plot%x(i) + eps*dx, xscale, threshold) - &
                apply_scale_transform(plot%x(i), xscale, threshold))*sx
            dy = (apply_scale_transform(plot%y(i) + eps*dy, yscale, threshold) - &
                apply_scale_transform(plot%y(i), yscale, threshold))*sy
            if (xy_angles) angles(i) = atan2(dy, dx)
            if (xy_lengths) lengths(i) = hypot(dx, dy)/eps
        end do
    end subroutine measure_quiver_vectors

    function quiver_unit_pixels(units, canvas, bounds, dpi) result(pixels)
        character(len=*), intent(in) :: units
        real(wp), intent(in) :: canvas(2), bounds(4), dpi
        real(wp) :: pixels

        select case (trim(units))
        case ('x')
            pixels = canvas(1)/(bounds(2) - bounds(1))
        case ('y')
            pixels = canvas(2)/(bounds(4) - bounds(3))
        case ('xy')
            pixels = hypot(canvas(1), canvas(2))/ &
                hypot(bounds(2) - bounds(1), bounds(4) - bounds(3))
        case ('height')
            pixels = canvas(2)
        case ('dots')
            pixels = 1.0_wp
        case ('inches')
            pixels = dpi
        case default
            pixels = canvas(1)
        end select
        pixels = max(pixels, tiny(1.0_wp))
    end function quiver_unit_pixels

    subroutine quiver_arrow_vertices(length, angle, width, headwidth, headlength, &
            pivot, x, y)
        real(wp), intent(in) :: length, angle, width, headwidth, headlength
        character(len=*), intent(in) :: pivot
        real(wp), intent(out) :: x(8), y(8)
        real(wp) :: extent, minshaft, shrink, local_x(8), local_y(8), theta
        integer :: i

        extent = min(65536.0_wp, max(0.0_wp, length/width))
        minshaft = max(0.0_wp, headlength)
        local_x = [0.0_wp, extent - 4.5_wp, extent - headlength, extent, &
            extent - headlength, extent - 4.5_wp, 0.0_wp, 0.0_wp]
        local_y = [0.5_wp, 0.5_wp, 0.5_wp*headwidth, 0.0_wp, &
            -0.5_wp*headwidth, -0.5_wp, -0.5_wp, 0.5_wp]
        if (extent < minshaft) then
            shrink = extent/minshaft
            local_x = [0.0_wp, minshaft - 4.5_wp, 0.0_wp, minshaft, &
                0.0_wp, minshaft - 4.5_wp, 0.0_wp, 0.0_wp]*shrink
            local_y = local_y*shrink
        end if
        select case (trim(pivot))
        case ('mid', 'middle')
            local_x = local_x - 0.5_wp*extent
        case ('tip')
            local_x = local_x - extent
        end select
        if (extent < 1.0_wp) then
            do i = 1, 8
                theta = real(i - 1, wp)*acos(-1.0_wp)/3.0_wp
                local_x(i) = 0.5_wp*cos(theta)
                local_y(i) = 0.5_wp*sin(theta)
            end do
        end if
        x = width*(local_x*cos(angle) - local_y*sin(angle))
        y = width*(local_x*sin(angle) + local_y*cos(angle))
    end subroutine quiver_arrow_vertices

end module fortplot_quiver_geometry
