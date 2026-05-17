submodule (fortplot_raster) fortplot_raster_text_impl
    !! Implementation of text rendering methods for raster_context.
  !! Contains: text, text_styled, text_with_bbox, rotated_text_with_bbox.

contains

    !! ── Text rendering ─────────────────────────────────────────────────

    module subroutine raster_draw_text(this, x, y, text)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: px, py
        integer(1) :: r, g, b
        character(len=600) :: escaped_text
        call prepare_text_for_raster(text, escaped_text)

        ! Transform coordinates to plot area (like matplotlib)
        ! Note: Raster Y=0 at top, so we need to flip Y coordinates
        px = (x - this%x_min)/(this%x_max - this%x_min)*real(this%plot_area%width, wp) &
             + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min)/(this%y_max - this%y_min)*real(this%plot_area%height, wp)

        call this%raster%get_color_bytes(r, g, b)
        call render_text_to_image(this%raster%image_data, this%width, this%height, &
                                  int(px), int(py), trim(escaped_text), r, g, b)
    end subroutine raster_draw_text

    module subroutine raster_draw_text_styled(this, x_px, y_px, text, pixel_height, &
                                       rotation, ha, va, bbox, color)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_px, y_px
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: pixel_height
        real(wp), intent(in) :: rotation
        character(len=*), intent(in) :: ha, va
        logical, intent(in) :: bbox
        real(wp), intent(in) :: color(3)

        integer :: x0, y0
        integer :: text_w, text_h
        integer(1) :: r, g, b
        real(wp) :: w_px, h_px
        real(wp) :: pad
        real(wp) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp) :: cr, cg, cb
        real(wp) :: ascent_px, descent_px
        integer :: baseline_y
        integer(1), allocatable :: text_bitmap(:, :, :)
        integer(1), allocatable :: rotated_bitmap(:, :, :)
        integer :: rot_w, rot_h
        real(wp) :: ax_src, ay_src
        real(wp) :: ax_dst, ay_dst
        logical :: have_metrics

        cr = max(0.0_wp, min(1.0_wp, color(1)))
        cg = max(0.0_wp, min(1.0_wp, color(2)))
        cb = max(0.0_wp, min(1.0_wp, color(3)))
        r = int(nint(cr*255.0_wp), kind=1)
        g = int(nint(cg*255.0_wp), kind=1)
        b = int(nint(cb*255.0_wp), kind=1)

        text_w = calculate_text_width_with_size(trim(text), pixel_height)
        text_h = max(1, int(pixel_height))
        ascent_px = 0.0_wp
        descent_px = 0.0_wp
        have_metrics = .false.
        call get_text_bitmap_metrics(pixel_height, ascent_px, descent_px, text_h, &
                                     have_metrics)
        if (.not. have_metrics) then
            text_h = max(1, int(1.2_wp*pixel_height))
            descent_px = 0.25_wp*pixel_height
        end if
        baseline_y = max(0, text_h - int(abs(descent_px)))
        w_px = real(text_w, wp)
        h_px = real(text_h, wp)

        ax_src = 0.0_wp
        select case (trim(ha))
        case ('center')
            ax_src = 0.5_wp*w_px
        case ('right')
            ax_src = w_px
        case default
            ax_src = 0.0_wp
        end select

        ay_src = 0.0_wp
        select case (trim(va))
        case ('center')
            ay_src = 0.5_wp*h_px
        case ('bottom')
            ay_src = h_px
        case ('top')
            ay_src = 0.0_wp
        case default
            ay_src = 0.0_wp
        end select

        if (bbox) then
            pad = max(2.0_wp, 0.2_wp*pixel_height)
        else
            pad = 0.0_wp
        end if

        if (abs(rotation) > 1.0e-6_wp) then
            call raster_draw_rotated_text_with_bbox(this%raster%image_data, this%width, &
                                                    this%height, x_px, y_px, &
                                                    trim(text), r, g, b, pixel_height, &
                                                    rotation, ha, va, bbox, ax_src, &
                                                    ay_src, pad)
        else
            call raster_draw_text_with_bbox(this%raster%image_data, this%width, &
                                            this%height, x_px, y_px, trim(text), r, &
                                            g, b, pixel_height, ha, va, bbox, &
                                            text_w, text_h, ascent_px, descent_px, &
                                            pad)
        end if
    end subroutine raster_draw_text_styled

    module subroutine raster_draw_text_with_bbox(image_data, width, height, x_px, y_px, &
                                          text, r, g, b, pixel_height, ha, va, &
                                          bbox, text_w, text_h, ascent_px, &
                                          descent_px, pad)
        !! Draw text with optional bounding box (non-rotated case)
        !! Refactored helper for raster_draw_text_styled (Issue #1747)
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: width, height
        real(wp), intent(in) :: x_px, y_px
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: pixel_height
        character(len=*), intent(in) :: ha, va
        logical, intent(in) :: bbox
        integer, intent(in) :: text_w, text_h
        real(wp), intent(in) :: ascent_px, descent_px
        real(wp), intent(in) :: pad

        integer :: x0, y0, baseline_i
        real(wp) :: w_px, h_px
        real(wp) :: baseline_px, top_px
        real(wp) :: descent_abs
        real(wp) :: h_box
        real(wp) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp) :: ax_src

        w_px = real(text_w, wp)
        h_px = real(text_h, wp)

        ax_src = 0.0_wp
        select case (trim(ha))
        case ('center')
            ax_src = 0.5_wp*w_px
        case ('right')
            ax_src = w_px
        case default
            ax_src = 0.0_wp
        end select

        x0 = int(nint(x_px - ax_src))

        descent_abs = abs(descent_px)
        h_box = max(1.0_wp, ascent_px + descent_abs)

        baseline_px = y_px
        select case (trim(va))
        case ('center')
            baseline_px = y_px + 0.5_wp*(ascent_px - descent_abs)
        case ('top')
            baseline_px = y_px + ascent_px
        case ('bottom')
            baseline_px = y_px - descent_abs
        case default
        end select

        top_px = baseline_px - ascent_px
        y0 = int(nint(top_px))
        baseline_i = int(nint(baseline_px))

        if (bbox) then
            x1 = real(x0, wp) - pad
            y1 = real(y0, wp) - pad
            x2 = real(x0, wp) + w_px + pad
            y2 = y1
            x3 = x2
            y3 = real(y0, wp) + h_box + pad
            x4 = x1
            y4 = y3

            call fill_triangle(image_data, width, height, x1, y1, x2, y2, x3, y3, &
                              -1_1, -1_1, -1_1)
            call fill_triangle(image_data, width, height, x1, y1, x3, y3, x4, y4, &
                              -1_1, -1_1, -1_1)

            call draw_line_distance_aa(image_data, width, height, x1, y1, x2, y2, &
                                       0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp)
            call draw_line_distance_aa(image_data, width, height, x2, y2, x3, y3, &
                                       0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp)
            call draw_line_distance_aa(image_data, width, height, x3, y3, x4, y4, &
                                       0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp)
            call draw_line_distance_aa(image_data, width, height, x4, y4, x1, y1, &
                                       0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp)
        end if

        call render_text_with_size(image_data, width, height, x0, baseline_i, &
                                   trim(text), r, g, b, pixel_height)
    end subroutine raster_draw_text_with_bbox

    module subroutine raster_draw_rotated_text_with_bbox(image_data, width, height, x_px, &
                                                  y_px, text, r, g, b, pixel_height, &
                                                  rotation, ha, va, bbox, ax_src, &
                                                  ay_src, pad)
        !! Draw rotated text with optional bounding box
        !! Refactored helper for raster_draw_text_styled (Issue #1747)
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: width, height
        real(wp), intent(in) :: x_px, y_px
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: pixel_height
        real(wp), intent(in) :: rotation
        character(len=*), intent(in) :: ha, va
        logical, intent(in) :: bbox
        real(wp), intent(in) :: ax_src, ay_src, pad

        integer :: text_w, text_h
        integer(1), allocatable :: text_bitmap(:, :, :)
        integer(1), allocatable :: rotated_bitmap(:, :, :)
        integer :: rot_w, rot_h
        integer :: x0, y0
        real(wp) :: ax_dst, ay_dst
        real(wp) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp) :: ascent_px, descent_px
        integer :: baseline_y
        logical :: have_metrics

        text_w = calculate_text_width_with_size(trim(text), pixel_height)
        text_h = max(1, int(pixel_height))
        ascent_px = 0.0_wp
        descent_px = 0.0_wp
        have_metrics = .false.
        call get_text_bitmap_metrics(pixel_height, ascent_px, descent_px, text_h, &
                                     have_metrics)
        if (.not. have_metrics) then
            text_h = max(1, int(1.2_wp*pixel_height))
            descent_px = 0.25_wp*pixel_height
        end if
        baseline_y = max(0, text_h - int(abs(descent_px)))

        allocate(text_bitmap(text_w, text_h, 3))
        text_bitmap = -1_1
        call render_text_to_bitmap_with_size(text_bitmap, text_w, text_h, 0, &
                                             baseline_y, trim(text), r, g, b, &
                                             pixel_height)

        call rotate_bitmap_about_anchor(text_bitmap, text_w, text_h, rotation, ax_src, &
                                        ay_src, rotated_bitmap, rot_w, rot_h, ax_dst, &
                                        ay_dst)

        x0 = int(nint(x_px - ax_dst))
        y0 = int(nint(y_px - ay_dst))

        if (bbox) then
            x1 = real(x0, wp) - pad
            y1 = real(y0, wp) - pad
            x2 = real(x0 + rot_w, wp) + pad
            y2 = y1
            x3 = x2
            y3 = real(y0 + rot_h, wp) + pad
            x4 = x1
            y4 = y3

            call fill_triangle(image_data, width, height, x1, y1, x2, y2, x3, y3, &
                              -1_1, -1_1, -1_1)
            call fill_triangle(image_data, width, height, x1, y1, x3, y3, x4, y4, &
                              -1_1, -1_1, -1_1)

            call draw_line_distance_aa(image_data, width, height, x1, y1, x2, y2, &
                                       0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp)
            call draw_line_distance_aa(image_data, width, height, x2, y2, x3, y3, &
                                       0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp)
            call draw_line_distance_aa(image_data, width, height, x3, y3, x4, y4, &
                                       0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp)
            call draw_line_distance_aa(image_data, width, height, x4, y4, x1, y1, &
                                       0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp)
        end if

        call composite_bitmap_to_raster_0(image_data, width, height, rotated_bitmap, &
                                          rot_w, rot_h, x0, y0)
    end subroutine raster_draw_rotated_text_with_bbox

    !! ── Utility methods ────────────────────────────────────────────────

    module function raster_get_ascii_output(this) result(output)
        !! Get ASCII output (not applicable for raster backend)
        class(raster_context), intent(in) :: this
        character(len=:), allocatable :: output

        output = ""  ! Raster backend doesn't produce ASCII output
    end function raster_get_ascii_output

    module function raster_get_width_scale(this) result(scale)
        !! Get width scaling factor for coordinate transformation
        class(raster_context), intent(in) :: this
        real(wp) :: scale

        ! Calculate scaling from logical to pixel coordinates
        if (this%width > 0 .and. this%x_max > this%x_min) then
            scale = real(this%width, wp)/(this%x_max - this%x_min)
        else
            scale = 1.0_wp
        end if
    end function raster_get_width_scale

    module function raster_get_height_scale(this) result(scale)
        !! Get height scaling factor for coordinate transformation
        class(raster_context), intent(in) :: this
        real(wp) :: scale

        ! Calculate scaling from logical to pixel coordinates
        if (this%height > 0 .and. this%y_max > this%y_min) then
            scale = real(this%height, wp)/(this%y_max - this%y_min)
        else
            scale = 1.0_wp
        end if
    end function raster_get_height_scale

end submodule fortplot_raster_text_impl
