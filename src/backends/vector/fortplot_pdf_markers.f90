module fortplot_pdf_markers
    !! PDF marker and graphics state operations
    !! Handles marker drawing, color management, and graphics state operations

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortplot_pdf_drawing, only: pdf_stream_writer, draw_pdf_arrow, &
                                    draw_pdf_circle_with_outline, &
                                    draw_pdf_square_with_outline, &
                                    draw_pdf_diamond_with_outline, draw_pdf_x_marker
    use fortplot_pdf_coordinate, only: pdf_context_handle, normalize_to_pdf_coords
    implicit none

    private

    public :: draw_pdf_marker_at_coords
    public :: pdf_set_marker_colors, pdf_set_marker_colors_with_alpha
    public :: draw_pdf_arrow_at_coords

contains

    subroutine draw_pdf_marker_at_coords(ctx_handle, stream_writer, x, y, style)
        type(pdf_context_handle), intent(in) :: ctx_handle
        type(pdf_stream_writer), intent(inout) :: stream_writer
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp) :: pdf_x, pdf_y
        real(wp) :: size

        size = 5.0_wp
        call normalize_to_pdf_coords(ctx_handle, x, y, pdf_x, pdf_y)

        ! Save state for marker drawing
        call stream_writer%save_state()

        ! Draw marker based on style
        select case (trim(style))
        case ('o', 'circle')
            call draw_pdf_circle_with_outline(stream_writer, pdf_x, pdf_y, size)
        case ('s', 'square')
            call draw_pdf_square_with_outline(stream_writer, pdf_x, pdf_y, size)
        case ('D', 'd', 'diamond')
            call draw_pdf_diamond_with_outline(stream_writer, pdf_x, pdf_y, size)
        case ('x', 'cross')
            call draw_pdf_x_marker(stream_writer, pdf_x, pdf_y, size)
        end select

        ! Restore state
        call stream_writer%restore_state()
    end subroutine draw_pdf_marker_at_coords

    subroutine pdf_set_marker_colors(stream_writer, edge_r, edge_g, edge_b, &
                                     face_r, face_g, face_b)
        type(pdf_stream_writer), intent(inout) :: stream_writer
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b

        call pdf_set_stroke_color(stream_writer, edge_r, edge_g, edge_b)
        call pdf_set_fill_color(stream_writer, face_r, face_g, face_b)
    end subroutine pdf_set_marker_colors

    subroutine pdf_set_marker_colors_with_alpha(stream_writer, edge_r, edge_g, edge_b, &
                                                edge_alpha, face_r, face_g, face_b, &
                                                face_alpha)
        type(pdf_stream_writer), intent(inout) :: stream_writer
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha

        real(wp) :: edge_alpha_safe, face_alpha_safe
        real(wp) :: edge_r_eff, edge_g_eff, edge_b_eff
        real(wp) :: face_r_eff, face_g_eff, face_b_eff

        edge_alpha_safe = edge_alpha
        face_alpha_safe = face_alpha

        if (.not. ieee_is_finite(edge_alpha_safe)) edge_alpha_safe = 1.0_wp
        if (.not. ieee_is_finite(face_alpha_safe)) face_alpha_safe = 1.0_wp

        edge_alpha_safe = max(0.0_wp, min(1.0_wp, edge_alpha_safe))
        face_alpha_safe = max(0.0_wp, min(1.0_wp, face_alpha_safe))

        edge_r_eff = 1.0_wp*(1.0_wp - edge_alpha_safe) + edge_r*edge_alpha_safe
        edge_g_eff = 1.0_wp*(1.0_wp - edge_alpha_safe) + edge_g*edge_alpha_safe
        edge_b_eff = 1.0_wp*(1.0_wp - edge_alpha_safe) + edge_b*edge_alpha_safe

        face_r_eff = 1.0_wp*(1.0_wp - face_alpha_safe) + face_r*face_alpha_safe
        face_g_eff = 1.0_wp*(1.0_wp - face_alpha_safe) + face_g*face_alpha_safe
        face_b_eff = 1.0_wp*(1.0_wp - face_alpha_safe) + face_b*face_alpha_safe

        call pdf_set_marker_colors(stream_writer, edge_r_eff, edge_g_eff, edge_b_eff, &
                                   face_r_eff, face_g_eff, face_b_eff)
    end subroutine pdf_set_marker_colors_with_alpha

    subroutine draw_pdf_arrow_at_coords(ctx_handle, stream_writer, x, y, dx, dy, &
                                        size, style)
        type(pdf_context_handle), intent(in) :: ctx_handle
        type(pdf_stream_writer), intent(inout) :: stream_writer
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        real(wp) :: pdf_x, pdf_y, pdf_dx, pdf_dy, pdf_size
        real(wp) :: x_range, y_range, left, right, bottom, top, scale_factor
        real(wp), parameter :: EPSILON = 1.0e-10_wp

        call normalize_to_pdf_coords(ctx_handle, x, y, pdf_x, pdf_y)

        x_range = ctx_handle%x_max - ctx_handle%x_min
        y_range = ctx_handle%y_max - ctx_handle%y_min

        left = real(ctx_handle%plot_area%left, wp)
        right = real(ctx_handle%plot_area%left + ctx_handle%plot_area%width, wp)
        bottom = real(ctx_handle%plot_area%bottom, wp)
        top = real(ctx_handle%plot_area%bottom + ctx_handle%plot_area%height, wp)

        scale_factor = sqrt((right - left)**2 + (top - bottom)**2)/sqrt(2.0_wp)

        if (abs(x_range) > EPSILON) then
            pdf_dx = dx*(right - left)/x_range
        else
            pdf_dx = 0.0_wp
        end if

        if (abs(y_range) > EPSILON) then
            pdf_dy = dy*(top - bottom)/y_range
        else
            pdf_dy = 0.0_wp
        end if

        if (abs(pdf_dx) < EPSILON .and. abs(pdf_dy) < EPSILON) return

        pdf_size = size*scale_factor/100.0_wp

        call draw_pdf_arrow(stream_writer, pdf_x, pdf_y, pdf_dx, pdf_dy, &
                            pdf_size, style)
    end subroutine draw_pdf_arrow_at_coords

    subroutine pdf_set_stroke_color(stream_writer, r, g, b)
        type(pdf_stream_writer), intent(inout) :: stream_writer
        real(wp), intent(in) :: r, g, b
        character(len=64) :: cmd
        real(wp) :: rr, gg, bb

        rr = pdf_color_component(r)
        gg = pdf_color_component(g)
        bb = pdf_color_component(b)

        write (cmd, '(F5.3,1X,F5.3,1X,F5.3," RG")') rr, gg, bb
        call stream_writer%add_to_stream(trim(cmd))
    end subroutine pdf_set_stroke_color

    subroutine pdf_set_fill_color(stream_writer, r, g, b)
        type(pdf_stream_writer), intent(inout) :: stream_writer
        real(wp), intent(in) :: r, g, b
        character(len=64) :: cmd
        real(wp) :: rr, gg, bb

        rr = pdf_color_component(r)
        gg = pdf_color_component(g)
        bb = pdf_color_component(b)

        write (cmd, '(F5.3,1X,F5.3,1X,F5.3," rg")') rr, gg, bb
        call stream_writer%add_to_stream(trim(cmd))
    end subroutine pdf_set_fill_color

    real(wp) function pdf_color_component(value) result(safe)
        real(wp), intent(in) :: value
        if (.not. ieee_is_finite(value)) then
            safe = 0.0_wp
        else
            safe = max(0.0_wp, min(1.0_wp, value))
        end if
    end function pdf_color_component

end module fortplot_pdf_markers
