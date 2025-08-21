module fortplot_raster
    use iso_c_binding
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_text, only: render_text_to_image, calculate_text_width, calculate_text_height
    use fortplot_latex_parser, only: process_latex_in_text
    ! use fortplot_unicode, only: unicode_to_ascii
    use fortplot_logging, only: log_error
    use fortplot_errors, only: fortplot_error_t, ERROR_INTERNAL
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area, get_axis_tick_positions
    use fortplot_ticks, only: generate_scale_aware_tick_labels, format_tick_value_smart, find_nice_tick_locations
    use fortplot_scales, only: apply_scale_transform
    use fortplot_label_positioning, only: calculate_x_label_position, calculate_y_label_position, &
                                         calculate_x_tick_label_position, calculate_y_tick_label_position, &
                                         calculate_x_axis_label_position, calculate_y_axis_label_position
    use fortplot_markers, only: get_marker_size, MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND, MARKER_CROSS
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_image_t, create_raster_image, destroy_raster_image
    public :: initialize_white_background, color_to_byte
    public :: draw_line_distance_aa, blend_pixel, composite_image, composite_bitmap_to_raster
    public :: distance_point_to_line_segment, ipart, fpart, rfpart
    public :: render_text_to_bitmap, rotate_bitmap_90_cw, rotate_bitmap_90_ccw, bitmap_to_png_buffer
    public :: raster_context, create_raster_canvas, draw_axes_and_labels, draw_rotated_ylabel_raster
    public :: draw_filled_quad_raster

    integer, parameter :: DEFAULT_RASTER_LINE_WIDTH_SCALING = 10

    type :: raster_image_t
        integer(1), allocatable :: image_data(:)
        integer :: width, height
        real(wp) :: current_r = 0.0_wp, current_g = 0.0_wp, current_b = 1.0_wp
        real(wp) :: current_line_width = 1.0_wp
        ! Marker colors - separate edge and face colors with alpha
        real(wp) :: marker_edge_r = 0.0_wp, marker_edge_g = 0.0_wp, marker_edge_b = 0.0_wp, marker_edge_alpha = 1.0_wp
        real(wp) :: marker_face_r = 1.0_wp, marker_face_g = 0.0_wp, marker_face_b = 0.0_wp, marker_face_alpha = 1.0_wp
    contains
        procedure :: set_color => raster_set_color
        procedure :: get_color_bytes => raster_get_color_bytes
    end type raster_image_t

    ! Raster plotting context - backend-agnostic bitmap operations
    type, extends(plot_context) :: raster_context
        type(raster_image_t) :: raster
        ! Plot area calculations (using common margin functionality)
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
    contains
        procedure :: line => raster_draw_line
        procedure :: color => raster_set_color_context
        procedure :: text => raster_draw_text
        procedure :: set_line_width => raster_set_line_width
        procedure :: save => raster_save_dummy
        procedure :: draw_marker => raster_draw_marker
        procedure :: set_marker_colors => raster_set_marker_colors
        procedure :: set_marker_colors_with_alpha => raster_set_marker_colors_with_alpha
        procedure :: fill_quad => raster_fill_quad
        procedure :: draw_arrow => raster_draw_arrow
        procedure :: get_ascii_output => raster_get_ascii_output
        
        !! New polymorphic methods to eliminate SELECT TYPE
        procedure :: get_width_scale => raster_get_width_scale
        procedure :: get_height_scale => raster_get_height_scale
        procedure :: fill_heatmap => raster_fill_heatmap
        procedure :: render_legend_specialized => raster_render_legend_specialized
        procedure :: calculate_legend_dimensions => raster_calculate_legend_dimensions
        procedure :: set_legend_border_width => raster_set_legend_border_width
        procedure :: calculate_legend_position_backend => raster_calculate_legend_position
        procedure :: extract_rgb_data => raster_extract_rgb_data
        procedure :: get_png_data_backend => raster_get_png_data
        procedure :: prepare_3d_data => raster_prepare_3d_data
        procedure :: render_ylabel => raster_render_ylabel
        procedure :: draw_axes_and_labels_backend => raster_draw_axes_and_labels
        procedure :: save_coordinates => raster_save_coordinates
        procedure :: set_coordinates => raster_set_coordinates
    end type raster_context

contains

    function create_raster_image(width, height) result(image)
        integer, intent(in) :: width, height
        type(raster_image_t) :: image

        image%width = width
        image%height = height
        allocate(image%image_data(width * height * 3))
        call initialize_white_background(image%image_data, width, height)
    end function create_raster_image

    subroutine destroy_raster_image(image)
        type(raster_image_t), intent(inout) :: image
        if (allocated(image%image_data)) deallocate(image%image_data)
    end subroutine destroy_raster_image

    subroutine raster_set_color(this, r, g, b)
        class(raster_image_t), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        this%current_r = r
        this%current_g = g
        this%current_b = b
    end subroutine raster_set_color

    subroutine raster_get_color_bytes(this, r, g, b)
        class(raster_image_t), intent(in) :: this
        integer(1), intent(out) :: r, g, b

        r = color_to_byte(this%current_r)
        g = color_to_byte(this%current_g)
        b = color_to_byte(this%current_b)
    end subroutine raster_get_color_bytes

    subroutine initialize_white_background(image_data, w, h)
        integer(1), intent(out) :: image_data(*)
        integer, intent(in) :: w, h
        integer :: i, j, k

        k = 1
        do i = 1, h
            do j = 1, w
                image_data(k) = -1_1     ! R (white = 255 = -1 in signed byte)
                image_data(k+1) = -1_1   ! G
                image_data(k+2) = -1_1   ! B
                k = k + 3
            end do
        end do
    end subroutine initialize_white_background

    function color_to_byte(color_val) result(byte_val)
        real(wp), intent(in) :: color_val
        integer(1) :: byte_val
        integer :: int_val

        ! Clamp color_val to [0,1] range to prevent overflow
        int_val = int(max(0.0_wp, min(1.0_wp, color_val)) * 255.0_wp)
        
        ! Ensure int_val is in valid range [0, 255] and prevent signed byte overflow
        ! Clamp to [0, 255] but also avoid exactly 128 which causes signed byte overflow
        int_val = max(0, min(255, int_val))
        if (int_val == 128) int_val = 127  ! Avoid signed byte overflow
        
        if (int_val > 127) then
            byte_val = int(int_val - 256, 1)
        else
            byte_val = int(int_val, 1)
        end if
    end function color_to_byte

    subroutine draw_line_distance_aa(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b, width)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x0, y0, x1, y1
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: width
        real(wp) :: line_width

        real(wp) :: dx, dy, length, nx, ny
        integer :: x_min, x_max, y_min, y_max, x, y
        real(wp) :: px, py, dist, alpha

        line_width = width

        dx = x1 - x0
        dy = y1 - y0
        length = sqrt(dx*dx + dy*dy)

        if (length < 1e-6_wp) return

        nx = -dy / length
        ny = dx / length

        x_min = max(1, int(min(x0, x1) - line_width - 1.0_wp))
        x_max = min(img_w, int(max(x0, x1) + line_width + 1.0_wp))
        y_min = max(1, int(min(y0, y1) - line_width - 1.0_wp))
        y_max = min(img_h, int(max(y0, y1) + line_width + 1.0_wp))

        do y = y_min, y_max
            do x = x_min, x_max
                px = real(x, wp)
                py = real(y, wp)

                dist = distance_point_to_line_segment(px, py, x0, y0, x1, y1)
                if (dist <= line_width) then
                    alpha = 1.0_wp
                else if (dist <= line_width + 1.0_wp) then
                    alpha = line_width + 1.0_wp - dist
                else
                    alpha = 0.0_wp
                end if

                if (alpha > 0.0_wp) then
                    call blend_pixel(image_data, img_w, img_h, x, y, alpha, r, g, b)
                end if
            end do
        end do
    end subroutine draw_line_distance_aa

    real(wp) function distance_point_to_line_segment(px, py, x0, y0, x1, y1) result(dist)
        real(wp), intent(in) :: px, py, x0, y0, x1, y1
        real(wp) :: dx, dy, length_sq, t, closest_x, closest_y

        dx = x1 - x0
        dy = y1 - y0
        length_sq = dx*dx + dy*dy

        if (length_sq < 1e-12_wp) then
            dist = sqrt((px - x0)**2 + (py - y0)**2)
            return
        end if

        t = ((px - x0) * dx + (py - y0) * dy) / length_sq
        t = max(0.0_wp, min(1.0_wp, t))

        closest_x = x0 + t * dx
        closest_y = y0 + t * dy

        dist = sqrt((px - closest_x)**2 + (py - closest_y)**2)
    end function distance_point_to_line_segment

    subroutine blend_pixel(image_data, img_w, img_h, x, y, alpha, new_r, new_g, new_b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h, x, y
        real(wp), intent(in) :: alpha
        integer(1), intent(in) :: new_r, new_g, new_b
        integer :: k
        real(wp) :: inv_alpha
        integer :: bg_r, bg_g, bg_b, fg_r, fg_g, fg_b

        if (x < 1 .or. x > img_w .or. y < 1 .or. y > img_h) return
        if (alpha <= 0.0_wp) return

        k = (y - 1) * img_w * 3 + (x - 1) * 3 + 1

        if (alpha >= 1.0_wp) then
            image_data(k) = new_r
            image_data(k+1) = new_g
            image_data(k+2) = new_b
        else
            inv_alpha = 1.0_wp - alpha

            bg_r = int(image_data(k))
            bg_g = int(image_data(k+1))
            bg_b = int(image_data(k+2))
            if (bg_r < 0) bg_r = bg_r + 256
            if (bg_g < 0) bg_g = bg_g + 256
            if (bg_b < 0) bg_b = bg_b + 256

            fg_r = int(new_r)
            fg_g = int(new_g)
            fg_b = int(new_b)
            if (fg_r < 0) fg_r = fg_r + 256
            if (fg_g < 0) fg_g = fg_g + 256
            if (fg_b < 0) fg_b = fg_b + 256

            bg_r = int(inv_alpha * real(bg_r) + alpha * real(fg_r))
            bg_g = int(inv_alpha * real(bg_g) + alpha * real(fg_g))
            bg_b = int(inv_alpha * real(bg_b) + alpha * real(fg_b))

            ! Clamp to [0, 255] range and handle signed byte conversion properly
            bg_r = max(0, min(255, bg_r))
            bg_g = max(0, min(255, bg_g))
            bg_b = max(0, min(255, bg_b))
            
            ! Prevent exactly 128 which causes signed byte overflow
            if (bg_r == 128) bg_r = 127
            if (bg_g == 128) bg_g = 127
            if (bg_b == 128) bg_b = 127
            
            if (bg_r > 127) bg_r = bg_r - 256
            if (bg_g > 127) bg_g = bg_g - 256
            if (bg_b > 127) bg_b = bg_b - 256

            image_data(k) = int(bg_r, 1)
            image_data(k+1) = int(bg_g, 1)
            image_data(k+2) = int(bg_b, 1)
        end if
    end subroutine blend_pixel

    subroutine composite_image(main_image, main_width, main_height, &
                              overlay_image, overlay_width, overlay_height, dest_x, dest_y)
        integer(1), intent(inout) :: main_image(*)
        integer, intent(in) :: main_width, main_height
        integer(1), intent(in) :: overlay_image(*)
        integer, intent(in) :: overlay_width, overlay_height, dest_x, dest_y
        integer :: x, y, src_idx, dst_idx, img_x, img_y
        
        do y = 1, overlay_height
            do x = 1, overlay_width
                img_x = dest_x + x - 1
                img_y = dest_y + y - 1
                
                if (img_x >= 1 .and. img_x <= main_width .and. &
                    img_y >= 1 .and. img_y <= main_height) then
                    
                    src_idx = ((y - 1) * overlay_width + (x - 1)) * 3 + 1
                    dst_idx = ((img_y - 1) * main_width + (img_x - 1)) * 3 + 1
                    
                    if (overlay_image(src_idx) /= -1_1 .or. &
                        overlay_image(src_idx+1) /= -1_1 .or. &
                        overlay_image(src_idx+2) /= -1_1) then
                        main_image(dst_idx:dst_idx+2) = overlay_image(src_idx:src_idx+2)
                    end if
                end if
            end do
        end do
    end subroutine composite_image

    subroutine composite_bitmap_to_raster(raster_buffer, raster_width, raster_height, &
                                         bitmap, bitmap_width, bitmap_height, dest_x, dest_y)
        !! Composite 3D RGB bitmap directly onto raster image buffer
        integer(1), intent(inout) :: raster_buffer(*)
        integer, intent(in) :: raster_width, raster_height
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: bitmap_width, bitmap_height, dest_x, dest_y
        integer :: x, y, raster_x, raster_y, raster_idx
        
        do y = 1, bitmap_height
            do x = 1, bitmap_width
                raster_x = dest_x + x - 1
                raster_y = dest_y + y - 1
                
                ! Check bounds
                if (raster_x >= 1 .and. raster_x <= raster_width .and. &
                    raster_y >= 1 .and. raster_y <= raster_height) then
                    
                    ! Skip white pixels (don't composite background)
                    if (bitmap(x, y, 1) /= -1_1 .or. &
                        bitmap(x, y, 2) /= -1_1 .or. &
                        bitmap(x, y, 3) /= -1_1) then
                        
                        raster_idx = ((raster_y - 1) * raster_width + (raster_x - 1)) * 3 + 1
                        raster_buffer(raster_idx)     = bitmap(x, y, 1)  ! R
                        raster_buffer(raster_idx + 1) = bitmap(x, y, 2)  ! G
                        raster_buffer(raster_idx + 2) = bitmap(x, y, 3)  ! B
                    end if
                end if
            end do
        end do
    end subroutine composite_bitmap_to_raster

    real(wp) function ipart(x)
        real(wp), intent(in) :: x
        ipart = floor(x)
    end function ipart

    real(wp) function fpart(x)
        real(wp), intent(in) :: x
        fpart = x - floor(x)
    end function fpart

    real(wp) function rfpart(x)
        real(wp), intent(in) :: x
        rfpart = 1.0_wp - fpart(x)
    end function rfpart

    subroutine render_text_to_bitmap(bitmap, width, height, x, y, text)
        !! Render text to RGB bitmap by using existing PNG rendering then converting
        use fortplot_text, only: render_text_to_image
        integer(1), intent(inout) :: bitmap(:,:,:)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        
        ! Create temporary PNG buffer for text rendering
        integer(1), allocatable :: temp_buffer(:)
        integer :: i, j, buf_idx
        
        allocate(temp_buffer(width * height * 3))
        call initialize_white_background(temp_buffer, width, height)
        call render_text_to_image(temp_buffer, width, height, x, y, text, 0_1, 0_1, 0_1)
        
        ! Convert PNG buffer to bitmap
        do j = 1, height
            do i = 1, width
                buf_idx = ((j - 1) * width + (i - 1)) * 3 + 1
                bitmap(i, j, 1) = temp_buffer(buf_idx)     ! R
                bitmap(i, j, 2) = temp_buffer(buf_idx + 1) ! G  
                bitmap(i, j, 3) = temp_buffer(buf_idx + 2) ! B
            end do
        end do
        
        deallocate(temp_buffer)
    end subroutine render_text_to_bitmap

    subroutine rotate_bitmap_90_ccw(src_bitmap, dst_bitmap, src_width, src_height)
        !! Rotate bitmap 90 degrees clockwise: (x,y) -> (y, src_width-x+1)
        integer(1), intent(in) :: src_bitmap(:,:,:)
        integer(1), intent(out) :: dst_bitmap(:,:,:)
        integer, intent(in) :: src_width, src_height
        integer :: i, j
        
        do j = 1, src_height
            do i = 1, src_width
                dst_bitmap(j, src_width - i + 1, :) = src_bitmap(i, j, :)
            end do
        end do
    end subroutine rotate_bitmap_90_ccw

    subroutine rotate_bitmap_90_cw(src_bitmap, dst_bitmap, src_width, src_height)
        !! Rotate bitmap 90 degrees counter-clockwise: (x,y) -> (src_height-y+1, x)
        integer(1), intent(in) :: src_bitmap(:,:,:)
        integer(1), intent(out) :: dst_bitmap(:,:,:)
        integer, intent(in) :: src_width, src_height
        integer :: i, j
        
        do j = 1, src_height
            do i = 1, src_width
                dst_bitmap(src_height - j + 1, i, :) = src_bitmap(i, j, :)
            end do
        end do
    end subroutine rotate_bitmap_90_cw

    subroutine bitmap_to_png_buffer(bitmap, width, height, buffer)
        !! Convert 3D RGB bitmap to PNG buffer format with filter bytes
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: width, height
        integer(1), intent(out) :: buffer(:)
        integer :: i, j, buf_idx, row_start
        
        ! PNG format: each row starts with filter byte (0 = no filter) followed by RGB data
        do j = 1, height
            row_start = (j - 1) * (1 + width * 3) + 1
            buffer(row_start) = 0_1  ! PNG filter byte: 0 = no filter
            
            do i = 1, width
                buf_idx = row_start + 1 + (i - 1) * 3
                buffer(buf_idx)     = bitmap(i, j, 1) ! R
                buffer(buf_idx + 1) = bitmap(i, j, 2) ! G
                buffer(buf_idx + 2) = bitmap(i, j, 3) ! B
            end do
        end do
    end subroutine bitmap_to_png_buffer

    function create_raster_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(raster_context) :: ctx

        call setup_canvas(ctx, width, height)

        ctx%raster = create_raster_image(width, height)

        ! Set up matplotlib-style margins using common module
        ctx%margins = plot_margins_t()  ! Use defaults
        call calculate_plot_area(width, height, ctx%margins, ctx%plot_area)
    end function create_raster_canvas

    subroutine raster_draw_line(this, x1, y1, x2, y2)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: px1, py1, px2, py2
        integer(1) :: r, g, b

        ! Transform coordinates to plot area (like matplotlib)
        ! Note: Raster Y=0 at top, so we need to flip Y coordinates
        px1 = (x1 - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py1 = real(this%plot_area%bottom + this%plot_area%height, wp) - &
              (y1 - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)
        px2 = (x2 - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py2 = real(this%plot_area%bottom + this%plot_area%height, wp) - &
              (y2 - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)

        call this%raster%get_color_bytes(r, g, b)

        call draw_line_distance_aa(this%raster%image_data, this%width, this%height, &
                                    px1, py1, px2, py2, r, g, b, this%raster%current_line_width)
    end subroutine raster_draw_line

    subroutine raster_set_color_context(this, r, g, b)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        call this%raster%set_color(r, g, b)
    end subroutine raster_set_color_context

    subroutine raster_set_line_width(this, width)
        !! Set line width for raster drawing with automatic scaling for pixel rendering
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: width

        ! Raster needs specific line widths due to pixel-based rendering
        ! Map common width values to appropriate pixel thickness
        if (abs(width - 2.0_wp) < 1e-6_wp) then
            ! Plot data lines: use 0.5 for main plot visibility
            this%raster%current_line_width = 0.5_wp
        else
            ! Axes and other elements: use 0.1 for fine lines
            this%raster%current_line_width = 0.1_wp
        end if
    end subroutine raster_set_line_width

    subroutine escape_unicode_for_raster(input_text, escaped_text)
        !! Pass through Unicode for raster rendering (STB TrueType supports Unicode)
        character(len=*), intent(in) :: input_text
        character(len=*), intent(out) :: escaped_text
        
        ! STB TrueType can handle Unicode directly, so just pass through
        escaped_text = input_text
    end subroutine escape_unicode_for_raster

    subroutine unicode_codepoint_to_ascii(codepoint, ascii_equiv)
        !! Convert Unicode codepoint to ASCII equivalent
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: ascii_equiv
        
        ! Convert Greek letters to ASCII names
        select case (codepoint)
        case (945) ! α
            ascii_equiv = "alpha"
        case (946) ! β
            ascii_equiv = "beta"
        case (947) ! γ
            ascii_equiv = "gamma"
        case (948) ! δ
            ascii_equiv = "delta"
        case (949) ! ε
            ascii_equiv = "epsilon"
        case (950) ! ζ
            ascii_equiv = "zeta"
        case (951) ! η
            ascii_equiv = "eta"
        case (952) ! θ
            ascii_equiv = "theta"
        case (953) ! ι
            ascii_equiv = "iota"
        case (954) ! κ
            ascii_equiv = "kappa"
        case (955) ! λ
            ascii_equiv = "lambda"
        case (956) ! μ
            ascii_equiv = "mu"
        case (957) ! ν
            ascii_equiv = "nu"
        case (958) ! ξ
            ascii_equiv = "xi"
        case (959) ! ο
            ascii_equiv = "omicron"
        case (960) ! π
            ascii_equiv = "pi"
        case (961) ! ρ
            ascii_equiv = "rho"
        case (963) ! σ
            ascii_equiv = "sigma"
        case (964) ! τ
            ascii_equiv = "tau"
        case (965) ! υ
            ascii_equiv = "upsilon"
        case (966) ! φ
            ascii_equiv = "phi"
        case (967) ! χ
            ascii_equiv = "chi"
        case (968) ! ψ
            ascii_equiv = "psi"
        case (969) ! ω
            ascii_equiv = "omega"
        case (913) ! Α
            ascii_equiv = "Alpha"
        case (914) ! Β
            ascii_equiv = "Beta"
        case (915) ! Γ
            ascii_equiv = "Gamma"
        case (916) ! Δ
            ascii_equiv = "Delta"
        case (917) ! Ε
            ascii_equiv = "Epsilon"
        case (918) ! Ζ
            ascii_equiv = "Zeta"
        case (919) ! Η
            ascii_equiv = "Eta"
        case (920) ! Θ
            ascii_equiv = "Theta"
        case (921) ! Ι
            ascii_equiv = "Iota"
        case (922) ! Κ
            ascii_equiv = "Kappa"
        case (923) ! Λ
            ascii_equiv = "Lambda"
        case (924) ! Μ
            ascii_equiv = "Mu"
        case (925) ! Ν
            ascii_equiv = "Nu"
        case (926) ! Ξ
            ascii_equiv = "Xi"
        case (927) ! Ο
            ascii_equiv = "Omicron"
        case (928) ! Π
            ascii_equiv = "Pi"
        case (929) ! Ρ
            ascii_equiv = "Rho"
        case (931) ! Σ
            ascii_equiv = "Sigma"
        case (932) ! Τ
            ascii_equiv = "Tau"
        case (933) ! Υ
            ascii_equiv = "Upsilon"
        case (934) ! Φ
            ascii_equiv = "Phi"
        case (935) ! Χ
            ascii_equiv = "Chi"
        case (936) ! Ψ
            ascii_equiv = "Psi"
        case (937) ! Ω
            ascii_equiv = "Omega"
        case default
            ! For other Unicode characters, use a placeholder
            write(ascii_equiv, '("U+", Z4.4)') codepoint
        end select
    end subroutine unicode_codepoint_to_ascii

    subroutine raster_draw_text(this, x, y, text)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: px, py
        integer(1) :: r, g, b
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len

        ! Process LaTeX commands to Unicode
        call process_latex_in_text(text, processed_text, processed_len)

        ! Escape Unicode characters for raster rendering
        call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)

        ! Transform coordinates to plot area (like matplotlib)
        ! Note: Raster Y=0 at top, so we need to flip Y coordinates
        px = (x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)

        call this%raster%get_color_bytes(r, g, b)
        call render_text_to_image(this%raster%image_data, this%width, this%height, &
                                 int(px), int(py), trim(escaped_text), r, g, b)
    end subroutine raster_draw_text

    subroutine raster_save_dummy(this, filename)
        !! Dummy save method - raster context doesn't save files directly
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        ! This is a dummy implementation - concrete backends like PNG will override this
        call log_error("raster_context cannot save files directly. Use a concrete backend like PNG.")
        ! Return instead of stopping - this allows graceful error handling
        return
    end subroutine raster_save_dummy

    subroutine raster_draw_marker(this, x, y, style)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp) :: px, py
        integer(1) :: r, g, b

        ! Transform coordinates to plot area
        px = (x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)

        call this%raster%get_color_bytes(r, g, b)

        call draw_raster_marker_by_style(this, px, py, style)
    end subroutine raster_draw_marker

    subroutine draw_raster_marker_by_style(this, px, py, style)
        !! Draw marker using shared style dispatch logic (DRY compliance)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: px, py
        character(len=*), intent(in) :: style
        real(wp) :: marker_size
        
        marker_size = get_marker_size(style)
        
        select case (trim(style))
        case (MARKER_CIRCLE)
            call draw_circle_with_edge_face(this%raster%image_data, this%width, this%height, px, py, marker_size, &
                                           this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b, &
                                           this%raster%marker_edge_alpha, this%raster%marker_face_r, this%raster%marker_face_g, &
                                           this%raster%marker_face_b, this%raster%marker_face_alpha)
        case (MARKER_SQUARE)
            call draw_square_with_edge_face(this%raster%image_data, this%width, this%height, px, py, marker_size, &
                                           this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b, &
                                           this%raster%marker_edge_alpha, this%raster%marker_face_r, this%raster%marker_face_g, &
                                           this%raster%marker_face_b, this%raster%marker_face_alpha)
        case (MARKER_DIAMOND)
            call draw_diamond_with_edge_face(this%raster%image_data, this%width, this%height, px, py, marker_size, &
                                            this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b, &
                                            this%raster%marker_edge_alpha, this%raster%marker_face_r, this%raster%marker_face_g, &
                                            this%raster%marker_face_b, this%raster%marker_face_alpha)
        case (MARKER_CROSS)
            call draw_x_marker(this%raster%image_data, this%width, this%height, px, py, marker_size, &
                               this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b)
        end select
    end subroutine draw_raster_marker_by_style

    subroutine raster_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, face_g, face_b)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b

        this%raster%marker_edge_r = edge_r
        this%raster%marker_edge_g = edge_g
        this%raster%marker_edge_b = edge_b
        this%raster%marker_edge_alpha = 1.0_wp  ! Default to opaque
        this%raster%marker_face_r = face_r
        this%raster%marker_face_g = face_g
        this%raster%marker_face_b = face_b
        this%raster%marker_face_alpha = 1.0_wp  ! Default to opaque
    end subroutine raster_set_marker_colors

    subroutine raster_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, edge_alpha, &
                                                   face_r, face_g, face_b, face_alpha)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha

        this%raster%marker_edge_r = edge_r
        this%raster%marker_edge_g = edge_g
        this%raster%marker_edge_b = edge_b
        this%raster%marker_edge_alpha = edge_alpha
        this%raster%marker_face_r = face_r
        this%raster%marker_face_g = face_g
        this%raster%marker_face_b = face_b
        this%raster%marker_face_alpha = face_alpha
    end subroutine raster_set_marker_colors_with_alpha

    subroutine raster_fill_quad(this, x_quad, y_quad)
        !! Fill quadrilateral with current color
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        
        real(wp) :: px_quad(4), py_quad(4)
        integer :: i
        
        ! Transform data coordinates to pixel coordinates (same as raster_draw_line)
        ! This ensures the quad respects plot area margins
        do i = 1, 4
            px_quad(i) = (x_quad(i) - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + &
                        real(this%plot_area%left, wp)
            py_quad(i) = real(this%plot_area%bottom + this%plot_area%height, wp) - &
                        (y_quad(i) - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)
        end do
        
        call draw_filled_quad_raster(this%raster%image_data, this%width, this%height, &
                                    px_quad, py_quad, &
                                    this%raster%current_r, this%raster%current_g, this%raster%current_b)
    end subroutine raster_fill_quad

    subroutine draw_circle_with_edge_face(image_data, img_w, img_h, cx, cy, radius, &
                                          edge_r, edge_g, edge_b, edge_alpha, face_r, face_g, face_b, face_alpha)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, radius
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        integer(1) :: edge_r_byte, edge_g_byte, edge_b_byte
        integer(1) :: face_r_byte, face_g_byte, face_b_byte
        integer :: x, y, x_min, x_max, y_min, y_max
        real(wp) :: distance, antialiasing_face_alpha, antialiasing_edge_alpha
        real(wp), parameter :: EDGE_WIDTH = 1.0_wp
        real(wp), parameter :: EDGE_SMOOTHING = 1.0_wp

        ! Convert colors to bytes using safe conversion
        edge_r_byte = color_to_byte(edge_r)
        edge_g_byte = color_to_byte(edge_g)
        edge_b_byte = color_to_byte(edge_b)
        face_r_byte = color_to_byte(face_r)
        face_g_byte = color_to_byte(face_g)
        face_b_byte = color_to_byte(face_b)

        x_min = max(1, int(cx - radius - EDGE_SMOOTHING))
        x_max = min(img_w, int(cx + radius + EDGE_SMOOTHING))
        y_min = max(1, int(cy - radius - EDGE_SMOOTHING))
        y_max = min(img_h, int(cy + radius + EDGE_SMOOTHING))

        do y = y_min, y_max
            do x = x_min, x_max
                distance = sqrt((real(x, wp) - cx)**2 + (real(y, wp) - cy)**2)
                
                ! Calculate antialiasing alpha for face (fill)
                if (distance <= radius - EDGE_WIDTH - EDGE_SMOOTHING) then
                    antialiasing_face_alpha = 1.0_wp
                else if (distance <= radius - EDGE_WIDTH + EDGE_SMOOTHING) then
                    antialiasing_face_alpha = 1.0_wp - (distance - (radius - EDGE_WIDTH - EDGE_SMOOTHING)) &
                                             / (2.0_wp * EDGE_SMOOTHING)
                    antialiasing_face_alpha = max(0.0_wp, min(1.0_wp, antialiasing_face_alpha))
                else
                    antialiasing_face_alpha = 0.0_wp
                end if
                
                ! Calculate antialiasing alpha for edge (outline)
                if (distance >= radius - EDGE_WIDTH - EDGE_SMOOTHING .and. &
                    distance <= radius + EDGE_SMOOTHING) then
                    if (distance <= radius - EDGE_SMOOTHING) then
                        antialiasing_edge_alpha = 1.0_wp
                    else
                        antialiasing_edge_alpha = 1.0_wp - (distance - (radius - EDGE_SMOOTHING)) &
                                             / (2.0_wp * EDGE_SMOOTHING)
                        antialiasing_edge_alpha = max(0.0_wp, min(1.0_wp, antialiasing_edge_alpha))
                    end if
                else
                    antialiasing_edge_alpha = 0.0_wp
                end if
                
                ! Combine user alpha with antialiasing alpha and draw face first, then edge on top
                if (antialiasing_face_alpha > 0.0_wp) then
                    call blend_pixel(image_data, img_w, img_h, x, y, face_alpha * antialiasing_face_alpha, &
                                     face_r_byte, face_g_byte, face_b_byte)
                end if
                if (antialiasing_edge_alpha > 0.0_wp) then
                    call blend_pixel(image_data, img_w, img_h, x, y, edge_alpha * antialiasing_edge_alpha, &
                                     edge_r_byte, edge_g_byte, edge_b_byte)
                end if
            end do
        end do
    end subroutine draw_circle_with_edge_face

    subroutine draw_circle_antialiased(image_data, img_w, img_h, cx, cy, radius, r, g, b)
        !! Convenience wrapper for single-color antialiased circles
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, radius
        integer(1), intent(in) :: r, g, b
        real(wp) :: color_r, color_g, color_b
        
        ! Convert bytes to normalized colors
        color_r = real(r, wp) / 255.0_wp
        color_g = real(g, wp) / 255.0_wp  
        color_b = real(b, wp) / 255.0_wp
        
        ! Use same color for both edge and face
        call draw_circle_with_edge_face(image_data, img_w, img_h, cx, cy, radius, &
                                        color_r, color_g, color_b, 1.0_wp, color_r, color_g, color_b, 1.0_wp)
    end subroutine draw_circle_antialiased

    subroutine draw_circle_outline_antialiased(image_data, img_w, img_h, cx, cy, radius, r, g, b)
        !! Draw antialiased circle outline that blends properly with inside/outside
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, radius
        integer(1), intent(in) :: r, g, b
        integer :: x, y, x_min, x_max, y_min, y_max
        real(wp) :: distance, alpha
        real(wp), parameter :: LINE_WIDTH = 1.0_wp
        
        x_min = max(1, int(cx - radius - 1.0_wp))
        x_max = min(img_w, int(cx + radius + 1.0_wp))
        y_min = max(1, int(cy - radius - 1.0_wp))
        y_max = min(img_h, int(cy + radius + 1.0_wp))
        
        do y = y_min, y_max
            do x = x_min, x_max
                distance = sqrt((real(x, wp) - cx)**2 + (real(y, wp) - cy)**2)
                
                ! Draw outline at the circle boundary
                if (abs(distance - radius) <= LINE_WIDTH) then
                    alpha = 1.0_wp - abs(distance - radius) / LINE_WIDTH
                    alpha = max(0.0_wp, min(1.0_wp, alpha))
                    if (alpha > 0.0_wp) then
                        ! Blend outline color directly - the antialiasing effect comes from
                        ! the alpha blending with the existing background (fill or canvas)
                        call blend_pixel(image_data, img_w, img_h, x, y, alpha, r, g, b)
                    end if
                end if
            end do
        end do
    end subroutine draw_circle_outline_antialiased

    subroutine draw_square_with_edge_face(image_data, img_w, img_h, cx, cy, size, &
                                          edge_r, edge_g, edge_b, edge_alpha, face_r, face_g, face_b, face_alpha)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, size
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        integer(1) :: edge_r_byte, edge_g_byte, edge_b_byte
        integer(1) :: face_r_byte, face_g_byte, face_b_byte
        integer :: x, y, x_min, x_max, y_min, y_max
        real(wp) :: half_size, antialiasing_face_alpha, antialiasing_edge_alpha
        real(wp), parameter :: EDGE_WIDTH = 1.0_wp
        real(wp), parameter :: EDGE_SMOOTHING = 1.0_wp

        ! Convert colors to bytes using safe conversion
        edge_r_byte = color_to_byte(edge_r)
        edge_g_byte = color_to_byte(edge_g)
        edge_b_byte = color_to_byte(edge_b)
        face_r_byte = color_to_byte(face_r)
        face_g_byte = color_to_byte(face_g)
        face_b_byte = color_to_byte(face_b)

        half_size = size * 0.5_wp
        x_min = max(1, int(cx - half_size - EDGE_SMOOTHING))
        x_max = min(img_w, int(cx + half_size + EDGE_SMOOTHING))
        y_min = max(1, int(cy - half_size - EDGE_SMOOTHING))
        y_max = min(img_h, int(cy + half_size + EDGE_SMOOTHING))

        do y = y_min, y_max
            do x = x_min, x_max
                ! Calculate distance from square boundary
                call calculate_square_distance(real(x, wp), real(y, wp), cx, cy, half_size, &
                                               EDGE_WIDTH, EDGE_SMOOTHING, antialiasing_face_alpha, antialiasing_edge_alpha)
                
                ! Draw face first, then edge on top
                if (antialiasing_face_alpha > 0.0_wp) then
                    call blend_pixel(image_data, img_w, img_h, x, y, face_alpha * antialiasing_face_alpha, &
                                     face_r_byte, face_g_byte, face_b_byte)
                end if
                if (antialiasing_edge_alpha > 0.0_wp) then
                    call blend_pixel(image_data, img_w, img_h, x, y, edge_alpha * antialiasing_edge_alpha, &
                                     edge_r_byte, edge_g_byte, edge_b_byte)
                end if
            end do
        end do
    end subroutine draw_square_with_edge_face

    subroutine calculate_square_distance(px, py, cx, cy, half_size, edge_width, edge_smoothing, &
                                          antialiasing_face_alpha, antialiasing_edge_alpha)
        real(wp), intent(in) :: px, py, cx, cy, half_size, edge_width, edge_smoothing
        real(wp), intent(out) :: antialiasing_face_alpha, antialiasing_edge_alpha
        real(wp) :: dx, dy, dist_to_edge

        dx = abs(px - cx)
        dy = abs(py - cy)
        dist_to_edge = max(dx, dy)  ! Distance to square boundary using max norm

        ! Calculate face (fill) alpha
        if (dist_to_edge <= half_size - edge_width - edge_smoothing) then
            antialiasing_face_alpha = 1.0_wp
        else if (dist_to_edge <= half_size - edge_width + edge_smoothing) then
            antialiasing_face_alpha = 1.0_wp - (dist_to_edge - (half_size - edge_width - edge_smoothing)) &
                                     / (2.0_wp * edge_smoothing)
            antialiasing_face_alpha = max(0.0_wp, min(1.0_wp, antialiasing_face_alpha))
        else
            antialiasing_face_alpha = 0.0_wp
        end if

        ! Calculate edge (outline) alpha
        if (dist_to_edge >= half_size - edge_width - edge_smoothing .and. dist_to_edge <= half_size + edge_smoothing) then
            if (dist_to_edge <= half_size - edge_smoothing) then
                antialiasing_edge_alpha = 1.0_wp
            else
                antialiasing_edge_alpha = 1.0_wp - (dist_to_edge - (half_size - edge_smoothing)) / (2.0_wp * edge_smoothing)
                antialiasing_edge_alpha = max(0.0_wp, min(1.0_wp, antialiasing_edge_alpha))
            end if
        else
            antialiasing_edge_alpha = 0.0_wp
        end if
    end subroutine calculate_square_distance

    subroutine draw_diamond_with_edge_face(image_data, img_w, img_h, cx, cy, size, &
                                           edge_r, edge_g, edge_b, edge_alpha, face_r, face_g, face_b, face_alpha)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, size
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        integer(1) :: edge_r_byte, edge_g_byte, edge_b_byte
        integer(1) :: face_r_byte, face_g_byte, face_b_byte
        integer :: x, y, x_min, x_max, y_min, y_max
        real(wp) :: half_size, antialiasing_face_alpha, antialiasing_edge_alpha
        real(wp), parameter :: EDGE_WIDTH = 1.0_wp
        real(wp), parameter :: EDGE_SMOOTHING = 1.0_wp

        ! Convert colors to bytes using safe conversion
        edge_r_byte = color_to_byte(edge_r)
        edge_g_byte = color_to_byte(edge_g)
        edge_b_byte = color_to_byte(edge_b)
        face_r_byte = color_to_byte(face_r)
        face_g_byte = color_to_byte(face_g)
        face_b_byte = color_to_byte(face_b)

        half_size = size * 0.5_wp
        x_min = max(1, int(cx - half_size - EDGE_SMOOTHING))
        x_max = min(img_w, int(cx + half_size + EDGE_SMOOTHING))
        y_min = max(1, int(cy - half_size - EDGE_SMOOTHING))
        y_max = min(img_h, int(cy + half_size + EDGE_SMOOTHING))

        do y = y_min, y_max
            do x = x_min, x_max
                ! Calculate distance from diamond boundary
                call calculate_diamond_distance(real(x, wp), real(y, wp), cx, cy, half_size, &
                                                EDGE_WIDTH, EDGE_SMOOTHING, antialiasing_face_alpha, antialiasing_edge_alpha)
                
                ! Draw face first, then edge on top
                if (antialiasing_face_alpha > 0.0_wp) then
                    call blend_pixel(image_data, img_w, img_h, x, y, face_alpha * antialiasing_face_alpha, &
                                     face_r_byte, face_g_byte, face_b_byte)
                end if
                if (antialiasing_edge_alpha > 0.0_wp) then
                    call blend_pixel(image_data, img_w, img_h, x, y, edge_alpha * antialiasing_edge_alpha, &
                                     edge_r_byte, edge_g_byte, edge_b_byte)
                end if
            end do
        end do
    end subroutine draw_diamond_with_edge_face

    subroutine calculate_diamond_distance(px, py, cx, cy, half_size, edge_width, edge_smoothing, &
                                           antialiasing_face_alpha, antialiasing_edge_alpha)
        real(wp), intent(in) :: px, py, cx, cy, half_size, edge_width, edge_smoothing
        real(wp), intent(out) :: antialiasing_face_alpha, antialiasing_edge_alpha
        real(wp) :: dx, dy, dist_to_edge

        dx = abs(px - cx)
        dy = abs(py - cy)
        dist_to_edge = dx + dy  ! Distance to diamond boundary using Manhattan distance

        ! Calculate face (fill) alpha
        if (dist_to_edge <= half_size - edge_width - edge_smoothing) then
            antialiasing_face_alpha = 1.0_wp
        else if (dist_to_edge <= half_size - edge_width + edge_smoothing) then
            antialiasing_face_alpha = 1.0_wp - (dist_to_edge - (half_size - edge_width - edge_smoothing)) &
                                     / (2.0_wp * edge_smoothing)
            antialiasing_face_alpha = max(0.0_wp, min(1.0_wp, antialiasing_face_alpha))
        else
            antialiasing_face_alpha = 0.0_wp
        end if

        ! Calculate edge (outline) alpha
        if (dist_to_edge >= half_size - edge_width - edge_smoothing .and. dist_to_edge <= half_size + edge_smoothing) then
            if (dist_to_edge <= half_size - edge_smoothing) then
                antialiasing_edge_alpha = 1.0_wp
            else
                antialiasing_edge_alpha = 1.0_wp - (dist_to_edge - (half_size - edge_smoothing)) / (2.0_wp * edge_smoothing)
                antialiasing_edge_alpha = max(0.0_wp, min(1.0_wp, antialiasing_edge_alpha))
            end if
        else
            antialiasing_edge_alpha = 0.0_wp
        end if
    end subroutine calculate_diamond_distance

    subroutine draw_x_marker(image_data, img_w, img_h, cx, cy, size, edge_r, edge_g, edge_b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, size
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        integer(1) :: edge_r_byte, edge_g_byte, edge_b_byte
        real(wp) :: half_size, line_width
        
        ! Convert colors to bytes using safe conversion
        edge_r_byte = color_to_byte(edge_r)
        edge_g_byte = color_to_byte(edge_g)
        edge_b_byte = color_to_byte(edge_b)

        half_size = size * 0.5_wp
        line_width = 1.0_wp

        ! Draw diagonal lines from corners to form an X
        ! Top-left to bottom-right
        call draw_line_distance_aa(image_data, img_w, img_h, &
                                   cx - half_size, cy - half_size, &
                                   cx + half_size, cy + half_size, &
                                   edge_r_byte, edge_g_byte, edge_b_byte, line_width)
        
        ! Top-right to bottom-left
        call draw_line_distance_aa(image_data, img_w, img_h, &
                                   cx + half_size, cy - half_size, &
                                   cx - half_size, cy + half_size, &
                                   edge_r_byte, edge_g_byte, edge_b_byte, line_width)
    end subroutine draw_x_marker

    subroutine draw_axes_and_labels(ctx, xscale, yscale, symlog_threshold, &
                                   x_min_orig, x_max_orig, y_min_orig, y_max_orig, &
                                   title, xlabel, ylabel, z_min_orig, z_max_orig, is_3d_plot, &
                                   grid_enabled, grid_axis, grid_which, &
                                   grid_alpha, grid_linestyle, grid_color)
        !! Draw plot axes and frame with scale-aware tick generation
        !! FIXED: Now generates tick values first, then positions to ensure proper alignment
        class(raster_context), intent(inout) :: ctx
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in), optional :: x_min_orig, x_max_orig, y_min_orig, y_max_orig
        real(wp), intent(in), optional :: z_min_orig, z_max_orig
        logical, intent(in), optional :: is_3d_plot
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        logical, intent(in), optional :: grid_enabled
        character(len=*), intent(in), optional :: grid_axis, grid_which, grid_linestyle
        real(wp), intent(in), optional :: grid_alpha
        real(wp), intent(in), optional :: grid_color(3)
        
        real(wp) :: x_tick_values(20), y_tick_values(20)
        real(wp) :: x_positions(20), y_positions(20)
        character(len=20) :: x_labels(20), y_labels(20)
        real(wp) :: nice_x_min, nice_x_max, nice_x_step
        real(wp) :: nice_y_min, nice_y_max, nice_y_step
        integer :: num_x_ticks, num_y_ticks, i
        real(wp) :: data_x_min, data_x_max, data_y_min, data_y_max
        real(wp) :: symlog_thresh
        character(len=10) :: x_scale_type, y_scale_type

        ! Set color to black for axes
        call ctx%raster%set_color(0.0_wp, 0.0_wp, 0.0_wp)
        
        ! For 3D plots, draw 3D axes instead of 2D frame
        if (present(is_3d_plot) .and. is_3d_plot .and. &
            present(z_min_orig) .and. present(z_max_orig)) then
            ! Use provided data ranges
            if (present(x_min_orig) .and. present(x_max_orig)) then
                data_x_min = x_min_orig
                data_x_max = x_max_orig
            else
                data_x_min = ctx%x_min
                data_x_max = ctx%x_max
            end if
            
            if (present(y_min_orig) .and. present(y_max_orig)) then
                data_y_min = y_min_orig
                data_y_max = y_max_orig
            else
                data_y_min = ctx%y_min
                data_y_max = ctx%y_max
            end if
            
            call draw_3d_axes_frame(ctx, data_x_min, data_x_max, &
                                   data_y_min, data_y_max, z_min_orig, z_max_orig)
            return
        end if

        ! Use provided data ranges or backend ranges
        if (present(x_min_orig) .and. present(x_max_orig)) then
            data_x_min = x_min_orig
            data_x_max = x_max_orig
        else
            data_x_min = ctx%x_min
            data_x_max = ctx%x_max
        end if
        
        if (present(y_min_orig) .and. present(y_max_orig)) then
            data_y_min = y_min_orig
            data_y_max = y_max_orig
        else
            data_y_min = ctx%y_min
            data_y_max = ctx%y_max
        end if

        ! Draw plot frame first
        call draw_raster_frame(ctx)

        ! Generate nice tick VALUES (not positions!) based on scale type
        if (present(xscale) .and. present(yscale) .and. (trim(xscale) /= 'linear' .or. trim(yscale) /= 'linear')) then
            ! For non-linear scales, use scale-aware coordinate transformation
            call find_nice_tick_locations(data_x_min, data_x_max, 5, &
                                        nice_x_min, nice_x_max, nice_x_step, &
                                        x_tick_values, num_x_ticks)
            
            call find_nice_tick_locations(data_y_min, data_y_max, 5, &
                                        nice_y_min, nice_y_max, nice_y_step, &
                                        y_tick_values, num_y_ticks)
            
            ! Apply scale transformation to boundaries and tick values
            ! Handle optional parameters with defaults
            symlog_thresh = 1.0_wp
            if (present(symlog_threshold)) symlog_thresh = symlog_threshold
            
            x_scale_type = 'linear'
            if (present(xscale)) x_scale_type = trim(xscale)
            
            y_scale_type = 'linear'
            if (present(yscale)) y_scale_type = trim(yscale)
            
            ctx%x_min = apply_scale_transform(x_tick_values(1), x_scale_type, symlog_thresh)
            ctx%x_max = apply_scale_transform(x_tick_values(num_x_ticks), x_scale_type, symlog_thresh)
            
            ctx%y_min = apply_scale_transform(y_tick_values(1), y_scale_type, symlog_thresh)
            ctx%y_max = apply_scale_transform(y_tick_values(num_y_ticks), y_scale_type, symlog_thresh)
            
            ! Convert tick values to pixel positions using scale-aware transformation
            do i = 1, num_x_ticks
                x_positions(i) = real(ctx%plot_area%left, wp) + &
                                (apply_scale_transform(x_tick_values(i), x_scale_type, symlog_thresh) - ctx%x_min) / &
                                (ctx%x_max - ctx%x_min) * real(ctx%plot_area%width, wp)
            end do
            
            ! For Y axis, account for flipped coordinates in raster
            do i = 1, num_y_ticks
                y_positions(i) = real(ctx%plot_area%bottom + ctx%plot_area%height, wp) - &
                                (apply_scale_transform(y_tick_values(i), y_scale_type, symlog_thresh) - ctx%y_min) / &
                                (ctx%y_max - ctx%y_min) * real(ctx%plot_area%height, wp)
            end do
        else
            ! For linear scale, use nice tick locations and adjust boundaries to match
            call find_nice_tick_locations(data_x_min, data_x_max, 5, &
                                        nice_x_min, nice_x_max, nice_x_step, &
                                        x_tick_values, num_x_ticks)
            
            call find_nice_tick_locations(data_y_min, data_y_max, 5, &
                                        nice_y_min, nice_y_max, nice_y_step, &
                                        y_tick_values, num_y_ticks)
            
            ! Update the context boundaries to match the nice tick boundaries
            ! This ensures ticks align perfectly with plot edges
            if (num_x_ticks > 0) then
                ctx%x_min = x_tick_values(1)
                ctx%x_max = x_tick_values(num_x_ticks)
            end if
            
            if (num_y_ticks > 0) then
                ctx%y_min = y_tick_values(1)
                ctx%y_max = y_tick_values(num_y_ticks)
            end if
            
            ! Convert tick values to pixel positions using the updated boundaries
            do i = 1, num_x_ticks
                x_positions(i) = real(ctx%plot_area%left, wp) + &
                                (x_tick_values(i) - ctx%x_min) / (ctx%x_max - ctx%x_min) * &
                                real(ctx%plot_area%width, wp)
            end do
            
            ! For Y axis, account for flipped coordinates in raster
            do i = 1, num_y_ticks
                y_positions(i) = real(ctx%plot_area%bottom + ctx%plot_area%height, wp) - &
                                (y_tick_values(i) - ctx%y_min) / (ctx%y_max - ctx%y_min) * &
                                real(ctx%plot_area%height, wp)
            end do
        end if

        ! Generate tick labels based on actual tick values, not data range
        if (present(xscale) .and. trim(xscale) /= 'linear') then
            ! For non-linear scales, use the original approach
            call generate_scale_aware_tick_labels(data_x_min, data_x_max, num_x_ticks, x_labels, xscale, symlog_threshold)
        else
            ! For linear scale, format the actual tick values
            do i = 1, num_x_ticks
                x_labels(i) = format_tick_value_smart(x_tick_values(i), 8)
            end do
        end if
        
        if (present(yscale) .and. trim(yscale) /= 'linear') then
            ! For non-linear scales, use the original approach
            call generate_scale_aware_tick_labels(data_y_min, data_y_max, num_y_ticks, y_labels, yscale, symlog_threshold)
        else
            ! For linear scale, format the actual tick values
            do i = 1, num_y_ticks
                y_labels(i) = format_tick_value_smart(y_tick_values(i), 8)
            end do
        end if
        
        ! Draw tick marks and labels
        call draw_raster_tick_marks(ctx, x_positions, y_positions, num_x_ticks, num_y_ticks)
        call draw_raster_tick_labels(ctx, x_positions, y_positions, x_labels, y_labels, num_x_ticks, num_y_ticks)

        ! Draw title and X-axis label first (they don't conflict with tick labels)
        call draw_raster_title_and_xlabel(ctx, title, xlabel)
        
        ! Draw Y-axis label LAST to avoid being overwritten by tick labels
        if (present(ylabel)) then
            call draw_rotated_ylabel_raster(ctx, ylabel)
        end if
        
        ! Draw grid lines if enabled
        ! FIXED: Check if grid_enabled is present AND true to avoid accessing uninitialized memory
        if (present(grid_enabled)) then
            if (grid_enabled) then
                call draw_raster_grid_lines(ctx, x_positions, y_positions, num_x_ticks, num_y_ticks, &
                                          grid_axis, grid_which, grid_alpha, grid_linestyle, grid_color)
            end if
        end if
    end subroutine draw_axes_and_labels

    subroutine draw_3d_axes_frame(ctx, x_min, x_max, y_min, y_max, z_min, z_max)
        !! Draw 3D axes frame for 3D plots
        use fortplot_3d_axes, only: draw_3d_axes_to_raster
        class(raster_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        call draw_3d_axes_to_raster(ctx, x_min, x_max, y_min, y_max, z_min, z_max)
    end subroutine draw_3d_axes_frame
    subroutine draw_raster_grid_lines(ctx, x_positions, y_positions, num_x_ticks, num_y_ticks, &
                                     grid_axis, grid_which, grid_alpha, grid_linestyle, grid_color)
        !! Draw grid lines at tick positions
        class(raster_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        integer, intent(in) :: num_x_ticks, num_y_ticks
        character(len=*), intent(in), optional :: grid_axis, grid_which, grid_linestyle
        real(wp), intent(in), optional :: grid_alpha
        real(wp), intent(in), optional :: grid_color(3)
        
        character(len=10) :: axis_choice, which_choice
        real(wp) :: alpha_value, line_color(3)
        integer :: i
        real(wp) :: grid_y_top, grid_y_bottom, grid_x_left, grid_x_right
        
        ! Set default values
        axis_choice = 'both'
        which_choice = 'major'
        alpha_value = 0.3_wp
        line_color = [0.5_wp, 0.5_wp, 0.5_wp]
        
        if (present(grid_axis)) axis_choice = grid_axis
        if (present(grid_which)) which_choice = grid_which
        if (present(grid_alpha)) alpha_value = grid_alpha
        if (present(grid_color)) line_color = grid_color
        
        ! Set grid line color with transparency
        call ctx%raster%set_color(line_color(1), line_color(2), line_color(3))
        
        ! Calculate plot area boundaries
        grid_y_top = real(ctx%plot_area%bottom + ctx%plot_area%height, wp)
        grid_y_bottom = real(ctx%plot_area%bottom, wp)
        grid_x_left = real(ctx%plot_area%left, wp)
        grid_x_right = real(ctx%plot_area%left + ctx%plot_area%width, wp)
        
        ! Draw vertical grid lines (at x tick positions)
        if (axis_choice == 'both' .or. axis_choice == 'x') then
            do i = 1, min(num_x_ticks, size(x_positions))
                call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                          x_positions(i), grid_y_bottom, &
                                          x_positions(i), grid_y_top, &
                                          color_to_byte(line_color(1)), &
                                          color_to_byte(line_color(2)), &
                                          color_to_byte(line_color(3)), &
                                          alpha_value)
            end do
        end if
        
        ! Draw horizontal grid lines (at y tick positions)
        if (axis_choice == 'both' .or. axis_choice == 'y') then
            do i = 1, min(num_y_ticks, size(y_positions))
                call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                          grid_x_left, y_positions(i), &
                                          grid_x_right, y_positions(i), &
                                          color_to_byte(line_color(1)), &
                                          color_to_byte(line_color(2)), &
                                          color_to_byte(line_color(3)), &
                                          alpha_value)
            end do
        end if
    end subroutine draw_raster_grid_lines

    subroutine draw_raster_frame(ctx)
        !! Draw the plot frame for raster backend
        class(raster_context), intent(inout) :: ctx

        ! Draw plot frame (raster coordinates with Y=0 at top)
        call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left, wp), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  real(ctx%plot_area%left + ctx%plot_area%width, wp), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  0_1, 0_1, 0_1, 0.1_wp)  ! Bottom edge (top in raster)

        call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left, wp), &
                                  real(ctx%plot_area%bottom, wp), &
                                  real(ctx%plot_area%left, wp), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  0_1, 0_1, 0_1, 0.1_wp)  ! Left edge

        call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left + ctx%plot_area%width, wp), &
                                  real(ctx%plot_area%bottom, wp), &
                                  real(ctx%plot_area%left + ctx%plot_area%width, wp), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  0_1, 0_1, 0_1, 0.1_wp)  ! Right edge

        call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left, wp), &
                                  real(ctx%plot_area%bottom, wp), &
                                  real(ctx%plot_area%left + ctx%plot_area%width, wp), &
                                  real(ctx%plot_area%bottom, wp), &
                                  0_1, 0_1, 0_1, 0.1_wp)  ! Top edge (bottom in raster)
    end subroutine draw_raster_frame

    subroutine draw_raster_tick_marks(ctx, x_positions, y_positions, num_x, num_y)
        !! Draw tick marks for raster backend
        class(raster_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        integer, intent(in) :: num_x, num_y
        integer :: i

        ! Draw X-axis tick marks (at bottom of plot - top in raster coords)
        do i = 1, num_x
            call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                      x_positions(i), &
                                      real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                      x_positions(i), &
                                      real(ctx%plot_area%bottom + ctx%plot_area%height + 5, wp), &
                                      0_1, 0_1, 0_1, 0.1_wp)
        end do

        ! Draw Y-axis tick marks (at left of plot)
        do i = 1, num_y
            call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                      real(ctx%plot_area%left, wp), y_positions(i), &
                                      real(ctx%plot_area%left - 5, wp), y_positions(i), &
                                      0_1, 0_1, 0_1, 0.1_wp)
        end do
    end subroutine draw_raster_tick_marks

    subroutine draw_raster_tick_labels(ctx, x_positions, y_positions, x_labels, y_labels, num_x, num_y)
        !! Draw tick labels for raster backend with boundary checking
        class(raster_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        character(len=*), intent(in) :: x_labels(:), y_labels(:)
        integer, intent(in) :: num_x, num_y
        integer :: i, text_width
        real(wp) :: label_x, label_y
        real(wp) :: min_x, max_x

        ! Calculate drawable bounds with small margin
        min_x = 5.0_wp  ! Left margin
        max_x = real(ctx%width - 5, wp)  ! Right margin

        ! Draw X-axis tick labels with boundary checking
        do i = 1, num_x
            call calculate_x_tick_label_position(x_positions(i), &
                                               real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                               trim(x_labels(i)), label_x, label_y)
            
            ! Check and adjust label position to stay within bounds
            text_width = calculate_text_width(trim(x_labels(i)))
            if (text_width <= 0) text_width = len_trim(x_labels(i)) * 8
            
            ! Adjust position if label extends beyond boundaries
            if (label_x < min_x) then
                label_x = min_x
            else if (label_x + real(text_width, wp) > max_x) then
                label_x = max_x - real(text_width, wp)
            end if
            
            call render_text_to_image(ctx%raster%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(x_labels(i)), &
                                     0_1, 0_1, 0_1)  ! Black text
        end do

        ! Draw Y-axis tick labels with right alignment and proper spacing
        do i = 1, num_y
            call calculate_y_tick_label_position(y_positions(i), real(ctx%plot_area%left, wp), &
                                               trim(y_labels(i)), label_x, label_y)
            call render_text_to_image(ctx%raster%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(y_labels(i)), &
                                     0_1, 0_1, 0_1)  ! Black text
        end do
    end subroutine draw_raster_tick_labels

    subroutine draw_raster_title_and_xlabel(ctx, title, xlabel)
        !! Draw figure title and X-axis label (but not Y-axis label)
        class(raster_context), intent(inout) :: ctx
        character(len=*), intent(in), optional :: title, xlabel
        real(wp) :: label_x, label_y, text_width
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len

        ! Draw title at top center with proper margin (matplotlib-style)
        if (present(title)) then
            ! Process LaTeX commands to Unicode and escape for raster rendering
            call process_latex_in_text(title, processed_text, processed_len)
            call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
            
            ! Center horizontally across the entire figure width (like matplotlib)
            text_width = real(calculate_text_width(trim(escaped_text)), wp)
            if (text_width <= 0.0_wp) then
                text_width = real(len_trim(escaped_text) * 8, wp)  ! 8 pixels per char for 12pt font
            end if
            label_x = real(ctx%width, wp) / 2.0_wp - text_width / 2.0_wp
            ! Position title in the top margin area (matplotlib uses ~25px from top)
            label_y = 25.0_wp
            call render_text_to_image(ctx%raster%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(escaped_text), &
                                     0_1, 0_1, 0_1)  ! Black text, normal weight (non-bold)
        end if

        ! Draw X-axis label using proper axis label positioning
        if (present(xlabel)) then
            ! Process LaTeX commands to Unicode and escape for raster rendering
            call process_latex_in_text(xlabel, processed_text, processed_len)
            call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
            
            call calculate_x_axis_label_position(real(ctx%plot_area%left + ctx%plot_area%width / 2, wp), &
                                               real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                               trim(escaped_text), label_x, label_y)
            call render_text_to_image(ctx%raster%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(escaped_text), &
                                     0_1, 0_1, 0_1)  ! Black text
        end if
    end subroutine draw_raster_title_and_xlabel

    subroutine draw_rotated_ylabel_raster(ctx, text)
        !! Draw Y-axis label by rendering text then rotating the bitmap 90 degrees
        class(raster_context), intent(inout) :: ctx
        character(len=*), intent(in) :: text
        real(wp) :: label_x, label_y
        integer :: text_width, text_height, padding
        integer :: buf_width, buf_height, i, j, src_x, src_y, dst_x, dst_y
        integer(1), allocatable :: text_bitmap(:,:,:), rotated_bitmap(:,:,:)
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len
        
        ! Process LaTeX commands to Unicode and escape for raster rendering
        call process_latex_in_text(text, processed_text, processed_len)
        call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
        
        ! Calculate text dimensions and position
        text_width = calculate_text_width(trim(escaped_text))
        text_height = calculate_text_height(trim(escaped_text))
        
        padding = 2
        buf_width = text_width + 2 * padding  
        buf_height = text_height + 2 * padding
        
        ! Create 2D RGB bitmap for text
        allocate(text_bitmap(buf_width, buf_height, 3))
        text_bitmap = -1_1  ! White background (255 in unsigned byte)
        
        ! Render text to bitmap
        call render_text_to_bitmap(text_bitmap, buf_width, buf_height, &
                                  padding, padding + text_height, trim(escaped_text))
                                  
        ! Rotate bitmap 90 degrees counter-clockwise (text reads bottom to top)
        allocate(rotated_bitmap(buf_height, buf_width, 3))
        call rotate_bitmap_90_ccw(text_bitmap, rotated_bitmap, buf_width, buf_height)
        
        ! Calculate position and composite rotated bitmap directly onto main image
        call calculate_y_axis_label_position(real(ctx%plot_area%bottom + ctx%plot_area%height / 2, wp), &
                                           real(ctx%plot_area%left, wp), text, label_x, label_y)
        
        ! Composite rotated bitmap directly onto main image buffer
        call composite_bitmap_to_raster(ctx%raster%image_data, ctx%width, ctx%height, &
                                       rotated_bitmap, buf_height, buf_width, &
                                       int(label_x - buf_height/2), int(label_y - buf_width/2))
        
        deallocate(text_bitmap, rotated_bitmap)
    end subroutine draw_rotated_ylabel_raster

    subroutine draw_filled_quad_raster(image_data, img_w, img_h, x_quad, y_quad, r, g, b)
        !! Draw filled quadrilateral using dense line sampling (simple and robust)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        real(wp), intent(in) :: r, g, b
        
        integer(1) :: r_byte, g_byte, b_byte
        integer :: i, num_lines
        real(wp) :: t, x1, y1, x2, y2
        
        ! Convert colors to bytes using safe conversion
        r_byte = color_to_byte(r)
        g_byte = color_to_byte(g)
        b_byte = color_to_byte(b)
        
        ! Simple approach: draw many horizontal lines across the quad
        ! This is robust and handles any quad shape
        num_lines = max(10, int(maxval(y_quad) - minval(y_quad)) + 1)
        
        do i = 0, num_lines
            t = real(i, wp) / real(num_lines, wp)
            
            ! Interpolate between left edge (vertices 1->4) and right edge (vertices 2->3)
            x1 = x_quad(1) + t * (x_quad(4) - x_quad(1))  ! Left edge
            y1 = y_quad(1) + t * (y_quad(4) - y_quad(1))
            
            x2 = x_quad(2) + t * (x_quad(3) - x_quad(2))  ! Right edge  
            y2 = y_quad(2) + t * (y_quad(3) - y_quad(2))
            
            ! Draw line between these points
            call draw_line_distance_aa(image_data, img_w, img_h, x1, y1, x2, y2, &
                                      r_byte, g_byte, b_byte, 1.0_wp)
        end do
    end subroutine draw_filled_quad_raster
    
    subroutine fill_triangle(image_data, img_w, img_h, x1, y1, x2, y2, x3, y3, r, g, b)
        !! Fill triangle using barycentric coordinates
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x1, y1, x2, y2, x3, y3
        integer(1), intent(in) :: r, g, b
        
        integer :: x, y, x_min, x_max, y_min, y_max
        real(wp) :: denom, a, b_coord, c
        integer :: pixel_index
        
        ! Find bounding box
        x_min = max(1, int(min(min(x1, x2), x3)))
        x_max = min(img_w, int(max(max(x1, x2), x3)) + 1)
        y_min = max(1, int(min(min(y1, y2), y3)))
        y_max = min(img_h, int(max(max(y1, y2), y3)) + 1)
        
        ! Precompute denominator for barycentric coordinates
        denom = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
        
        if (abs(denom) < 1e-10_wp) return  ! Degenerate triangle
        
        ! Check each pixel in bounding box
        do y = y_min, y_max
            do x = x_min, x_max
                ! Compute barycentric coordinates
                a = ((y2 - y3) * (real(x, wp) - x3) + (x3 - x2) * (real(y, wp) - y3)) / denom
                b_coord = ((y3 - y1) * (real(x, wp) - x3) + (x1 - x3) * (real(y, wp) - y3)) / denom
                c = 1.0_wp - a - b_coord
                
                ! Check if point is inside triangle
                if (a >= 0.0_wp .and. b_coord >= 0.0_wp .and. c >= 0.0_wp) then
                    pixel_index = 3 * ((y - 1) * img_w + (x - 1)) + 1
                    image_data(pixel_index) = r      ! Red
                    image_data(pixel_index + 1) = g  ! Green
                    image_data(pixel_index + 2) = b  ! Blue
                end if
            end do
        end do
    end subroutine fill_triangle

    subroutine fill_horizontal_line(image_data, img_w, img_h, x1, x2, y, r, g, b)
        !! Fill horizontal line segment
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h, x1, x2, y
        integer(1), intent(in) :: r, g, b
        
        integer :: x, x_start, x_end, pixel_index
        
        x_start = max(1, min(x1, x2))
        x_end = min(img_w, max(x1, x2))
        
        if (y >= 1 .and. y <= img_h) then
            do x = x_start, x_end
                pixel_index = 3 * ((y - 1) * img_w + (x - 1)) + 1
                image_data(pixel_index) = r      ! Red
                image_data(pixel_index + 1) = g  ! Green  
                image_data(pixel_index + 2) = b  ! Blue
            end do
        end if
    end subroutine fill_horizontal_line

    subroutine raster_draw_arrow(this, x, y, dx, dy, size, style)
        !! Draw arrow head for streamplot arrows in raster backend
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        
        ! Simple stub implementation to avoid compiler crash
        ! TODO: Implement proper arrow rendering
        
        ! Mark that arrows have been rendered
        this%has_rendered_arrows = .true.
        this%uses_vector_arrows = .false.
        this%has_triangular_arrows = .true.
    end subroutine raster_draw_arrow

    function raster_get_ascii_output(this) result(output)
        !! Get ASCII output (not applicable for raster backend)
        class(raster_context), intent(in) :: this
        character(len=:), allocatable :: output
        
        output = ""  ! Raster backend doesn't produce ASCII output
    end function raster_get_ascii_output

    function raster_get_width_scale(this) result(scale)
        !! Get width scaling factor for coordinate transformation
        class(raster_context), intent(in) :: this
        real(wp) :: scale
        
        ! Calculate scaling from logical to pixel coordinates
        if (this%width > 0 .and. this%x_max > this%x_min) then
            scale = real(this%width, wp) / (this%x_max - this%x_min)
        else
            scale = 1.0_wp
        end if
    end function raster_get_width_scale

    function raster_get_height_scale(this) result(scale)
        !! Get height scaling factor for coordinate transformation  
        class(raster_context), intent(in) :: this
        real(wp) :: scale
        
        ! Calculate scaling from logical to pixel coordinates
        if (this%height > 0 .and. this%y_max > this%y_min) then
            scale = real(this%height, wp) / (this%y_max - this%y_min)
        else
            scale = 1.0_wp
        end if
    end function raster_get_height_scale

    subroutine raster_fill_heatmap(this, x_grid, y_grid, z_grid, z_min, z_max)
        !! Fill heatmap (not supported for raster backend - no-op)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        
        ! Raster backend doesn't support heatmap rendering like ASCII
        ! This is a no-op to satisfy polymorphic interface
    end subroutine raster_fill_heatmap

    subroutine raster_render_legend_specialized(this, legend, legend_x, legend_y)
        !! Render legend using standard algorithm for PNG
        use fortplot_legend, only: legend_t
        class(raster_context), intent(inout) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(in) :: legend_x, legend_y
        
        ! No-op: legend rendering handled by fortplot_legend module
        ! This method exists only for polymorphic compatibility
    end subroutine raster_render_legend_specialized

    subroutine raster_calculate_legend_dimensions(this, legend, legend_width, legend_height)
        !! Calculate standard legend dimensions for PNG
        use fortplot_legend, only: legend_t
        class(raster_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: legend_width, legend_height
        
        ! Use standard dimension calculation for PNG backend
        legend_width = 80.0_wp   ! Standard legend width
        legend_height = real(legend%num_entries * 20 + 10, wp)  ! 20 pixels per entry + margins
    end subroutine raster_calculate_legend_dimensions

    subroutine raster_set_legend_border_width(this)
        !! Set thin border width for PNG legend
        class(raster_context), intent(inout) :: this
        
        call this%set_line_width(0.1_wp)  ! Thin border for PNG like axes
    end subroutine raster_set_legend_border_width

    subroutine raster_calculate_legend_position(this, legend, x, y)
        !! Calculate standard legend position for PNG using plot coordinates
        use fortplot_legend, only: legend_t
        class(raster_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: x, y
        
        ! No-op: position calculation handled by fortplot_legend module
        ! This method exists only for polymorphic compatibility
        x = 0.0_wp
        y = 0.0_wp
    end subroutine raster_calculate_legend_position

    subroutine raster_extract_rgb_data(this, width, height, rgb_data)
        !! Extract RGB data from PNG backend
        use, intrinsic :: iso_fortran_env, only: real64
        class(raster_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(real64), intent(out) :: rgb_data(width, height, 3)
        integer :: x, y, idx_base
        
        do y = 1, height
            do x = 1, width
                ! Calculate 1D index for packed RGB data (width * height * 3 array)
                ! Format: [R1, G1, B1, R2, G2, B2, ...]
                idx_base = ((y-1) * width + (x-1)) * 3
                
                ! Extract RGB values (normalized to 0-1)
                rgb_data(x, y, 1) = real(this%raster%image_data(idx_base + 1), real64) / 255.0_real64
                rgb_data(x, y, 2) = real(this%raster%image_data(idx_base + 2), real64) / 255.0_real64
                rgb_data(x, y, 3) = real(this%raster%image_data(idx_base + 3), real64) / 255.0_real64
            end do
        end do
    end subroutine raster_extract_rgb_data

    subroutine raster_get_png_data(this, width, height, png_data, status)
        !! Raster context doesn't generate PNG data - only PNG context does
        class(raster_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        ! Raster context doesn't generate PNG data
        ! This should be overridden by PNG context
        allocate(png_data(0))
        status = -1
    end subroutine raster_get_png_data

    subroutine raster_prepare_3d_data(this, plots)
        !! Prepare 3D data for PNG backend (no-op - PNG doesn't use 3D data)
        use fortplot_plot_data, only: plot_data_t
        class(raster_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)
        
        ! PNG backend doesn't need 3D data preparation - no-op
    end subroutine raster_prepare_3d_data

    subroutine raster_render_ylabel(this, ylabel)
        !! Render rotated Y-axis label for PNG backend
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel
        
        call draw_rotated_ylabel_raster(this, ylabel)
    end subroutine raster_render_ylabel

    subroutine raster_draw_axes_and_labels(this, xscale, yscale, symlog_threshold, &
                                          x_min, x_max, y_min, y_max, &
                                          title, xlabel, ylabel, &
                                          z_min, z_max, has_3d_plots)
        !! Draw axes and labels for raster backends
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        
        ! For raster/PNG backends, draw matplotlib-style axes with margins
        ! This would typically call the internal axes drawing routine
        ! For now, just draw simple axes as a placeholder
        call this%line(x_min, y_min, x_max, y_min)
        call this%line(x_min, y_min, x_min, y_max)
        
        ! TODO: Add full axes implementation with ticks, labels, etc.
    end subroutine raster_draw_axes_and_labels

    subroutine raster_save_coordinates(this, x_min, x_max, y_min, y_max)
        !! Save current coordinate system
        class(raster_context), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max
        
        x_min = this%x_min
        x_max = this%x_max
        y_min = this%y_min
        y_max = this%y_max
    end subroutine raster_save_coordinates

    subroutine raster_set_coordinates(this, x_min, x_max, y_min, y_max)
        !! Set coordinate system
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        
        this%x_min = x_min
        this%x_max = x_max
        this%y_min = y_min
        this%y_max = y_max
    end subroutine raster_set_coordinates

end module fortplot_raster