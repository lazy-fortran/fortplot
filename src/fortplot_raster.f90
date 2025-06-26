module fortplot_raster
    use iso_c_binding
    use fortplot_context
    use fortplot_text, only: render_text_to_image, calculate_text_width, calculate_text_height
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area, get_axis_tick_positions
    use fortplot_ticks, only: generate_scale_aware_tick_labels
    use fortplot_label_positioning, only: calculate_x_label_position, calculate_y_label_position, &
                                         calculate_x_tick_label_position, calculate_y_tick_label_position, &
                                         calculate_x_axis_label_position, calculate_y_axis_label_position
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_image_t, create_raster_image, destroy_raster_image
    public :: initialize_white_background, color_to_byte
    public :: draw_line_distance_aa, blend_pixel, composite_image, composite_bitmap_to_raster
    public :: distance_point_to_line_segment, ipart, fpart, rfpart
    public :: render_text_to_bitmap, rotate_bitmap_90_cw, rotate_bitmap_90_ccw, bitmap_to_png_buffer
    public :: raster_context, create_raster_canvas, draw_axes_and_labels, draw_rotated_ylabel_raster

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
    end type raster_context

contains

    function create_raster_image(width, height) result(image)
        integer, intent(in) :: width, height
        type(raster_image_t) :: image

        image%width = width
        image%height = height
        allocate(image%image_data(height * (1 + width * 3)))
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
            image_data(k) = 0_1
            k = k + 1
            do j = 1, w
                image_data(k) = -1_1
                image_data(k+1) = -1_1
                image_data(k+2) = -1_1
                k = k + 3
            end do
        end do
    end subroutine initialize_white_background

    function color_to_byte(color_val) result(byte_val)
        real(wp), intent(in) :: color_val
        integer(1) :: byte_val
        integer :: int_val

        int_val = int(color_val * 255.0_wp)
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

        k = (y - 1) * (1 + img_w * 3) + 1 + (x - 1) * 3 + 1

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
                    
                    src_idx = (y - 1) * (1 + overlay_width * 3) + 1 + (x - 1) * 3 + 1
                    dst_idx = (img_y - 1) * (1 + main_width * 3) + 1 + (img_x - 1) * 3 + 1
                    
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
                        
                        raster_idx = (raster_y - 1) * (1 + raster_width * 3) + 1 + (raster_x - 1) * 3 + 1
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
        
        allocate(temp_buffer(height * (1 + width * 3)))
        call initialize_white_background(temp_buffer, width, height)
        call render_text_to_image(temp_buffer, width, height, x, y, text, 0_1, 0_1, 0_1)
        
        ! Convert PNG buffer to bitmap
        do j = 1, height
            do i = 1, width
                buf_idx = (j - 1) * (1 + width * 3) + 1 + (i - 1) * 3 + 1
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
        !! Convert 3D RGB bitmap to PNG buffer format
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: width, height
        integer(1), intent(out) :: buffer(:)
        integer :: i, j, buf_idx
        
        call initialize_white_background(buffer, width, height)
        
        do j = 1, height
            do i = 1, width
                buf_idx = (j - 1) * (1 + width * 3) + 1 + (i - 1) * 3 + 1
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

    subroutine raster_draw_text(this, x, y, text)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: px, py
        integer(1) :: r, g, b

        ! Transform coordinates to plot area (like matplotlib)
        ! Note: Raster Y=0 at top, so we need to flip Y coordinates
        px = (x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)

        call this%raster%get_color_bytes(r, g, b)
        call render_text_to_image(this%raster%image_data, this%width, this%height, &
                                 int(px), int(py), text, r, g, b)
    end subroutine raster_draw_text

    subroutine raster_save_dummy(this, filename)
        !! Dummy save method - raster context doesn't save files directly
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        ! This is a dummy implementation - concrete backends like PNG will override this
        print *, "Error: raster_context cannot save files directly. Use a concrete backend like PNG."
        stop
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

        if (trim(style) == 'o') then
            ! Draw filled circle with thin black outline
            ! First draw the filled circle at full size
            call draw_circle_antialiased(this%raster%image_data, this%width, this%height, px, py, 5.0_wp, &
                                         color_to_byte(this%raster%current_r), &
                                         color_to_byte(this%raster%current_g), &
                                         color_to_byte(this%raster%current_b))
            ! Then draw black outline at same size (will blend over the edge)
            call draw_circle_outline_antialiased(this%raster%image_data, this%width, this%height, px, py, 5.0_wp, &
                                                 0_1, 0_1, 0_1)
        else if (trim(style) == 's') then
            call draw_square_with_edge_face(this%raster%image_data, this%width, this%height, px, py, 6.0_wp, &
                                           this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b, &
                                           this%raster%marker_edge_alpha, this%raster%marker_face_r, this%raster%marker_face_g, &
                                           this%raster%marker_face_b, this%raster%marker_face_alpha)
        else if (trim(style) == 'D') then
            call draw_diamond_with_edge_face(this%raster%image_data, this%width, this%height, px, py, 6.0_wp, &
                                            this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b, &
                                            this%raster%marker_edge_alpha, this%raster%marker_face_r, this%raster%marker_face_g, &
                                            this%raster%marker_face_b, this%raster%marker_face_alpha)
        else if (trim(style) == 'x') then
            call draw_x_marker(this%raster%image_data, this%width, this%height, px, py, 5.0_wp, &
                               this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b)
        end if
    end subroutine raster_draw_marker

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

        ! Convert colors to bytes
        edge_r_byte = int(edge_r * 255.0_wp, 1)
        edge_g_byte = int(edge_g * 255.0_wp, 1)
        edge_b_byte = int(edge_b * 255.0_wp, 1)
        face_r_byte = int(face_r * 255.0_wp, 1)
        face_g_byte = int(face_g * 255.0_wp, 1)
        face_b_byte = int(face_b * 255.0_wp, 1)

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

        ! Convert colors to bytes
        edge_r_byte = int(edge_r * 255.0_wp, 1)
        edge_g_byte = int(edge_g * 255.0_wp, 1)
        edge_b_byte = int(edge_b * 255.0_wp, 1)
        face_r_byte = int(face_r * 255.0_wp, 1)
        face_g_byte = int(face_g * 255.0_wp, 1)
        face_b_byte = int(face_b * 255.0_wp, 1)

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

        ! Convert colors to bytes
        edge_r_byte = int(edge_r * 255.0_wp, 1)
        edge_g_byte = int(edge_g * 255.0_wp, 1)
        edge_b_byte = int(edge_b * 255.0_wp, 1)
        face_r_byte = int(face_r * 255.0_wp, 1)
        face_g_byte = int(face_g * 255.0_wp, 1)
        face_b_byte = int(face_b * 255.0_wp, 1)

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
        
        ! Convert colors to bytes
        edge_r_byte = int(edge_r * 255.0_wp, 1)
        edge_g_byte = int(edge_g * 255.0_wp, 1)
        edge_b_byte = int(edge_b * 255.0_wp, 1)

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
                                   title, xlabel, ylabel)
        !! Draw plot axes and frame with scale-aware tick generation
        class(raster_context), intent(inout) :: ctx
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in), optional :: x_min_orig, x_max_orig, y_min_orig, y_max_orig
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        integer :: i
        real(wp) :: x_positions(10), y_positions(10)
        integer :: num_x, num_y
        character(len=20) :: x_labels(10), y_labels(10)

        ! Set color to black for axes
        call ctx%raster%set_color(0.0_wp, 0.0_wp, 0.0_wp)

        ! Draw plot frame using common functionality
        call draw_raster_frame(ctx)

        ! Draw tick marks and labels with scale-aware generation
        call get_axis_tick_positions(ctx%plot_area, 5, 5, x_positions, y_positions, num_x, num_y)

        ! Use original coordinates for tick generation if provided, otherwise use backend coordinates
        if (present(x_min_orig) .and. present(x_max_orig)) then
            call generate_scale_aware_tick_labels(x_min_orig, x_max_orig, num_x, x_labels, xscale, symlog_threshold)
        else
            call generate_scale_aware_tick_labels(ctx%x_min, ctx%x_max, num_x, x_labels, xscale, symlog_threshold)
        end if

        if (present(y_min_orig) .and. present(y_max_orig)) then
            call generate_scale_aware_tick_labels(y_min_orig, y_max_orig, num_y, y_labels, yscale, symlog_threshold)
        else
            call generate_scale_aware_tick_labels(ctx%y_min, ctx%y_max, num_y, y_labels, yscale, symlog_threshold)
        end if
        call draw_raster_tick_marks(ctx, x_positions, y_positions, num_x, num_y)
        call draw_raster_tick_labels(ctx, x_positions, y_positions, x_labels, y_labels, num_x, num_y)

        ! Draw title and X-axis label first (they don't conflict with tick labels)
        call draw_raster_title_and_xlabel(ctx, title, xlabel)
        
        ! Draw Y-axis label LAST to avoid being overwritten by tick labels
        if (present(ylabel)) then
            call draw_rotated_ylabel_raster(ctx, ylabel)
        end if
    end subroutine draw_axes_and_labels

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
        !! Draw tick labels for raster backend like matplotlib
        class(raster_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        character(len=*), intent(in) :: x_labels(:), y_labels(:)
        integer, intent(in) :: num_x, num_y
        integer :: i
        real(wp) :: label_x, label_y

        ! Draw X-axis tick labels with proper spacing and center alignment
        do i = 1, num_x
            call calculate_x_tick_label_position(x_positions(i), &
                                               real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                               trim(x_labels(i)), label_x, label_y)
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

        ! Draw title at top center with proper margin (matplotlib-style)
        if (present(title)) then
            ! Center horizontally across the entire figure width (like matplotlib)
            text_width = real(calculate_text_width(trim(title)), wp)
            if (text_width <= 0.0_wp) then
                text_width = real(len_trim(title) * 8, wp)  ! 8 pixels per char for 12pt font
            end if
            label_x = real(ctx%width, wp) / 2.0_wp - text_width / 2.0_wp
            ! Position title in the top margin area (matplotlib uses ~25px from top)
            label_y = 25.0_wp
            call render_text_to_image(ctx%raster%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(title), &
                                     0_1, 0_1, 0_1)  ! Black text, normal weight (non-bold)
        end if

        ! Draw X-axis label using proper axis label positioning
        if (present(xlabel)) then
            call calculate_x_axis_label_position(real(ctx%plot_area%left + ctx%plot_area%width / 2, wp), &
                                               real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                               trim(xlabel), label_x, label_y)
            call render_text_to_image(ctx%raster%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(xlabel), &
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
        
        ! Calculate text dimensions and position
        text_width = calculate_text_width(trim(text))
        text_height = calculate_text_height(trim(text))
        
        padding = 2
        buf_width = text_width + 2 * padding  
        buf_height = text_height + 2 * padding
        
        ! Create 2D RGB bitmap for text
        allocate(text_bitmap(buf_width, buf_height, 3))
        text_bitmap = -1_1  ! White background (255 in unsigned byte)
        
        ! Render text to bitmap
        call render_text_to_bitmap(text_bitmap, buf_width, buf_height, &
                                  padding, padding + text_height, trim(text))
                                  
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


end module fortplot_raster