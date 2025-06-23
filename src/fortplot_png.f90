module fortplot_png
    use iso_c_binding
    use fortplot_context
    use fortplot_text
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area, get_axis_tick_positions
    use fortplot_ticks, only: generate_scale_aware_tick_labels
    use fortplot_label_positioning, only: calculate_x_label_position, calculate_y_label_position
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: png_context, create_png_canvas, draw_axes_and_labels
    
    ! PNG plotting context
    type, extends(plot_context) :: png_context
        integer(1), allocatable :: image_data(:)
        real(wp) :: current_r, current_g, current_b
        ! Plot area calculations (using common margin functionality)
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
    contains
        procedure :: line => png_draw_line
        procedure :: color => png_set_color
        procedure :: text => png_draw_text
        procedure :: save => png_finalize
    end type png_context
    
contains

    function create_png_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(png_context) :: ctx
        
        call setup_canvas(ctx, width, height)
        
        allocate(ctx%image_data(height * (1 + width * 3)))
        call initialize_white_background(ctx%image_data, width, height)
        
        ctx%current_r = 0.0_wp
        ctx%current_g = 0.0_wp
        ctx%current_b = 1.0_wp
        
        ! Set up matplotlib-style margins using common module
        ctx%margins = plot_margins_t()  ! Use defaults
        call calculate_plot_area(width, height, ctx%margins, ctx%plot_area)
    end function create_png_canvas
    
    subroutine png_draw_line(this, x1, y1, x2, y2)
        class(png_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: px1, py1, px2, py2
        integer(1) :: r, g, b
        
        ! Transform coordinates to plot area (like matplotlib)
        ! Note: PNG Y=0 at top, so we need to flip Y coordinates
        px1 = (x1 - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py1 = real(this%plot_area%bottom + this%plot_area%height, wp) - &
              (y1 - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)
        px2 = (x2 - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py2 = real(this%plot_area%bottom + this%plot_area%height, wp) - &
              (y2 - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)
        
        r = color_to_byte(this%current_r)
        g = color_to_byte(this%current_g)
        b = color_to_byte(this%current_b)
        
        call draw_line_distance_aa(this%image_data, this%width, this%height, px1, py1, px2, py2, r, g, b)
    end subroutine png_draw_line
    
    subroutine png_set_color(this, r, g, b)
        class(png_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        
        this%current_r = r
        this%current_g = g
        this%current_b = b
    end subroutine png_set_color
    
    subroutine png_draw_text(this, x, y, text)
        class(png_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: px, py
        
        ! Transform coordinates to plot area (like matplotlib)
        ! Note: PNG Y=0 at top, so we need to flip Y coordinates
        px = (x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)
        
        call render_text_to_image(this%image_data, this%width, this%height, &
                                 int(px), int(py), text, &
                                 color_to_byte(this%current_r), &
                                 color_to_byte(this%current_g), &
                                 color_to_byte(this%current_b))
    end subroutine png_draw_text
    
    subroutine png_finalize(this, filename)
        class(png_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        call write_png_file(filename, this%width, this%height, this%image_data)
    end subroutine png_finalize
    
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
    
    ! Include necessary functions from png_module
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
    
    ! Distance-based antialiasing algorithm (AGG-style)
    subroutine draw_line_distance_aa(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x0, y0, x1, y1
        integer(1), intent(in) :: r, g, b
        real(wp), parameter :: line_width = 0.75_wp
        
        real(wp) :: dx, dy, length, nx, ny
        integer :: x_min, x_max, y_min, y_max, x, y
        real(wp) :: px, py, dist, alpha
        
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
    
    ! Calculate distance from point to line segment
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

    ! Adaptive line drawing algorithm 
    subroutine draw_line_adaptive(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x0, y0, x1, y1
        integer(1), intent(in) :: r, g, b
        real(wp) :: dx, dy, length
        integer :: subdivisions, i
        real(wp) :: step_x, step_y, x, y, next_x, next_y
        
        dx = x1 - x0
        dy = y1 - y0
        length = sqrt(dx*dx + dy*dy)
        
        if (length > 3.0_wp) then
            subdivisions = int(length / 2.0_wp) + 1
        else
            subdivisions = 1
        end if
        
        step_x = dx / real(subdivisions, wp)
        step_y = dy / real(subdivisions, wp)
        
        x = x0
        y = y0
        
        do i = 1, subdivisions
            next_x = x + step_x
            next_y = y + step_y
            call draw_line_wu(image_data, img_w, img_h, x, y, next_x, next_y, r, g, b)
            call draw_line_wu(image_data, img_w, img_h, x+0.3_wp, y+0.3_wp, next_x+0.3_wp, next_y+0.3_wp, r, g, b)
            call draw_line_wu(image_data, img_w, img_h, x-0.3_wp, y+0.3_wp, next_x-0.3_wp, next_y+0.3_wp, r, g, b)
            call draw_line_wu(image_data, img_w, img_h, x+0.3_wp, y-0.3_wp, next_x+0.3_wp, next_y-0.3_wp, r, g, b)
            call draw_line_wu(image_data, img_w, img_h, x-0.3_wp, y-0.3_wp, next_x-0.3_wp, next_y-0.3_wp, r, g, b)
            x = next_x
            y = next_y
        end do
    end subroutine draw_line_adaptive

    ! Improved line drawing algorithm
    subroutine draw_line_improved(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x0, y0, x1, y1
        integer(1), intent(in) :: r, g, b
        real(wp) :: dx, dy, length, step_x, step_y, x, y
        integer :: steps, i
        real(wp), parameter :: line_width = 1.5_wp
        
        dx = x1 - x0
        dy = y1 - y0
        length = sqrt(dx*dx + dy*dy)
        
        if (length < 1e-6_wp) return
        
        steps = max(int(length * 2), 1)
        step_x = dx / real(steps, wp)
        step_y = dy / real(steps, wp)
        
        x = x0
        y = y0
        
        do i = 0, steps
            call draw_thick_point(image_data, img_w, img_h, x, y, line_width, r, g, b)
            x = x + step_x
            y = y + step_y
        end do
    end subroutine draw_line_improved
    
    subroutine draw_thick_point(image_data, img_w, img_h, cx, cy, width, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, width
        integer(1), intent(in) :: r, g, b
        integer :: ix, iy, x, y
        real(wp) :: dx, dy, dist, alpha, half_width
        
        half_width = width * 0.5_wp
        ix = int(cx)
        iy = int(cy)
        
        do y = iy - int(half_width) - 1, iy + int(half_width) + 1
            do x = ix - int(half_width) - 1, ix + int(half_width) + 1
                if (x >= 1 .and. x <= img_w .and. y >= 1 .and. y <= img_h) then
                    dx = real(x, wp) - cx
                    dy = real(y, wp) - cy
                    dist = sqrt(dx*dx + dy*dy)
                    
                    if (dist <= half_width) then
                        alpha = 1.0_wp
                    else if (dist <= half_width + 1.0_wp) then
                        alpha = half_width + 1.0_wp - dist
                    else
                        alpha = 0.0_wp
                    end if
                    
                    if (alpha > 0.0_wp) then
                        call blend_pixel(image_data, img_w, img_h, x, y, alpha, r, g, b)
                    end if
                end if
            end do
        end do
    end subroutine draw_thick_point

    ! Wu's line algorithm and supporting functions
    subroutine draw_line_wu(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x0, y0, x1, y1
        integer(1), intent(in) :: r, g, b
        logical :: steep
        
        steep = abs(y1 - y0) > abs(x1 - x0)
        
        if (steep) then
            call draw_line_wu_impl(image_data, img_w, img_h, y0, x0, y1, x1, r, g, b, .true.)
        else
            call draw_line_wu_impl(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b, .false.)
        end if
    end subroutine draw_line_wu

    subroutine draw_line_wu_impl(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b, swapped)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x0, y0, x1, y1
        integer(1), intent(in) :: r, g, b
        logical, intent(in) :: swapped
        real(wp) :: dx, dy, gradient, xend, yend, xgap, xpxl1, ypxl1, xpxl2, ypxl2
        real(wp) :: intery, x_start, y_start, x_end, y_end
        integer :: i
        
        x_start = x0
        y_start = y0
        x_end = x1
        y_end = y1
        
        if (x_start > x_end) then
            x_start = x1
            y_start = y1
            x_end = x0
            y_end = y0
        end if
        
        dx = x_end - x_start
        dy = y_end - y_start
        
        if (abs(dx) < 1e-6_wp) then
            call draw_vertical_line_aa(image_data, img_w, img_h, x_start, y_start, y_end, r, g, b, swapped)
            return
        end if
        
        gradient = dy / dx
        
        xend = x_start
        yend = y_start + gradient * (xend - x_start)
        xgap = 1.0_wp - fpart(x_start + 0.5_wp)
        xpxl1 = xend
        ypxl1 = ipart(yend)
        
        call plot_aa_pixel(image_data, img_w, img_h, int(xpxl1), int(ypxl1), &
                          rfpart(yend) * xgap, r, g, b, swapped)
        call plot_aa_pixel(image_data, img_w, img_h, int(xpxl1), int(ypxl1) + 1, &
                          fpart(yend) * xgap, r, g, b, swapped)
        
        intery = yend + gradient
        
        xend = x_end
        yend = y_end + gradient * (xend - x_end)
        xgap = fpart(x_end + 0.5_wp)
        xpxl2 = xend
        ypxl2 = ipart(yend)
        
        call plot_aa_pixel(image_data, img_w, img_h, int(xpxl2), int(ypxl2), &
                          rfpart(yend) * xgap, r, g, b, swapped)
        call plot_aa_pixel(image_data, img_w, img_h, int(xpxl2), int(ypxl2) + 1, &
                          fpart(yend) * xgap, r, g, b, swapped)
        
        do i = int(xpxl1) + 1, int(xpxl2) - 1
            call plot_aa_pixel(image_data, img_w, img_h, i, int(ipart(intery)), &
                              rfpart(intery), r, g, b, swapped)
            call plot_aa_pixel(image_data, img_w, img_h, i, int(ipart(intery)) + 1, &
                              fpart(intery), r, g, b, swapped)
            intery = intery + gradient
        end do
    end subroutine draw_line_wu_impl

    subroutine draw_vertical_line_aa(image_data, img_w, img_h, x, y0, y1, r, g, b, swapped)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x, y0, y1
        integer(1), intent(in) :: r, g, b
        logical, intent(in) :: swapped
        real(wp) :: y_start, y_end
        integer :: i
        
        y_start = min(y0, y1)
        y_end = max(y0, y1)
        
        do i = int(y_start), int(y_end)
            call plot_aa_pixel(image_data, img_w, img_h, int(x), i, 1.0_wp, r, g, b, swapped)
        end do
    end subroutine draw_vertical_line_aa

    subroutine plot_aa_pixel(image_data, img_w, img_h, x, y, alpha, r, g, b, swapped)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h, x, y
        real(wp), intent(in) :: alpha
        integer(1), intent(in) :: r, g, b
        logical, intent(in) :: swapped
        integer :: px, py
        
        if (swapped) then
            px = y
            py = x
        else
            px = x
            py = y
        end if
        
        call blend_pixel(image_data, img_w, img_h, px, py, alpha, r, g, b)
    end subroutine plot_aa_pixel

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
    
    ! PNG file writing functionality
    subroutine write_png_file(filename, width, height, image_data)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height
        integer(1), intent(in) :: image_data(:)
        
        integer(1) :: png_signature(8) = &
            [int(-119,1), int(80,1), int(78,1), int(71,1), int(13,1), int(10,1), int(26,1), int(10,1)]
        integer, parameter :: bit_depth = 8, color_type = 2
        integer :: png_unit = 10
        integer(1), allocatable, target :: compressed_data(:)
        integer(c_long), target :: compressed_size
        integer :: status, data_size
        
        open(unit=png_unit, file=filename, access='stream', form='unformatted', status='replace')
        
        write(png_unit) png_signature
        call write_ihdr_chunk(png_unit, width, height, bit_depth, color_type)
        
        data_size = size(image_data)
        compressed_size = int(data_size * 1.1 + 12, c_long)
        allocate(compressed_data(compressed_size))
        
        status = compress_data(image_data, data_size, compressed_data, compressed_size)
        if (status /= 0) then
            print *, "Compression failed with status:", status
            stop
        end if
        
        call write_idat_chunk(png_unit, compressed_data, int(compressed_size))
        call write_iend_chunk(png_unit)
        
        close(png_unit)
        deallocate(compressed_data)
        
        print *, "PNG file '", trim(filename), "' created successfully!"
    end subroutine write_png_file
    
    ! PNG chunk writing functions (simplified versions)
    subroutine write_ihdr_chunk(unit, w, h, bd, ct)
        integer, intent(in) :: unit, w, h, bd, ct
        integer(1) :: ihdr_data(13)
        integer :: w_be, h_be
        
        w_be = to_big_endian(w)
        h_be = to_big_endian(h)
        
        ihdr_data(1:4) = transfer(w_be, ihdr_data(1:4))
        ihdr_data(5:8) = transfer(h_be, ihdr_data(5:8))
        ihdr_data(9) = int(bd, 1)
        ihdr_data(10) = int(ct, 1)
        ihdr_data(11) = 0_1
        ihdr_data(12) = 0_1
        ihdr_data(13) = 0_1
        
        call write_chunk(unit, "IHDR", ihdr_data, 13)
    end subroutine write_ihdr_chunk
    
    subroutine write_idat_chunk(unit, data, size)
        integer, intent(in) :: unit, size
        integer(1), intent(in) :: data(size)
        call write_chunk(unit, "IDAT", data, size)
    end subroutine write_idat_chunk
    
    subroutine write_iend_chunk(unit)
        integer, intent(in) :: unit
        integer(1) :: dummy(1)
        call write_chunk(unit, "IEND", dummy, 0)
    end subroutine write_iend_chunk
    
    subroutine write_chunk(unit, chunk_type, chunk_data, data_size)
        integer, intent(in) :: unit
        character(len=4), intent(in) :: chunk_type
        integer(1), intent(in) :: chunk_data(*)
        integer, intent(in) :: data_size
        
        integer :: length_be
        integer(1) :: type_bytes(4)
        integer(1), allocatable, target :: full_data(:)
        integer(c_int32_t) :: crc_value
        integer :: crc_be
        integer :: i
        
        length_be = to_big_endian(data_size)
        
        do i = 1, 4
            type_bytes(i) = int(iachar(chunk_type(i:i)), 1)
        end do
        
        allocate(full_data(4 + data_size))
        full_data(1:4) = type_bytes
        if (data_size > 0) then
            full_data(5:4+data_size) = chunk_data(1:data_size)
        end if
        
        crc_value = calculate_crc32(full_data, 4 + data_size)
        crc_be = to_big_endian(int(crc_value))
        
        write(unit) length_be
        write(unit) type_bytes
        if (data_size > 0) then
            write(unit) chunk_data(1:data_size)
        end if
        write(unit) crc_be
        
        deallocate(full_data)
    end subroutine write_chunk
    
    function to_big_endian(value) result(be_value)
        integer, intent(in) :: value
        integer :: be_value
        integer(1) :: bytes(4)
        
        bytes(1) = int(ibits(value, 24, 8), 1)
        bytes(2) = int(ibits(value, 16, 8), 1)
        bytes(3) = int(ibits(value, 8, 8), 1)
        bytes(4) = int(ibits(value, 0, 8), 1)
        
        be_value = transfer(bytes, be_value)
    end function to_big_endian
    
    function compress_data(source, source_len, dest, dest_len) result(status)
        integer(1), target, intent(in) :: source(*)
        integer, intent(in) :: source_len
        integer(1), target, intent(out) :: dest(*)
        integer(c_long), target, intent(inout) :: dest_len
        integer :: status
        
        interface
            function compress(dest, destLen, source, sourceLen) bind(C, name="compress")
                import :: c_ptr, c_long, c_int
                type(c_ptr), value :: dest
                type(c_ptr), value :: destLen
                type(c_ptr), value :: source
                integer(c_long), value :: sourceLen
                integer(c_int) :: compress
            end function compress
        end interface
        
        status = compress(c_loc(dest), c_loc(dest_len), c_loc(source), int(source_len, c_long))
    end function compress_data
    
    function calculate_crc32(data, len) result(crc)
        integer(1), target, intent(in) :: data(*)
        integer, intent(in) :: len
        integer(c_int32_t) :: crc
        
        interface
            function crc32(crc, buf, len) bind(C, name="crc32")
                import :: c_int32_t, c_ptr, c_int
                integer(c_int32_t), value :: crc
                type(c_ptr), value :: buf
                integer(c_int), value :: len
                integer(c_int32_t) :: crc32
            end function crc32
        end interface
        
        crc = crc32(0_c_int32_t, c_loc(data), int(len, c_int))
    end function calculate_crc32

    subroutine draw_axes_and_labels(ctx, xscale, yscale, symlog_threshold, &
                                   x_min_orig, x_max_orig, y_min_orig, y_max_orig, &
                                   title, xlabel, ylabel)
        !! Draw plot axes and frame with scale-aware tick generation
        type(png_context), intent(inout) :: ctx
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in), optional :: x_min_orig, x_max_orig, y_min_orig, y_max_orig
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        integer :: i
        real(wp) :: x_positions(10), y_positions(10)
        integer :: num_x, num_y
        character(len=20) :: x_labels(10), y_labels(10)
        
        ! Set color to black for axes
        ctx%current_r = 0.0_wp
        ctx%current_g = 0.0_wp
        ctx%current_b = 0.0_wp
        
        ! Draw plot frame using common functionality
        call draw_png_frame(ctx)
        
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
        call draw_png_tick_marks(ctx, x_positions, y_positions, num_x, num_y)
        call draw_png_tick_labels(ctx, x_positions, y_positions, x_labels, y_labels, num_x, num_y)
        
        ! Draw title and axis labels
        call draw_png_title_and_labels(ctx, title, xlabel, ylabel)
    end subroutine draw_axes_and_labels
    
    subroutine draw_png_frame(ctx)
        !! Draw the plot frame for PNG backend
        type(png_context), intent(inout) :: ctx
        
        ! Draw plot frame (PNG coordinates with Y=0 at top)
        call draw_line_distance_aa(ctx%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left, wp), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  real(ctx%plot_area%left + ctx%plot_area%width, wp), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  0_1, 0_1, 0_1)  ! Bottom edge (top in PNG)
        
        call draw_line_distance_aa(ctx%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left, wp), &
                                  real(ctx%plot_area%bottom, wp), &
                                  real(ctx%plot_area%left, wp), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  0_1, 0_1, 0_1)  ! Left edge
        
        call draw_line_distance_aa(ctx%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left + ctx%plot_area%width, wp), &
                                  real(ctx%plot_area%bottom, wp), &
                                  real(ctx%plot_area%left + ctx%plot_area%width, wp), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  0_1, 0_1, 0_1)  ! Right edge
        
        call draw_line_distance_aa(ctx%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left, wp), &
                                  real(ctx%plot_area%bottom, wp), &
                                  real(ctx%plot_area%left + ctx%plot_area%width, wp), &
                                  real(ctx%plot_area%bottom, wp), &
                                  0_1, 0_1, 0_1)  ! Top edge (bottom in PNG)
    end subroutine draw_png_frame
    
    subroutine draw_png_tick_marks(ctx, x_positions, y_positions, num_x, num_y)
        !! Draw tick marks for PNG backend
        type(png_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        integer, intent(in) :: num_x, num_y
        integer :: i
        
        ! Draw X-axis tick marks (at bottom of plot - top in PNG coords)
        do i = 1, num_x
            call draw_line_distance_aa(ctx%image_data, ctx%width, ctx%height, &
                                      x_positions(i), &
                                      real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                      x_positions(i), &
                                      real(ctx%plot_area%bottom + ctx%plot_area%height + 5, wp), &
                                      0_1, 0_1, 0_1)
        end do
        
        ! Draw Y-axis tick marks (at left of plot)
        do i = 1, num_y
            call draw_line_distance_aa(ctx%image_data, ctx%width, ctx%height, &
                                      real(ctx%plot_area%left, wp), y_positions(i), &
                                      real(ctx%plot_area%left - 5, wp), y_positions(i), &
                                      0_1, 0_1, 0_1)
        end do
    end subroutine draw_png_tick_marks
    
    subroutine draw_png_tick_labels(ctx, x_positions, y_positions, x_labels, y_labels, num_x, num_y)
        !! Draw tick labels for PNG backend like matplotlib
        type(png_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        character(len=*), intent(in) :: x_labels(:), y_labels(:)
        integer, intent(in) :: num_x, num_y
        integer :: i
        real(wp) :: label_x, label_y
        
        ! Draw X-axis labels with proper spacing and center alignment
        do i = 1, num_x
            call calculate_x_label_position(x_positions(i), real(ctx%plot_area%bottom, wp), &
                                          real(ctx%plot_area%height, wp), trim(x_labels(i)), &
                                          label_x, label_y)
            call render_text_to_image(ctx%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(x_labels(i)), &
                                     0_1, 0_1, 0_1)  ! Black text
        end do
        
        ! Draw Y-axis labels with right alignment and proper spacing
        do i = 1, num_y
            call calculate_y_label_position(y_positions(i), real(ctx%plot_area%left, wp), &
                                          trim(y_labels(i)), label_x, label_y)
            call render_text_to_image(ctx%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(y_labels(i)), &
                                     0_1, 0_1, 0_1)  ! Black text
        end do
    end subroutine draw_png_tick_labels
    
    subroutine draw_png_title_and_labels(ctx, title, xlabel, ylabel)
        !! Draw figure title and axis labels
        type(png_context), intent(inout) :: ctx
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp) :: label_x, label_y
        
        ! Draw title at top center of plot (larger and higher)
        if (present(title)) then
            label_x = real(ctx%width, wp) / 2.0_wp
            label_y = real(30, wp)  ! Much higher up near top of canvas
            call render_text_to_image(ctx%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(title), &
                                     0_1, 0_1, 0_1)  ! Black text
        end if
        
        ! Draw X-axis label properly centered below plot
        if (present(xlabel)) then
            ! Center horizontally on plot area
            label_x = real(ctx%plot_area%left + ctx%plot_area%width / 2, wp)
            ! Position below tick labels with adequate spacing
            label_y = real(ctx%plot_area%bottom + ctx%plot_area%height + 45, wp)
            call render_text_to_image(ctx%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y), trim(xlabel), &
                                     0_1, 0_1, 0_1)  ! Black text
        end if
        
        ! Draw Y-axis label rotated 90 degrees matplotlib-style (debug version)
        if (present(ylabel)) then
            call draw_vertical_text_png(ctx, ylabel)
        end if
    end subroutine draw_png_title_and_labels
    
    subroutine draw_vertical_text_png(ctx, text)
        !! Draw text rotated 90 degrees by rendering each character rotated with kerning
        use iso_c_binding, only: c_int, c_double
        type(png_context), intent(inout) :: ctx
        character(len=*), intent(in) :: text
        real(wp) :: label_x, label_y, total_height, current_y, kerning_adj
        integer :: i, text_len, char_code, next_char_code
        character(len=1) :: char
        integer, allocatable :: char_heights(:)
        
        ! Position for rotated Y-axis label (further left, away from axis)
        label_x = real(ctx%plot_area%left - 35, wp)  ! Further from axis to avoid overlap
        text_len = len_trim(text)
        
        ! Calculate individual character heights and kerning using FreeType metrics
        allocate(char_heights(text_len))
        total_height = 0.0_wp
        
        do i = 1, text_len
            char = text(i:i)
            char_code = iachar(char)
            char_heights(i) = get_char_advance_height(char_code)
            total_height = total_height + real(char_heights(i), wp)
            
            ! Add kerning adjustment for character pairs (except last character)
            if (i < text_len) then
                next_char_code = iachar(text(i+1:i+1))
                kerning_adj = real(get_kerning_adjustment(char_code, next_char_code), wp)
                total_height = total_height + kerning_adj
            end if
        end do
        
        ! Start position centered vertically
        label_y = real(ctx%plot_area%bottom + ctx%plot_area%height / 2, wp) - total_height / 2.0_wp
        current_y = label_y
        
        ! Draw each character rotated 90 degrees counter-clockwise (bottom to top order)
        do i = 1, text_len
            char = text(text_len + 1 - i:text_len + 1 - i)  ! Reverse character order (bottom to top)
            call draw_rotated_char_png(ctx, int(label_x), int(current_y), char)
            current_y = current_y + real(char_heights(text_len + 1 - i), wp)
            
            ! Apply kerning for character pairs (except last character in reversed order)
            if (i < text_len) then
                char_code = iachar(text(text_len + 1 - i:text_len + 1 - i))
                next_char_code = iachar(text(text_len - i:text_len - i))
                kerning_adj = real(get_kerning_adjustment(char_code, next_char_code), wp)
                current_y = current_y + kerning_adj
            end if
        end do
        
        deallocate(char_heights)
    end subroutine draw_vertical_text_png
    
    function get_char_advance_height(char_code) result(advance_height)
        !! Get character advance height using FreeType metrics
        use iso_c_binding, only: c_int, c_double
        integer, intent(in) :: char_code
        integer :: advance_height
        
        ! Local glyph info type (matches C wrapper)
        type, bind(C) :: glyph_info_t
            integer(c_int) :: width
            integer(c_int) :: height
            integer(c_int) :: left
            integer(c_int) :: top
            integer(c_int) :: advance_x
            type(c_ptr) :: buffer
            integer(c_int) :: buffer_size
        end type glyph_info_t
        
        ! Interface for FreeType character rendering
        interface
            function ft_wrapper_render_char(char_code, glyph_info) bind(C, name="ft_wrapper_render_char")
                import :: c_int, glyph_info_t
                integer(c_int), value :: char_code
                type(glyph_info_t), intent(out) :: glyph_info
                integer(c_int) :: ft_wrapper_render_char
            end function ft_wrapper_render_char
            
            subroutine ft_wrapper_free_glyph(glyph_info) bind(C, name="ft_wrapper_free_glyph")
                import :: glyph_info_t
                type(glyph_info_t), intent(inout) :: glyph_info
            end subroutine ft_wrapper_free_glyph
        end interface
        
        type(glyph_info_t) :: glyph_info
        
        ! Get glyph metrics from FreeType
        if (ft_wrapper_render_char(char_code, glyph_info) == 0) then
            ! For vertical text, use the character's actual height
            advance_height = glyph_info%height
            call ft_wrapper_free_glyph(glyph_info)
        else
            ! Fallback for failed character rendering
            advance_height = 12
        end if
    end function get_char_advance_height
    
    function get_kerning_adjustment(left_char, right_char) result(kerning_pixels)
        !! Get kerning adjustment between two characters using FreeType
        use iso_c_binding, only: c_int
        integer, intent(in) :: left_char, right_char
        integer :: kerning_pixels
        
        ! Interface for FreeType kerning function
        interface
            function ft_wrapper_get_kerning(left_char, right_char) bind(C, name="ft_wrapper_get_kerning")
                import :: c_int
                integer(c_int), value :: left_char, right_char
                integer(c_int) :: ft_wrapper_get_kerning
            end function ft_wrapper_get_kerning
        end interface
        
        ! Get kerning from FreeType and scale for vertical text
        ! For vertical text, we apply a fraction of horizontal kerning
        kerning_pixels = ft_wrapper_get_kerning(left_char, right_char) / 3
    end function get_kerning_adjustment
    
    subroutine draw_rotated_char_png(ctx, x, y, char)
        !! Draw a single character rotated 90 degrees counter-clockwise using FreeType
        use iso_c_binding, only: c_int, c_double, c_associated, c_f_pointer, c_ptr
        type(png_context), intent(inout) :: ctx
        integer, intent(in) :: x, y
        character(len=1), intent(in) :: char
        
        ! Local glyph info type (matches C wrapper)
        type, bind(C) :: glyph_info_t
            integer(c_int) :: width
            integer(c_int) :: height
            integer(c_int) :: left
            integer(c_int) :: top
            integer(c_int) :: advance_x
            type(c_ptr) :: buffer
            integer(c_int) :: buffer_size
        end type glyph_info_t
        
        ! Local interface for FreeType rotation function
        interface
            function ft_wrapper_render_char_rotated(char_code, glyph_info, angle) bind(C, name="ft_wrapper_render_char_rotated")
                import :: c_int, c_double, glyph_info_t
                integer(c_int), value :: char_code
                type(glyph_info_t), intent(out) :: glyph_info
                real(c_double), value :: angle
                integer(c_int) :: ft_wrapper_render_char_rotated
            end function ft_wrapper_render_char_rotated
        end interface
        
        type(glyph_info_t) :: glyph_info
        integer :: char_code, row, col, img_x, img_y, pixel_val
        real(8), parameter :: rotation_angle = 90.0_8  ! +90 degrees
        integer(1), pointer :: bitmap_buffer(:)
        real(8) :: alpha_val
        
        ! Get character code
        char_code = iachar(char)
        
        ! Render rotated character using FreeType
        if (ft_wrapper_render_char_rotated(char_code, glyph_info, rotation_angle) == 0) then
            if (glyph_info%width <= 0 .or. glyph_info%height <= 0) return
            if (.not. c_associated(glyph_info%buffer)) return
            
            ! Convert C pointer to Fortran array
            call c_f_pointer(glyph_info%buffer, bitmap_buffer, [glyph_info%buffer_size])
            
            ! Copy rotated glyph to image
            do row = 0, glyph_info%height - 1
                do col = 0, glyph_info%width - 1
                    img_x = x + glyph_info%left + col
                    img_y = y - glyph_info%top + row
                    
                    if (img_x >= 1 .and. img_x <= ctx%width .and. img_y >= 1 .and. img_y <= ctx%height) then
                        if (row * glyph_info%width + col + 1 <= glyph_info%buffer_size) then
                            pixel_val = int(bitmap_buffer(row * glyph_info%width + col + 1))
                            if (pixel_val < 0) pixel_val = pixel_val + 256
                            
                            if (pixel_val > 128) then
                                ! Use solid black for strong pixels
                                call blend_pixel(ctx%image_data, ctx%width, ctx%height, &
                                               img_x, img_y, 1.0_8, 0_1, 0_1, 0_1)
                            else if (pixel_val > 32) then
                                ! Use anti-aliased blending for edge pixels
                                alpha_val = real(pixel_val, 8) / 255.0_8
                                call blend_pixel(ctx%image_data, ctx%width, ctx%height, &
                                               img_x, img_y, alpha_val, 0_1, 0_1, 0_1)
                            end if
                        end if
                    end if
                end do
            end do
        end if
    end subroutine draw_rotated_char_png
    
    subroutine draw_simple_vertical_text_png(ctx, text)
        !! Draw text vertically using simple character-by-character placement (no rotation)
        type(png_context), intent(inout) :: ctx
        character(len=*), intent(in) :: text
        real(wp) :: label_x, label_y
        integer :: i, text_len
        character(len=1) :: char
        
        ! Position for vertical Y-axis label (left side of plot)
        label_x = real(ctx%plot_area%left - 15, wp)
        text_len = len_trim(text)
        
        ! Center vertically and draw each character below the previous one
        label_y = real(ctx%plot_area%bottom + ctx%plot_area%height / 2, wp) - &
                 real(text_len, wp) * 6.0_wp  ! 6 pixels spacing per character
        
        ! Draw each character vertically spaced
        do i = 1, text_len
            char = text(i:i)
            call render_text_to_image(ctx%image_data, ctx%width, ctx%height, &
                                     int(label_x), int(label_y + real(i-1, wp) * 12.0_wp), char, &
                                     0_1, 0_1, 0_1)  ! Black text
        end do
    end subroutine draw_simple_vertical_text_png
    
    
    subroutine copy_bitmap_to_image(image_data, img_width, img_height, dest_x, dest_y, &
                                   bitmap, bmp_width, bmp_height)
        !! Copy a small bitmap to the main image at specified position
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_width, img_height, dest_x, dest_y
        integer(1), intent(in) :: bitmap(*)
        integer, intent(in) :: bmp_width, bmp_height
        
        integer :: i, j, src_idx, dst_idx, pixel_x, pixel_y
        
        do j = 1, bmp_height
            do i = 1, bmp_width
                pixel_x = dest_x + i - 1
                pixel_y = dest_y + j - 1
                
                ! Check bounds
                if (pixel_x >= 0 .and. pixel_x < img_width .and. &
                    pixel_y >= 0 .and. pixel_y < img_height) then
                    
                    src_idx = ((j-1) * bmp_width + (i-1)) * 3 + 1
                    dst_idx = (pixel_y * img_width + pixel_x) * 3 + 1
                    
                    ! Only copy non-white pixels (simple transparency)
                    if (bitmap(src_idx) < 250 .or. bitmap(src_idx+1) < 250 .or. bitmap(src_idx+2) < 250) then
                        image_data(dst_idx:dst_idx+2) = bitmap(src_idx:src_idx+2)
                    end if
                end if
            end do
        end do
    end subroutine copy_bitmap_to_image

end module fortplot_png