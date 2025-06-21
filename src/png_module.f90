module png_module
    use iso_c_binding
    implicit none
    
    private
    public :: generate_png_with_plot, plot_line
    
contains

    subroutine generate_png_with_plot(filename, width, height)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height
        
        ! PNG signature as signed integers (handling unsigned bytes)
        integer(1) :: png_signature(8) = &
            [int(-119,1), int(80,1), int(78,1), int(71,1), int(13,1), int(10,1), int(26,1), int(10,1)]
        
        ! PNG parameters
        integer, parameter :: bit_depth = 8
        integer, parameter :: color_type = 2  ! RGB
        
        ! File unit
        integer :: png_unit = 10
        
        ! Variables
        integer(1), allocatable, target :: image_data(:)
        integer(1), allocatable, target :: compressed_data(:)
        integer(c_long), target :: compressed_size
        integer :: status
        
        ! Open output file
        open(unit=png_unit, file=filename, access='stream', form='unformatted', status='replace')
        
        ! Write PNG signature
        write(png_unit) png_signature
        
        ! Write IHDR chunk
        call write_ihdr_chunk(png_unit, width, height, bit_depth, color_type)
        
        ! Create image data with plotted line
        allocate(image_data(height * (1 + width * 3)))
        call create_plot_with_axes(image_data, width, height)
        
        ! Compress image data
        compressed_size = int(size(image_data) * 1.1 + 12, c_long)
        allocate(compressed_data(compressed_size))
        
        status = compress_data(image_data, size(image_data), compressed_data, compressed_size)
        if (status /= 0) then
            print *, "Compression failed with status:", status
            stop
        end if
        
        ! Write IDAT chunk
        call write_idat_chunk(png_unit, compressed_data, int(compressed_size))
        
        ! Write IEND chunk
        call write_iend_chunk(png_unit)
        
        ! Close file
        close(png_unit)
        
        ! Cleanup
        deallocate(image_data)
        deallocate(compressed_data)
        
        print *, "PNG file '", trim(filename), "' created successfully!"
    end subroutine generate_png_with_plot

    subroutine create_plot_with_axes(image_data, w, h)
        integer(1), intent(out) :: image_data(*)
        integer, intent(in) :: w, h
        real :: pi
        
        pi = 4.0 * atan(1.0)
        
        ! Initialize image with white background
        call initialize_white_background(image_data, w, h)
        
        ! Plot continuous sine wave
        call draw_continuous_sine_wave(image_data, w, h, pi)
        
        ! Add coordinate axes
        call draw_axes(image_data, w, h)
    end subroutine create_plot_with_axes

    subroutine initialize_white_background(image_data, w, h)
        integer(1), intent(out) :: image_data(*)
        integer, intent(in) :: w, h
        integer :: i, j, k
        
        k = 1
        do i = 1, h
            image_data(k) = 0_1  ! Filter type (none)
            k = k + 1
            do j = 1, w
                image_data(k) = -1_1     ! White background (R=255)
                image_data(k+1) = -1_1   ! White background (G=255)
                image_data(k+2) = -1_1   ! White background (B=255)
                k = k + 3
            end do
        end do
    end subroutine initialize_white_background

    subroutine draw_axes(image_data, w, h)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: w, h
        integer :: i, j, k, x_center, y_center
        
        x_center = w / 2
        y_center = h / 2
        
        ! X-axis (horizontal line at center)
        do j = 1, w
            k = (y_center - 1) * (1 + w * 3) + 1 + (j - 1) * 3 + 1
            image_data(k) = 64_1       ! Dark gray (R=64)
            image_data(k+1) = 64_1     ! Dark gray (G=64)
            image_data(k+2) = 64_1     ! Dark gray (B=64)
        end do
        
        ! Y-axis (vertical line at center)
        do i = 1, h
            k = (i - 1) * (1 + w * 3) + 1 + (x_center - 1) * 3 + 1
            image_data(k) = 64_1       ! Dark gray (R=64)
            image_data(k+1) = 64_1     ! Dark gray (G=64)
            image_data(k+2) = 64_1     ! Dark gray (B=64)
        end do
    end subroutine draw_axes

    subroutine draw_continuous_sine_wave(image_data, w, h, pi)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: w, h
        real, intent(in) :: pi
        
        ! Draw sine wave using parametric approach with real coordinates
        call draw_parametric_curve(image_data, w, h, 0_1, 0_1, -1_1, pi)
    end subroutine draw_continuous_sine_wave

    subroutine draw_parametric_curve(image_data, img_w, img_h, r, g, b, pi)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        integer(1), intent(in) :: r, g, b
        real, intent(in) :: pi
        real :: x_prev, y_prev, x_curr, y_curr
        real :: t, dt, x_norm, y_norm
        integer :: i, n_steps
        
        ! Use small steps for smooth curve - step size based on pixel resolution
        n_steps = img_w * 2  ! 2 steps per pixel width for smooth curves
        dt = 1.0 / real(n_steps - 1)
        
        ! Calculate first point
        t = 0.0
        x_norm = t
        y_norm = 0.5 * sin(4.0 * pi * x_norm) + 0.5
        x_prev = x_norm * real(img_w - 1) + 1.0
        y_prev = y_norm * real(img_h - 1) + 1.0
        
        ! Draw curve by connecting consecutive real-valued points
        do i = 2, n_steps
            t = real(i - 1) * dt
            x_norm = t
            y_norm = 0.5 * sin(4.0 * pi * x_norm) + 0.5
            
            x_curr = x_norm * real(img_w - 1) + 1.0
            y_curr = y_norm * real(img_h - 1) + 1.0
            
            ! Draw line segment using real coordinates directly
            call draw_line_wu(image_data, img_w, img_h, x_prev, y_prev, x_curr, y_curr, r, g, b)
            
            x_prev = x_curr
            y_prev = y_curr
        end do
    end subroutine draw_parametric_curve

    subroutine plot_line(image_data, img_w, img_h, x_data, y_data, n_points, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h, n_points
        real, intent(in) :: x_data(n_points), y_data(n_points)
        integer(1), intent(in) :: r, g, b
        integer :: i, x_prev, y_prev, x_curr, y_curr
        integer :: x_min, x_max, y_min, y_max, x, y
        
        if (n_points < 1) return
        
        ! Convert first point to pixel coordinates
        x_prev = int(x_data(1) * real(img_w - 1)) + 1
        y_prev = int(y_data(1) * real(img_h - 1)) + 1
        call clamp_coordinates(x_prev, y_prev, img_w, img_h)
        
        ! Draw first point
        call set_pixel(image_data, x_prev, y_prev, img_w, img_h, r, g, b)
        
        ! Draw line segments connecting consecutive points
        do i = 2, n_points
            ! Convert current point to pixel coordinates
            x_curr = int(x_data(i) * real(img_w - 1)) + 1
            y_curr = int(y_data(i) * real(img_h - 1)) + 1
            call clamp_coordinates(x_curr, y_curr, img_w, img_h)
            
            ! Draw line segment using Bresenham-like algorithm
            call draw_line_segment(image_data, img_w, img_h, x_prev, y_prev, x_curr, y_curr, r, g, b)
            
            x_prev = x_curr
            y_prev = y_curr
        end do
    end subroutine plot_line

    subroutine draw_line_segment(image_data, img_w, img_h, x1, y1, x2, y2, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h, x1, y1, x2, y2
        integer(1), intent(in) :: r, g, b
        
        ! Use antialiased line drawing
        call draw_line_wu(image_data, img_w, img_h, real(x1), real(y1), real(x2), real(y2), r, g, b)
    end subroutine draw_line_segment

    subroutine draw_line_wu(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real, intent(in) :: x0, y0, x1, y1
        integer(1), intent(in) :: r, g, b
        logical :: steep
        real :: dx, dy, gradient, xend, yend, xgap, xpxl1, ypxl1, xpxl2, ypxl2
        real :: intery, x, y
        integer :: ix, iy, i
        
        steep = abs(y1 - y0) > abs(x1 - x0)
        
        if (steep) then
            ! Swap x and y coordinates
            call draw_line_wu_impl(image_data, img_w, img_h, y0, x0, y1, x1, r, g, b, .true.)
        else
            call draw_line_wu_impl(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b, .false.)
        end if
    end subroutine draw_line_wu

    subroutine draw_line_wu_impl(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b, swapped)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real, intent(in) :: x0, y0, x1, y1
        integer(1), intent(in) :: r, g, b
        logical, intent(in) :: swapped
        real :: dx, dy, gradient, xend, yend, xgap, xpxl1, ypxl1, xpxl2, ypxl2
        real :: intery, x_start, y_start, x_end, y_end
        integer :: ix, iy, i
        
        x_start = x0
        y_start = y0
        x_end = x1
        y_end = y1
        
        ! Ensure line goes from left to right
        if (x_start > x_end) then
            ! Swap points
            x_start = x1
            y_start = y1
            x_end = x0
            y_end = y0
        end if
        
        dx = x_end - x_start
        dy = y_end - y_start
        
        if (abs(dx) < 1e-6) then
            ! Vertical line
            call draw_vertical_line_aa(image_data, img_w, img_h, x_start, y_start, y_end, r, g, b, swapped)
            return
        end if
        
        gradient = dy / dx
        
        ! Handle first endpoint
        xend = x_start
        yend = y_start + gradient * (xend - x_start)
        xgap = 1.0 - fpart(x_start + 0.5)
        xpxl1 = xend
        ypxl1 = ipart(yend)
        
        call plot_aa_pixel(image_data, img_w, img_h, int(xpxl1), int(ypxl1), &
                          rfpart(yend) * xgap, r, g, b, swapped)
        call plot_aa_pixel(image_data, img_w, img_h, int(xpxl1), int(ypxl1) + 1, &
                          fpart(yend) * xgap, r, g, b, swapped)
        
        intery = yend + gradient
        
        ! Handle second endpoint  
        xend = x_end
        yend = y_end + gradient * (xend - x_end)
        xgap = fpart(x_end + 0.5)
        xpxl2 = xend
        ypxl2 = ipart(yend)
        
        call plot_aa_pixel(image_data, img_w, img_h, int(xpxl2), int(ypxl2), &
                          rfpart(yend) * xgap, r, g, b, swapped)
        call plot_aa_pixel(image_data, img_w, img_h, int(xpxl2), int(ypxl2) + 1, &
                          fpart(yend) * xgap, r, g, b, swapped)
        
        ! Main loop
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
        real, intent(in) :: x, y0, y1
        integer(1), intent(in) :: r, g, b
        logical, intent(in) :: swapped
        real :: y_start, y_end
        integer :: i
        
        y_start = min(y0, y1)
        y_end = max(y0, y1)
        
        do i = int(y_start), int(y_end)
            call plot_aa_pixel(image_data, img_w, img_h, int(x), i, 1.0, r, g, b, swapped)
        end do
    end subroutine draw_vertical_line_aa

    subroutine plot_aa_pixel(image_data, img_w, img_h, x, y, alpha, r, g, b, swapped)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h, x, y
        real, intent(in) :: alpha
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
        real, intent(in) :: alpha
        integer(1), intent(in) :: new_r, new_g, new_b
        integer :: k
        real :: inv_alpha
        integer :: bg_r, bg_g, bg_b, fg_r, fg_g, fg_b
        
        ! Check bounds
        if (x < 1 .or. x > img_w .or. y < 1 .or. y > img_h) return
        if (alpha <= 0.0) return
        
        ! Calculate pixel position
        k = (y - 1) * (1 + img_w * 3) + 1 + (x - 1) * 3 + 1
        
        if (alpha >= 1.0) then
            ! Full opacity - just set the color
            image_data(k) = new_r
            image_data(k+1) = new_g
            image_data(k+2) = new_b
        else
            ! Alpha blending
            inv_alpha = 1.0 - alpha
            
            ! Get background color (convert from signed to unsigned)
            bg_r = int(image_data(k))
            bg_g = int(image_data(k+1))
            bg_b = int(image_data(k+2))
            if (bg_r < 0) bg_r = bg_r + 256
            if (bg_g < 0) bg_g = bg_g + 256
            if (bg_b < 0) bg_b = bg_b + 256
            
            ! Get foreground color (convert from signed to unsigned)
            fg_r = int(new_r)
            fg_g = int(new_g)
            fg_b = int(new_b)
            if (fg_r < 0) fg_r = fg_r + 256
            if (fg_g < 0) fg_g = fg_g + 256
            if (fg_b < 0) fg_b = fg_b + 256
            
            ! Blend colors
            bg_r = int(inv_alpha * real(bg_r) + alpha * real(fg_r))
            bg_g = int(inv_alpha * real(bg_g) + alpha * real(fg_g))
            bg_b = int(inv_alpha * real(bg_b) + alpha * real(fg_b))
            
            ! Convert back to signed bytes
            if (bg_r > 127) bg_r = bg_r - 256
            if (bg_g > 127) bg_g = bg_g - 256
            if (bg_b > 127) bg_b = bg_b - 256
            
            image_data(k) = int(bg_r, 1)
            image_data(k+1) = int(bg_g, 1)
            image_data(k+2) = int(bg_b, 1)
        end if
    end subroutine blend_pixel

    ! Helper functions for Wu's algorithm
    real function ipart(x)
        real, intent(in) :: x
        ipart = real(int(x))
    end function ipart

    real function fpart(x)
        real, intent(in) :: x
        fpart = x - ipart(x)
    end function fpart

    real function rfpart(x)
        real, intent(in) :: x
        rfpart = 1.0 - fpart(x)
    end function rfpart

    subroutine clamp_coordinates(x, y, max_w, max_h)
        integer, intent(inout) :: x, y
        integer, intent(in) :: max_w, max_h
        
        if (x < 1) x = 1
        if (x > max_w) x = max_w
        if (y < 1) y = 1
        if (y > max_h) y = max_h
    end subroutine clamp_coordinates

    subroutine set_pixel(image_data, x, y, w, h, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: x, y, w, h
        integer(1), intent(in) :: r, g, b
        integer :: k
        
        ! Check bounds
        if (x < 1 .or. x > w .or. y < 1 .or. y > h) return
        
        ! Calculate pixel position in image_data array
        k = (y - 1) * (1 + w * 3) + 1 + (x - 1) * 3 + 1
        
        ! Set pixel color
        image_data(k) = r
        image_data(k+1) = g
        image_data(k+2) = b
    end subroutine set_pixel

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
        
        ! Convert length to big-endian
        length_be = to_big_endian(data_size)
        
        ! Convert chunk type to bytes
        do i = 1, 4
            type_bytes(i) = int(iachar(chunk_type(i:i)), 1)
        end do
        
        ! Prepare data for CRC calculation
        allocate(full_data(4 + data_size))
        full_data(1:4) = type_bytes
        if (data_size > 0) then
            full_data(5:4+data_size) = chunk_data(1:data_size)
        end if
        
        ! Calculate CRC
        crc_value = calculate_crc32(full_data, 4 + data_size)
        crc_be = to_big_endian(int(crc_value))
        
        ! Write chunk
        write(unit) length_be
        write(unit) type_bytes
        if (data_size > 0) then
            write(unit) chunk_data(1:data_size)
        end if
        write(unit) crc_be
        
        deallocate(full_data)
    end subroutine write_chunk
    
    subroutine write_ihdr_chunk(unit, w, h, bd, ct)
        integer, intent(in) :: unit, w, h, bd, ct
        integer(1) :: ihdr_data(13)
        integer :: w_be, h_be
        
        w_be = to_big_endian(w)
        h_be = to_big_endian(h)
        
        ! Pack IHDR data
        ihdr_data(1:4) = transfer(w_be, ihdr_data(1:4))
        ihdr_data(5:8) = transfer(h_be, ihdr_data(5:8))
        ihdr_data(9) = int(bd, 1)
        ihdr_data(10) = int(ct, 1)
        ihdr_data(11) = 0_1  ! Compression method
        ihdr_data(12) = 0_1  ! Filter method
        ihdr_data(13) = 0_1  ! Interlace method
        
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
    
    ! Interface to zlib compress function
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
    
    ! Interface to zlib crc32 function
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
    
    ! Helper function to convert unsigned byte to signed integer(1)
    function uint8_to_int1(uint_val) result(int_val)
        integer, intent(in) :: uint_val
        integer(1) :: int_val
        
        if (uint_val > 127) then
            int_val = int(uint_val - 256, 1)
        else
            int_val = int(uint_val, 1)
        end if
    end function uint8_to_int1

end module png_module