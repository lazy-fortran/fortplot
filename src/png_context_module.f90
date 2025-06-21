module png_context_module
    use iso_c_binding
    use plotting_module
    use text_module
    implicit none
    
    private
    public :: png_context, create_png_canvas
    
    ! PNG plotting context
    type, extends(plot_context) :: png_context
        integer(1), allocatable :: image_data(:)
        real :: current_r, current_g, current_b
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
        
        ! Initialize PNG image data
        allocate(ctx%image_data(height * (1 + width * 3)))
        call initialize_white_background(ctx%image_data, width, height)
        
        ! Default color (blue)
        ctx%current_r = 0.0
        ctx%current_g = 0.0
        ctx%current_b = 1.0
    end function create_png_canvas
    
    subroutine png_draw_line(this, x1, y1, x2, y2)
        class(png_context), intent(inout) :: this
        real, intent(in) :: x1, y1, x2, y2
        real :: px1, py1, px2, py2
        integer(1) :: r, g, b
        
        ! Convert world coordinates to pixel coordinates
        px1 = (x1 - this%x_min) / (this%x_max - this%x_min) * real(this%width - 1) + 1.0
        py1 = (1.0 - (y1 - this%y_min) / (this%y_max - this%y_min)) * real(this%height - 1) + 1.0
        px2 = (x2 - this%x_min) / (this%x_max - this%x_min) * real(this%width - 1) + 1.0
        py2 = (1.0 - (y2 - this%y_min) / (this%y_max - this%y_min)) * real(this%height - 1) + 1.0
        
        ! Convert color to signed bytes
        r = color_to_byte(this%current_r)
        g = color_to_byte(this%current_g)
        b = color_to_byte(this%current_b)
        
        ! Draw antialiased line
        call draw_line_wu(this%image_data, this%width, this%height, px1, py1, px2, py2, r, g, b)
    end subroutine png_draw_line
    
    subroutine png_set_color(this, r, g, b)
        class(png_context), intent(inout) :: this
        real, intent(in) :: r, g, b
        
        this%current_r = r
        this%current_g = g
        this%current_b = b
    end subroutine png_set_color
    
    subroutine png_draw_text(this, x, y, text)
        class(png_context), intent(inout) :: this
        real, intent(in) :: x, y
        character(len=*), intent(in) :: text
        real :: px, py
        
        px = (x - this%x_min) / (this%x_max - this%x_min) * real(this%width - 1) + 1.0
        py = (1.0 - (y - this%y_min) / (this%y_max - this%y_min)) * real(this%height - 1) + 1.0
        
        call render_text_to_image(this%image_data, this%width, this%height, &
                                 int(px), int(py), text, &
                                 color_to_byte(this%current_r), &
                                 color_to_byte(this%current_g), &
                                 color_to_byte(this%current_b))
    end subroutine png_draw_text
    
    subroutine png_finalize(this, filename)
        class(png_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        ! Use existing PNG generation functionality
        call write_png_file(filename, this%width, this%height, this%image_data)
    end subroutine png_finalize
    
    function color_to_byte(color_val) result(byte_val)
        real, intent(in) :: color_val
        integer(1) :: byte_val
        integer :: int_val
        
        int_val = int(color_val * 255.0)
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
    
    ! Wu's line algorithm and supporting functions
    subroutine draw_line_wu(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real, intent(in) :: x0, y0, x1, y1
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
        real, intent(in) :: x0, y0, x1, y1
        integer(1), intent(in) :: r, g, b
        logical, intent(in) :: swapped
        real :: dx, dy, gradient, xend, yend, xgap, xpxl1, ypxl1, xpxl2, ypxl2
        real :: intery, x_start, y_start, x_end, y_end
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
        
        if (abs(dx) < 1e-6) then
            call draw_vertical_line_aa(image_data, img_w, img_h, x_start, y_start, y_end, r, g, b, swapped)
            return
        end if
        
        gradient = dy / dx
        
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
        
        xend = x_end
        yend = y_end + gradient * (xend - x_end)
        xgap = fpart(x_end + 0.5)
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
        
        if (x < 1 .or. x > img_w .or. y < 1 .or. y > img_h) return
        if (alpha <= 0.0) return
        
        k = (y - 1) * (1 + img_w * 3) + 1 + (x - 1) * 3 + 1
        
        if (alpha >= 1.0) then
            image_data(k) = new_r
            image_data(k+1) = new_g
            image_data(k+2) = new_b
        else
            inv_alpha = 1.0 - alpha
            
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


end module png_context_module