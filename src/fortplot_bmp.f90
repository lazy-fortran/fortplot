module fortplot_bmp
    implicit none

    private

    public :: write_bmp_file

    contains

    subroutine write_bmp_file(filename, bitmap, width, height)
        !! Write RGB bitmap as simple 24-bit BMP file
        character(len=*), intent(in) :: filename
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: width, height
        
        integer :: unit, i, j, row_padding, file_size, pixel_data_size
        integer(1) :: bmp_header(54), padding_bytes(3)
        integer(1) :: r, g, b
        
        ! Calculate row padding (BMP rows must be multiple of 4 bytes)
        row_padding = mod(4 - mod(width * 3, 4), 4)
        pixel_data_size = height * (width * 3 + row_padding)
        file_size = 54 + pixel_data_size
        
        ! Initialize padding bytes
        padding_bytes = 0_1
        
        ! BMP header (54 bytes total)
        bmp_header = 0_1
        
        ! File header (14 bytes)
        bmp_header(1:2) = [66_1, 77_1]  ! "BM" signature
        call write_int32_le(bmp_header(3:6), file_size)     ! File size
        ! Reserved fields (4 bytes) already zero
        call write_int32_le(bmp_header(11:14), 54)          ! Pixel data offset
        
        ! Info header (40 bytes)
        call write_int32_le(bmp_header(15:18), 40)          ! Info header size
        call write_int32_le(bmp_header(19:22), width)       ! Width
        call write_int32_le(bmp_header(23:26), height)      ! Height
        call write_int16_le(bmp_header(27:28), 1)           ! Planes
        call write_int16_le(bmp_header(29:30), 24)          ! Bits per pixel
        ! Compression, image size, resolution fields already zero
        
        ! Write BMP file
        open(newunit=unit, file=filename, access="stream", form="unformatted", status="replace")
        
        write(unit) bmp_header
        
        ! Write pixel data (BMP stores rows bottom-to-top)
        do j = height, 1, -1
            do i = 1, width
                ! Convert signed byte to unsigned and write as BGR
                r = bitmap(i, j, 1)
                g = bitmap(i, j, 2) 
                b = bitmap(i, j, 3)
                write(unit) b, g, r  ! BMP uses BGR order
            end do
            ! Write row padding
            if (row_padding > 0) then
                write(unit) padding_bytes(1:row_padding)
            end if
        end do
        
        close(unit)
    end subroutine write_bmp_file
    
    subroutine write_int32_le(bytes, value)
        !! Write 32-bit integer in little-endian format
        integer(1), intent(out) :: bytes(4)
        integer, intent(in) :: value
        
        bytes(1) = int(iand(value, 255), 1)
        bytes(2) = int(iand(ishft(value, -8), 255), 1)
        bytes(3) = int(iand(ishft(value, -16), 255), 1)
        bytes(4) = int(iand(ishft(value, -24), 255), 1)
    end subroutine write_int32_le
    
    subroutine write_int16_le(bytes, value)
        !! Write 16-bit integer in little-endian format
        integer(1), intent(out) :: bytes(2)
        integer, intent(in) :: value
        
        bytes(1) = int(iand(value, 255), 1)
        bytes(2) = int(iand(ishft(value, -8), 255), 1)
    end subroutine write_int16_le

end module fortplot_bmp