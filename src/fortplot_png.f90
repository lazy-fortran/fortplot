module fortplot_png
    use iso_c_binding
    use fortplot_context, only: setup_canvas
    use fortplot_raster, only: raster_context, create_raster_canvas, draw_axes_and_labels, draw_rotated_ylabel_raster
    use fortplot_zlib, only: zlib_compress, crc32_calculate
    use, intrinsic :: iso_fortran_env, only: wp => real64, int8, int32
    implicit none

    private
    public :: png_context, create_png_canvas, draw_axes_and_labels, write_png_file, get_png_data

    ! PNG plotting context - extends raster context and adds PNG file I/O
    type, extends(raster_context) :: png_context
    contains
        procedure :: save => png_finalize
    end type png_context

contains

    function create_png_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(png_context) :: ctx
        type(raster_context) :: raster_base

        ! Create raster canvas and copy components to PNG context
        raster_base = create_raster_canvas(width, height)
        
        ! Initialize PNG context with same data as raster context
        call setup_canvas(ctx, width, height)
        ctx%raster = raster_base%raster
        ctx%margins = raster_base%margins
        ctx%plot_area = raster_base%plot_area
    end function create_png_canvas

    ! All drawing methods are inherited from raster_context

    subroutine png_finalize(this, filename)
        class(png_context), intent(inout) :: this
        character(len=*), intent(in) :: filename

        call write_png_file(filename, this%width, this%height, this%raster%image_data)
    end subroutine png_finalize

    ! Generate PNG data from image data
    subroutine generate_png_data(width, height, image_data, png_buffer)
        integer, intent(in) :: width, height
        integer(1), intent(in) :: image_data(:)
        integer(1), allocatable, intent(out) :: png_buffer(:)

        integer(1) :: png_signature(8) = &
            [int(-119,1), int(80,1), int(78,1), int(71,1), int(13,1), int(10,1), int(26,1), int(10,1)]
        integer, parameter :: bit_depth = 8, color_type = 2
        integer(int8), allocatable :: compressed_data(:)
        integer :: compressed_size, data_size

        data_size = size(image_data)
        compressed_data = zlib_compress(image_data, data_size, compressed_size)
        
        if (.not. allocated(compressed_data) .or. compressed_size <= 0) then
            print *, "Compression failed"
            return
        end if

        call build_png_buffer(width, height, compressed_data, compressed_size, png_buffer)
        
        deallocate(compressed_data)
    end subroutine generate_png_data

    ! Build complete PNG buffer from compressed data  
    subroutine build_png_buffer(width, height, compressed_data, compressed_size, png_buffer)
        integer, intent(in) :: width, height, compressed_size
        integer(int8), intent(in) :: compressed_data(:)
        integer(1), allocatable, intent(out) :: png_buffer(:)

        integer(1) :: png_signature(8) = &
            [int(-119,1), int(80,1), int(78,1), int(71,1), int(13,1), int(10,1), int(26,1), int(10,1)]
        integer, parameter :: bit_depth = 8, color_type = 2
        integer :: total_size, pos
        integer(1) :: ihdr_data(13)
        integer :: w_be, h_be
        
        ! Calculate total PNG size: signature + IHDR + IDAT + IEND
        total_size = 8 + (4+4+13+4) + (4+4+compressed_size+4) + (4+4+0+4)
        allocate(png_buffer(total_size))
        
        pos = 1
        
        ! Write PNG signature
        png_buffer(pos:pos+7) = png_signature
        pos = pos + 8
        
        ! Build IHDR data
        w_be = to_big_endian(width)
        h_be = to_big_endian(height)
        ihdr_data(1:4) = transfer(w_be, ihdr_data(1:4))
        ihdr_data(5:8) = transfer(h_be, ihdr_data(5:8))
        ihdr_data(9) = int(bit_depth, 1)
        ihdr_data(10) = int(color_type, 1)
        ihdr_data(11:13) = 0_1
        
        ! Write IHDR chunk to buffer
        call write_chunk_to_buffer(png_buffer, pos, "IHDR", ihdr_data, 13)
        
        ! Write IDAT chunk to buffer
        call write_chunk_to_buffer(png_buffer, pos, "IDAT", compressed_data, compressed_size)
        
        ! Write IEND chunk to buffer
        call write_chunk_to_buffer(png_buffer, pos, "IEND", [integer(1)::], 0)
    end subroutine build_png_buffer

    ! Write PNG data to file
    subroutine write_png_file(filename, width, height, image_data)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height
        integer(1), intent(in) :: image_data(:)
        
        integer(1), allocatable :: png_buffer(:)
        integer :: png_unit
        
        call generate_png_data(width, height, image_data, png_buffer)
        
        open(unit=png_unit, file=filename, access='stream', form='unformatted', status='replace')
        write(png_unit) png_buffer
        close(png_unit)
        
        deallocate(png_buffer)
        print *, "PNG file '", trim(filename), "' created successfully!"
    end subroutine write_png_file

    ! Public wrapper for getting PNG data
    subroutine get_png_data(width, height, image_data, png_buffer)
        integer, intent(in) :: width, height
        integer(1), intent(in) :: image_data(:)
        integer(1), allocatable, intent(out) :: png_buffer(:)
        
        call generate_png_data(width, height, image_data, png_buffer)
    end subroutine get_png_data

    ! Write PNG chunk to buffer
    subroutine write_chunk_to_buffer(buffer, pos, chunk_type, data, data_len)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        character(len=4), intent(in) :: chunk_type
        integer(1), intent(in) :: data(:)
        integer, intent(in) :: data_len
        
        integer :: length_be, crc_val, crc_be
        integer(1) :: type_bytes(4)
        
        ! Convert chunk type to bytes
        type_bytes = transfer(chunk_type, type_bytes)
        
        ! Write length (big endian)
        length_be = to_big_endian(data_len)
        buffer(pos:pos+3) = transfer(length_be, buffer(pos:pos+3))
        pos = pos + 4
        
        ! Write chunk type
        buffer(pos:pos+3) = type_bytes
        pos = pos + 4
        
        ! Write data
        if (data_len > 0) then
            buffer(pos:pos+data_len-1) = data(1:data_len)
            pos = pos + data_len
        end if
        
        ! Calculate and write CRC
        crc_val = calculate_chunk_crc(type_bytes, data, data_len)
        crc_be = to_big_endian(crc_val)
        buffer(pos:pos+3) = transfer(crc_be, buffer(pos:pos+3))
        pos = pos + 4
    end subroutine write_chunk_to_buffer

    ! Calculate CRC for PNG chunk
    function calculate_chunk_crc(type_bytes, data, data_len) result(crc)
        integer(1), intent(in) :: type_bytes(4), data(:)
        integer, intent(in) :: data_len
        integer :: crc
        
        integer(1), allocatable :: combined(:)
        
        allocate(combined(4 + data_len))
        combined(1:4) = type_bytes
        if (data_len > 0) then
            combined(5:4+data_len) = data(1:data_len)
        end if
        
        crc = int(crc32_calculate(combined, size(combined)))
        deallocate(combined)
    end function calculate_chunk_crc

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


    function calculate_crc32(data, len) result(crc)
        integer(1), intent(in) :: data(*)
        integer, intent(in) :: len
        integer(int32) :: crc

        crc = crc32_calculate(data, len)
    end function calculate_crc32



end module fortplot_png
