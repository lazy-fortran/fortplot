module fortplot_png
    use iso_c_binding
    use fortplot_context, only: setup_canvas
    use fortplot_raster, only: raster_context, create_raster_canvas, draw_axes_and_labels, draw_rotated_ylabel_raster
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: png_context, create_png_canvas, draw_axes_and_labels, write_png_file

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

    ! PNG file writing functionality
    subroutine write_png_file(filename, width, height, image_data)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height
        integer(1), intent(in) :: image_data(:)

        integer(1) :: png_signature(8) = &
            [int(-119,1), int(80,1), int(78,1), int(71,1), int(13,1), int(10,1), int(26,1), int(10,1)]
        integer, parameter :: bit_depth = 8, color_type = 2
        integer :: png_unit = 10
        type(c_ptr) :: compressed_ptr
        integer(1), pointer :: compressed_data(:)
        integer :: compressed_size, data_size

        open(unit=png_unit, file=filename, access='stream', form='unformatted', status='replace')

        write(png_unit) png_signature
        call write_ihdr_chunk(png_unit, width, height, bit_depth, color_type)

        data_size = size(image_data)
        call compress_data_stb(image_data, data_size, compressed_ptr, compressed_size)
        
        if (.not. c_associated(compressed_ptr)) then
            print *, "Compression failed"
            close(png_unit)
            stop
        end if
        
        ! Convert C pointer to Fortran pointer for writing
        call c_f_pointer(compressed_ptr, compressed_data, [compressed_size])

        call write_idat_chunk(png_unit, compressed_data, compressed_size)
        call write_iend_chunk(png_unit)

        close(png_unit)
        
        ! Free STB allocated memory
        call free_stb_data(compressed_ptr)

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

    subroutine compress_data_stb(source, source_len, compressed_ptr, compressed_len)
        integer(1), target, intent(in) :: source(*)
        integer, intent(in) :: source_len
        type(c_ptr), intent(out) :: compressed_ptr
        integer, intent(out) :: compressed_len
        integer(c_int), target :: compressed_len_c

        interface
            function stb_compress_data(data, data_len, compressed_len, quality) bind(C, name="stb_compress_data")
                import :: c_ptr, c_int
                type(c_ptr), value :: data
                integer(c_int), value :: data_len
                type(c_ptr), value :: compressed_len
                integer(c_int), value :: quality
                type(c_ptr) :: stb_compress_data
            end function stb_compress_data
        end interface

        ! Call STB compression with quality level 8 (default)
        compressed_ptr = stb_compress_data(c_loc(source), int(source_len, c_int), c_loc(compressed_len_c), 8_c_int)
        compressed_len = int(compressed_len_c)
    end subroutine compress_data_stb

    subroutine free_stb_data(data_ptr)
        type(c_ptr), intent(in) :: data_ptr
        
        interface
            subroutine stb_free_data(data) bind(C, name="stb_free_data")
                import :: c_ptr
                type(c_ptr), value :: data
            end subroutine stb_free_data
        end interface
        
        call stb_free_data(data_ptr)
    end subroutine free_stb_data

    function calculate_crc32(data, len) result(crc)
        integer(1), target, intent(in) :: data(*)
        integer, intent(in) :: len
        integer(c_int32_t) :: crc

        interface
            function stb_crc32(data, data_len) bind(C, name="stb_crc32")
                import :: c_ptr, c_int, c_int32_t
                type(c_ptr), value :: data
                integer(c_int), value :: data_len
                integer(c_int32_t) :: stb_crc32
            end function stb_crc32
        end interface

        crc = stb_crc32(c_loc(data), int(len, c_int))
    end function calculate_crc32



end module fortplot_png
