module fortplot_png
    use iso_c_binding
    use fortplot_context, only: setup_canvas
    use fortplot_raster, only: raster_context, create_raster_canvas, raster_draw_axes_and_labels, raster_render_ylabel
    use fortplot_zlib_core, only: zlib_compress_into, crc32_calculate
    use fortplot_logging, only: log_error, log_info
    use, intrinsic :: iso_fortran_env, only: wp => real64, int8, int32
    implicit none

    private
    public :: png_context, create_png_canvas, raster_draw_axes_and_labels, write_png_file, get_png_data

    ! PNG plotting context - extends raster context and adds PNG file I/O
    type, extends(raster_context) :: png_context
    contains
        procedure :: save => png_finalize
        procedure :: get_png_data_backend => png_get_png_data
    end type png_context

    ! C interface for rename function
    interface
        function c_rename(oldname, newname) bind(C, name="rename") result(status)
            import :: c_char, c_int
            character(kind=c_char), dimension(*), intent(in) :: oldname
            character(kind=c_char), dimension(*), intent(in) :: newname
            integer(c_int) :: status
        end function c_rename
    end interface

contains

    subroutine rename(oldname, newname)
        character(len=*), intent(in) :: oldname
        character(len=*), intent(in) :: newname
        character(kind=c_char), dimension(:), allocatable :: c_oldname, c_newname
        integer :: i, status
        
        ! Convert Fortran strings to C strings (null-terminated)
        allocate(c_oldname(len_trim(oldname) + 1))
        allocate(c_newname(len_trim(newname) + 1))
        
        do i = 1, len_trim(oldname)
            c_oldname(i) = oldname(i:i)
        end do
        c_oldname(len_trim(oldname) + 1) = c_null_char
        
        do i = 1, len_trim(newname)
            c_newname(i) = newname(i:i)
        end do
        c_newname(len_trim(newname) + 1) = c_null_char
        
        status = c_rename(c_oldname, c_newname)
        
        deallocate(c_oldname)
        deallocate(c_newname)
    end subroutine rename

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
        use fortplot_system_viewer, only: launch_system_viewer, has_graphical_session, &
                                          get_temp_filename
        class(png_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        character(len=1024) :: temp_file
        logical :: viewer_success

        if (trim(filename) == 'terminal') then
            if (has_graphical_session()) then
                call get_temp_filename('.png', temp_file)
                call write_png_file(temp_file, this%width, this%height, this%raster%image_data)
                call launch_system_viewer(temp_file, viewer_success)
                if (.not. viewer_success) then
                    call log_error("Failed to launch PNG viewer for: " // trim(temp_file))
                    call log_info("You can manually open: " // trim(temp_file))
                end if
            else
                call log_info("No graphical session detected, cannot display PNG")
                call log_info("Use savefig('filename.png') to save to file or")
                call log_info("Use savefig('filename.txt') for ASCII rendering")
            end if
        else
            call write_png_file(filename, this%width, this%height, this%raster%image_data)
        end if
    end subroutine png_finalize

    subroutine fallback_to_ascii(this)
        !! Fallback to ASCII rendering when no graphical session
        class(png_context), intent(inout) :: this
        associate(dw=>this%width); end associate

        call log_info("PNG backend cannot display without graphics - ASCII fallback not yet implemented")
    end subroutine fallback_to_ascii

    subroutine png_get_png_data(this, width, height, png_data, status)
        !! Get PNG data from PNG context's raster data
        class(png_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        call generate_png_data(width, height, this%raster%image_data, png_data)
        status = 0
    end subroutine png_get_png_data

    ! Generate PNG data from image data
    subroutine generate_png_data(width, height, image_data, png_buffer)
        integer, intent(in) :: width, height
        integer(1), intent(in) :: image_data(:)
        integer(1), allocatable, intent(out) :: png_buffer(:)

        integer(int8), allocatable :: compressed_data(:)
        integer :: compressed_size, data_size
        integer(1), allocatable :: png_row_data(:)
        
        ! Convert RGB image data to PNG row format with filter bytes
        call convert_rgb_to_png_rows(width, height, image_data, png_row_data)
        
        data_size = size(png_row_data)
        call zlib_compress_into(png_row_data, data_size, compressed_data, compressed_size)
        
        if (.not. allocated(compressed_data) .or. compressed_size <= 0) then
            call log_error("PNG compression failed")
            return
        end if

        call build_png_buffer(width, height, compressed_data, compressed_size, png_buffer)
        
        if (allocated(compressed_data)) deallocate(compressed_data)
        if (allocated(png_row_data)) deallocate(png_row_data)
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
        
        ! Build IHDR data using the exact working approach
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

    ! Write PNG data to file with error handling
    subroutine write_png_file(filename, width, height, image_data)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height
        integer(1), intent(in) :: image_data(:)
        
        integer(1), allocatable :: png_buffer(:)
        integer :: png_unit, ios
        character(len=512) :: error_msg
        character(len=1024) :: tmp_filename
        integer :: clk_count, clk_rate, clk_max
        logical :: final_exists
        logical :: tmp_exists
        integer :: ios_tmp
        
        call generate_png_data(width, height, image_data, png_buffer)
        
        if (.not. allocated(png_buffer)) then
            call log_error("Failed to generate PNG data for '" // trim(filename) // "'")
            return
        end if

        ! Create a unique temporary filename in the same directory for atomic write
        call system_clock(clk_count, clk_rate, clk_max)
        write(tmp_filename, '(A,".tmp.",I0)') trim(filename), clk_count

        open(newunit=png_unit, file=trim(tmp_filename), access='stream', form='unformatted', &
             status='replace', iostat=ios, iomsg=error_msg)

        if (ios /= 0) then
            call log_error("Cannot save PNG file '" // trim(tmp_filename) // "': " // trim(error_msg))
            if (allocated(png_buffer)) deallocate(png_buffer)
            return
        end if

        write(png_unit, iostat=ios) png_buffer
        
        if (ios /= 0) then
            call log_error("Failed to write PNG data to '" // trim(tmp_filename) // "'")
            close(png_unit, status='delete')  ! Remove incomplete file
            if (allocated(png_buffer)) deallocate(png_buffer)
            return
        end if
        close(png_unit)

        ! Atomically move the temporary file into place (best-effort)
        call rename(trim(tmp_filename), trim(filename))
        inquire(file=trim(tmp_filename), exist=tmp_exists)
        if (tmp_exists) then
            ! Some platforms (e.g., certain Windows runtimes) don't overwrite existing files on rename.
            ! Fallback: delete destination if it exists, then try rename again; finally, last-resort copy.
            open(newunit=png_unit, file=trim(filename), status='old', iostat=ios_tmp)
            if (ios_tmp == 0) then
                close(png_unit, status='delete')
            end if

            call rename(trim(tmp_filename), trim(filename))
            inquire(file=trim(tmp_filename), exist=tmp_exists)
            if (tmp_exists) then
                ! Last-resort non-atomic fallback: write buffer directly to destination
                open(newunit=png_unit, file=trim(filename), access='stream', form='unformatted', &
                     status='replace', iostat=ios, iomsg=error_msg)
                if (ios == 0) then
                    write(png_unit, iostat=ios) png_buffer
                    close(png_unit)
                end if

                if (ios /= 0) then
                    call log_error("Failed to finalize PNG file '" // trim(filename) // "': " // trim(error_msg))
                    ! Clean up temp file if it still exists
                    open(newunit=png_unit, file=trim(tmp_filename), status='old', iostat=ios_tmp)
                    if (ios_tmp == 0) close(png_unit, status='delete')
                    if (allocated(png_buffer)) deallocate(png_buffer)
                    return
                end if

                ! Cleanup: remove temp file after successful copy
                open(newunit=png_unit, file=trim(tmp_filename), status='old', iostat=ios_tmp)
                if (ios_tmp == 0) close(png_unit, status='delete')
            end if
        end if

        ! Verify destination exists
        inquire(file=trim(filename), exist=final_exists)
        if (.not. final_exists) then
            call log_error("Failed to finalize PNG file '" // trim(filename) // "'")
            if (allocated(png_buffer)) deallocate(png_buffer)
            return
        end if

        if (allocated(png_buffer)) deallocate(png_buffer)
        call log_info("PNG file '" // trim(filename) // "' created successfully!")
    end subroutine write_png_file

    ! Public wrapper for getting PNG data
    subroutine get_png_data(width, height, image_data, png_buffer)
        integer, intent(in) :: width, height
        integer(1), intent(in) :: image_data(:)
        integer(1), allocatable, intent(out) :: png_buffer(:)
        
        call generate_png_data(width, height, image_data, png_buffer)
    end subroutine get_png_data
    
    ! Convert RGB image data to PNG row format with filter bytes
    subroutine convert_rgb_to_png_rows(width, height, rgb_data, png_row_data)
        integer, intent(in) :: width, height
        integer(1), intent(in) :: rgb_data(:)  ! width * height * 3 RGB bytes
        integer(1), allocatable, intent(out) :: png_row_data(:)  ! height * (1 + width * 3) bytes
        
        integer :: row, col, rgb_idx, png_idx, row_start
        
        ! Allocate PNG row data: height rows * (1 filter byte + width * 3 RGB bytes)
        allocate(png_row_data(height * (1 + width * 3)))
        
        ! Convert row by row
        do row = 1, height
            ! Calculate indices
            row_start = (row - 1) * (1 + width * 3) + 1
            
            ! Set filter byte to 0 (no filter)
            png_row_data(row_start) = 0_1
            
            ! Copy RGB data for this row  
            do col = 1, width
                rgb_idx = (row - 1) * width * 3 + (col - 1) * 3 + 1
                png_idx = row_start + 1 + (col - 1) * 3
                
                png_row_data(png_idx)     = rgb_data(rgb_idx)     ! R
                png_row_data(png_idx + 1) = rgb_data(rgb_idx + 1) ! G  
                png_row_data(png_idx + 2) = rgb_data(rgb_idx + 2) ! B
            end do
        end do
    end subroutine convert_rgb_to_png_rows

    ! Write PNG chunk to buffer
    subroutine write_chunk_to_buffer(buffer, pos, chunk_type, data, data_len)
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: pos
        character(len=4), intent(in) :: chunk_type
        integer(1), intent(in) :: data(:)
        integer, intent(in) :: data_len
        
        integer :: crc_val, i
        integer(1) :: type_bytes(4)
        
        ! Convert chunk type to bytes (using correct ASCII conversion)
        do i = 1, 4
            type_bytes(i) = int(iachar(chunk_type(i:i)), 1)
        end do
        
        ! Write length (big endian) - write bytes directly
        buffer(pos) = int(ibits(data_len, 24, 8), 1)
        buffer(pos+1) = int(ibits(data_len, 16, 8), 1)
        buffer(pos+2) = int(ibits(data_len, 8, 8), 1)
        buffer(pos+3) = int(ibits(data_len, 0, 8), 1)
        pos = pos + 4
        
        ! Write chunk type
        buffer(pos:pos+3) = type_bytes
        pos = pos + 4
        
        ! Write data
        if (data_len > 0) then
            buffer(pos:pos+data_len-1) = data(1:data_len)
            pos = pos + data_len
        end if
        
        ! Calculate and write CRC (write bytes directly in big-endian order)
        crc_val = calculate_chunk_crc(type_bytes, data, data_len)
        buffer(pos) = int(ibits(crc_val, 24, 8), 1)
        buffer(pos+1) = int(ibits(crc_val, 16, 8), 1)
        buffer(pos+2) = int(ibits(crc_val, 8, 8), 1)
        buffer(pos+3) = int(ibits(crc_val, 0, 8), 1)
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
        if (allocated(combined)) deallocate(combined)
    end function calculate_chunk_crc

    ! Removed unused chunk writer helpers; buffer-based writer is used instead




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

    ! calculate_crc32 helper removed with unused writer



end module fortplot_png
