module fortplot_jpeg
    use iso_c_binding
    use fortplot_raster, only: raster_context, initialize_raster_backend, draw_axes_and_labels, draw_rotated_ylabel_raster
    use, intrinsic :: iso_fortran_env, only: wp => real64, int8, int32
    implicit none

    private
    public :: jpeg_context, create_jpeg_canvas, write_jpeg_file, get_jpeg_data

    ! JPEG plotting context - extends raster context and adds JPEG-specific functionality
    type, extends(raster_context) :: jpeg_context
        integer :: quality = 85  ! JPEG quality (0-100)
    contains
        procedure :: save => jpeg_finalize
        procedure :: set_quality => jpeg_set_quality
    end type jpeg_context

contains

    function create_jpeg_canvas(width, height, quality) result(ctx)
        integer, intent(in) :: width, height
        integer, intent(in), optional :: quality
        type(jpeg_context) :: ctx

        ! Use common raster backend initialization
        call initialize_raster_backend(ctx, width, height)
        
        ! Set JPEG-specific quality
        if (present(quality)) then
            ctx%quality = quality
        else
            ctx%quality = 85  ! Default quality
        end if
    end function create_jpeg_canvas

    subroutine jpeg_set_quality(this, quality)
        class(jpeg_context), intent(inout) :: this
        integer, intent(in) :: quality
        
        ! Clamp quality to valid range
        this%quality = max(1, min(100, quality))
    end subroutine jpeg_set_quality

    ! All drawing methods are inherited from raster_context

    subroutine jpeg_finalize(this, filename)
        class(jpeg_context), intent(inout) :: this
        character(len=*), intent(in) :: filename

        call write_jpeg_file(filename, this%width, this%height, this%raster%image_data, this%quality)
    end subroutine jpeg_finalize

    ! Generate JPEG data from image data
    subroutine generate_jpeg_data(width, height, image_data, quality, jpeg_buffer)
        integer, intent(in) :: width, height, quality
        integer(1), intent(in) :: image_data(:)
        integer(1), allocatable, intent(out) :: jpeg_buffer(:)

        ! For now, create a minimal JPEG-like structure
        ! This is a simplified implementation - real JPEG would need proper encoding
        integer :: buffer_size
        integer :: i
        
        ! Estimate buffer size (simplified)
        buffer_size = width * height / 4  ! Rough compression estimate
        allocate(jpeg_buffer(buffer_size))
        
        ! Create simple JPEG header (SOI marker)
        jpeg_buffer(1) = int(Z'FF', 1)
        jpeg_buffer(2) = int(Z'D8', 1)
        
        ! Simple data encoding (placeholder - real implementation would use DCT)
        do i = 3, min(buffer_size, size(image_data) + 2)
            if (i - 2 <= size(image_data)) then
                jpeg_buffer(i) = image_data(i - 2)
            else
                jpeg_buffer(i) = 0_1
            end if
        end do
        
        ! Add EOI marker at end
        if (buffer_size >= 2) then
            jpeg_buffer(buffer_size - 1) = int(Z'FF', 1)
            jpeg_buffer(buffer_size) = int(Z'D9', 1)
        end if
    end subroutine generate_jpeg_data

    ! Write JPEG data to file
    subroutine write_jpeg_file(filename, width, height, image_data, quality)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height, quality
        integer(1), intent(in) :: image_data(:)
        
        integer(1), allocatable :: jpeg_buffer(:)
        integer :: jpeg_unit
        
        call generate_jpeg_data(width, height, image_data, quality, jpeg_buffer)
        
        open(newunit=jpeg_unit, file=filename, access='stream', form='unformatted', status='replace')
        write(jpeg_unit) jpeg_buffer
        close(jpeg_unit)
        
        deallocate(jpeg_buffer)
        print *, "JPEG file '", trim(filename), "' created successfully!"
    end subroutine write_jpeg_file

    ! Public wrapper for getting JPEG data
    subroutine get_jpeg_data(width, height, image_data, quality, jpeg_buffer)
        integer, intent(in) :: width, height, quality
        integer(1), intent(in) :: image_data(:)
        integer(1), allocatable, intent(out) :: jpeg_buffer(:)
        
        call generate_jpeg_data(width, height, image_data, quality, jpeg_buffer)
    end subroutine get_jpeg_data

end module fortplot_jpeg