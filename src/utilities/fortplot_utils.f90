module fortplot_utils
    !! Utility functions module for common operations
    !! 
    !! This module provides utility functions that are used across
    !! multiple plotting modules. Follows Interface Segregation Principle
    !! by grouping related utility functions together.
    
    use fortplot_context, only: plot_context
    use fortplot_string_utils, only: to_lowercase
    use fortplot_png, only: create_png_canvas
    use fortplot_pdf, only: create_pdf_canvas
    use fortplot_ascii, only: create_ascii_canvas
    use fortplot_constants, only: MAX_SAFE_PIXELS
    implicit none
    
    private
    public :: get_backend_from_filename, initialize_backend
    public :: ensure_directory_exists
    
contains

    function get_backend_from_filename(filename) result(backend_type)
        !! Determine backend type from file extension
        !! 
        !! @param filename: Output filename
        !! @return backend_type: Backend identifier string
        
        character(len=*), intent(in) :: filename
        character(len=20) :: backend_type
        character(len=10) :: extension
        integer :: dot_pos
        
        ! Find the last dot in filename
        dot_pos = index(filename, '.', back=.true.)
        
        if (dot_pos > 0) then
            extension = to_lowercase(filename(dot_pos+1:))
            select case (trim(extension))
            case ('png')
                backend_type = 'png'
            case ('pdf')
                backend_type = 'pdf'
            case ('txt')
                backend_type = 'ascii'
            case ('dat')
                backend_type = 'ascii'
            case default
                backend_type = 'png'  ! Default fallback
            end select
        else
            backend_type = 'png'  ! Default if no extension
        end if
    end function get_backend_from_filename

    subroutine initialize_backend(backend, backend_type, width, height)
        !! Initialize the appropriate backend based on type
        !! 
        !! @param backend: Polymorphic backend to initialize
        !! @param backend_type: Type of backend ('png', 'pdf', 'ascii')
        !! @param width: Canvas width
        !! @param height: Canvas height
        
        class(plot_context), allocatable, intent(out) :: backend
        character(len=*), intent(in) :: backend_type
        integer, intent(in) :: width, height
        
        select case (trim(backend_type))
        case ('png')
            ! Validate dimensions to prevent raster backend crashes
            ! Allow up to MAX_SAFE_PIXELS (matches matplotlib figure limits)
            if (width > MAX_SAFE_PIXELS .or. height > MAX_SAFE_PIXELS .or. width <= 0 .or. height <= 0) then
                print *, "WARNING: PNG backend dimensions invalid or too large:", width, "x", height
                print *, "Falling back to PDF backend for this file"
                allocate(backend, source=create_pdf_canvas(min(max(width, 800), 1920), min(max(height, 600), 1080)))
            else
                allocate(backend, source=create_png_canvas(width, height))
            end if
        case ('pdf')
            allocate(backend, source=create_pdf_canvas(width, height))
        case ('ascii')
            allocate(backend, source=create_ascii_canvas(width, height))
        case default
            ! Default to PNG with dimension validation
            if (width > MAX_SAFE_PIXELS .or. height > MAX_SAFE_PIXELS .or. width <= 0 .or. height <= 0) then
                allocate(backend, source=create_pdf_canvas(min(max(width, 800), 1920), min(max(height, 600), 1080)))
            else
                allocate(backend, source=create_png_canvas(width, height))
            end if
        end select
    end subroutine initialize_backend

    ! Note: to_lowercase and boolean env parsing are provided by
    ! src/utilities/core/fortplot_string_utils.f90

    subroutine ensure_directory_exists(filepath)
        !! Ensure output directory exists for a given filepath
        !! Extracts the parent directory and creates it securely using
        !! the runtime file operations with path validation.
        use fortplot_file_operations, only: create_directory_runtime, check_directory_exists
        character(len=*), intent(in) :: filepath
        character(len=512) :: dir
        integer :: i
        logical :: success, exists

        dir = ''

        ! Find last path separator (both '/' and '\\' supported)
        do i = len_trim(filepath), 1, -1
            if (filepath(i:i) == '/' .or. filepath(i:i) == '\\') then
                if (i > 1) dir = filepath(1:i-1)
                exit
            end if
        end do

        ! If no directory component, nothing to do
        if (len_trim(dir) == 0) return

        ! If already exists, we're done
        call check_directory_exists(trim(dir), exists)
        if (exists) return

        ! Create directory tree with security validation/whitelisting
        call create_directory_runtime(trim(dir), success)
        ! Intentionally no error STOP here; upstream will report failures
    end subroutine ensure_directory_exists

end module fortplot_utils
