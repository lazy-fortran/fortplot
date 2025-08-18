module fortplot_utils
    !! Utility functions module for common operations
    !! 
    !! This module provides utility functions that are used across
    !! multiple plotting modules. Follows Interface Segregation Principle
    !! by grouping related utility functions together.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_png, only: create_png_canvas
    use fortplot_pdf, only: create_pdf_canvas
    use fortplot_ascii, only: create_ascii_canvas
    use fortplot_gltf, only: create_gltf_canvas
    implicit none
    
    private
    public :: get_backend_from_filename, initialize_backend
    public :: to_lowercase
    
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
            case ('gltf')
                backend_type = 'gltf'
            case ('glb')
                backend_type = 'glb'
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
            allocate(backend, source=create_png_canvas(width, height))
        case ('pdf')
            allocate(backend, source=create_pdf_canvas(width, height))
        case ('ascii')
            allocate(backend, source=create_ascii_canvas(width, height))
        case ('gltf', 'glb')
            allocate(backend, source=create_gltf_canvas(width, height))
        case default
            allocate(backend, source=create_png_canvas(width, height))
        end select
    end subroutine initialize_backend

    function to_lowercase(input) result(output)
        !! Convert string to lowercase
        !! 
        !! @param input: Input string
        !! @return output: Lowercase string
        
        character(len=*), intent(in) :: input
        character(len=len(input)) :: output
        integer :: i, char_code
        
        do i = 1, len(input)
            char_code = iachar(input(i:i))
            if (char_code >= 65 .and. char_code <= 90) then
                ! Convert uppercase A-Z to lowercase a-z
                output(i:i) = achar(char_code + 32)
            else
                output(i:i) = input(i:i)
            end if
        end do
    end function to_lowercase


end module fortplot_utils