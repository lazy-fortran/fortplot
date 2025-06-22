module fortplot_utils
    !! Utility functions module for common operations
    !! 
    !! This module provides utility functions that are used across
    !! multiple plotting modules. Follows Interface Segregation Principle
    !! by grouping related utility functions together.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_png
    use fortplot_pdf  
    use fortplot_ascii
    implicit none
    
    private
    public :: get_backend_from_filename, initialize_backend
    public :: to_lowercase, sort_array
    public :: expand_range, estimate_text_width
    
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
            allocate(png_context :: backend)
        case ('pdf')
            allocate(pdf_context :: backend)
        case ('ascii')
            allocate(ascii_context :: backend)
        case default
            allocate(png_context :: backend)
        end select
        
        call setup_canvas(backend, width, height)
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

    subroutine sort_array(arr)
        !! Simple bubble sort for small arrays
        !! 
        !! @param arr: Array to sort in place
        
        real(wp), intent(inout) :: arr(:)
        integer :: i, j, n
        real(wp) :: temp
        
        n = size(arr)
        do i = 1, n-1
            do j = 1, n-i
                if (arr(j) > arr(j+1)) then
                    temp = arr(j)
                    arr(j) = arr(j+1)
                    arr(j+1) = temp
                end if
            end do
        end do
    end subroutine sort_array

    subroutine expand_range(range_min, range_max, factor)
        !! Expand a data range by a given factor
        !! 
        !! @param range_min: Minimum value (modified in place)
        !! @param range_max: Maximum value (modified in place)
        !! @param factor: Expansion factor (e.g., 0.1 for 10% expansion)
        
        real(wp), intent(inout) :: range_min, range_max
        real(wp), intent(in) :: factor
        real(wp) :: range_size, expansion
        
        range_size = range_max - range_min
        if (range_size > 0.0_wp) then
            expansion = range_size * factor
            range_min = range_min - expansion
            range_max = range_max + expansion
        end if
    end subroutine expand_range

    function estimate_text_width(text, font_size) result(width)
        !! Estimate text width in pixels
        !! Simple approximation based on character count and font size
        !! 
        !! @param text: Text string
        !! @param font_size: Font size in points
        !! @return width: Estimated width in pixels
        
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: font_size
        real(wp) :: width
        
        ! Simple approximation: average character width ≈ 0.6 * font_size
        width = len_trim(text) * font_size * 0.6_wp
    end function estimate_text_width

end module fortplot_utils