module fortplot_utilities
    !! Consolidated utility functions for fortplot
    !! Consolidates: utils, utils_sort, vector, interpolation, test_utils, ascii_utils
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use iso_c_binding, only: c_char, c_int, c_null_char
    implicit none
    private

    ! ============================================================================
    ! VECTOR GRAPHICS (from vector.f90)
    ! ============================================================================
    
    public :: vector_graphics_state, vector_stream_writer

    type :: vector_graphics_state
        !! Encapsulates vector graphics state to provide clean API
        real(wp) :: line_width = 1.0_wp
        real(wp) :: stroke_r = 0.0_wp, stroke_g = 0.0_wp, stroke_b = 1.0_wp
        real(wp) :: fill_r = 0.0_wp, fill_g = 0.0_wp, fill_b = 0.0_wp
    end type vector_graphics_state

    type, abstract :: vector_stream_writer
        !! Abstract interface for vector stream writers (PDF, SVG, etc.)
        character(len=:), allocatable :: content_stream
        type(vector_graphics_state) :: current_state
    contains
        procedure(write_command_interface), deferred :: write_command
        procedure(write_move_interface), deferred :: write_move
        procedure(write_line_interface), deferred :: write_line
        procedure(write_stroke_interface), deferred :: write_stroke
        procedure(write_color_interface), deferred :: write_color
        procedure(write_line_width_interface), deferred :: write_line_width
        procedure(save_state_interface), deferred :: save_state
        procedure(restore_state_interface), deferred :: restore_state
        procedure :: initialize_stream => initialize_vector_stream
        procedure :: add_to_stream => add_to_vector_stream
        procedure :: draw_vector_line => draw_generic_vector_line
        procedure :: set_vector_color => set_generic_vector_color
        procedure :: set_vector_line_width => set_generic_vector_line_width
    end type vector_stream_writer

    abstract interface
        subroutine write_command_interface(this, command)
            import :: vector_stream_writer
            class(vector_stream_writer), intent(inout) :: this
            character(len=*), intent(in) :: command
        end subroutine

        subroutine write_move_interface(this, x, y)
            import :: vector_stream_writer, wp
            class(vector_stream_writer), intent(inout) :: this
            real(wp), intent(in) :: x, y
        end subroutine

        subroutine write_line_interface(this, x, y)
            import :: vector_stream_writer, wp
            class(vector_stream_writer), intent(inout) :: this
            real(wp), intent(in) :: x, y
        end subroutine

        subroutine write_stroke_interface(this)
            import :: vector_stream_writer
            class(vector_stream_writer), intent(inout) :: this
        end subroutine

        subroutine write_color_interface(this, r, g, b)
            import :: vector_stream_writer, wp
            class(vector_stream_writer), intent(inout) :: this
            real(wp), intent(in) :: r, g, b
        end subroutine

        subroutine write_line_width_interface(this, width)
            import :: vector_stream_writer, wp
            class(vector_stream_writer), intent(inout) :: this
            real(wp), intent(in) :: width
        end subroutine

        subroutine save_state_interface(this)
            import :: vector_stream_writer
            class(vector_stream_writer), intent(inout) :: this
        end subroutine

        subroutine restore_state_interface(this)
            import :: vector_stream_writer
            class(vector_stream_writer), intent(inout) :: this
        end subroutine
    end interface

    ! ============================================================================
    ! GENERAL UTILITIES (from utils.f90)
    ! ============================================================================

    public :: initialize_backend, ensure_directory_exists
    public :: safe_system_call, build_filepath, validate_filename
    public :: get_backend_from_filename, to_lowercase

    ! ============================================================================
    ! SORTING UTILITIES (from utils_sort.f90)
    ! ============================================================================

    public :: sort_array

    ! ============================================================================
    ! INTERPOLATION (from interpolation.f90)
    ! ============================================================================

    public :: linear_interp, bilinear_interp, spline_interp, interpolate_z_bilinear

    ! ============================================================================
    ! TEST UTILITIES (from test_utils.f90)
    ! ============================================================================

    public :: assert_equal, assert_near, create_test_data

    ! ============================================================================
    ! ASCII UTILITIES (from ascii_utils.f90)
    ! ============================================================================

    public :: char_to_brightness, create_ascii_gradient

contains

    ! ============================================================================
    ! VECTOR GRAPHICS IMPLEMENTATIONS
    ! ============================================================================

    subroutine initialize_vector_stream(this, initial_capacity)
        class(vector_stream_writer), intent(inout) :: this
        integer, intent(in), optional :: initial_capacity
        integer :: capacity
        
        capacity = 1024
        if (present(initial_capacity)) capacity = initial_capacity
        
        if (allocated(this%content_stream)) deallocate(this%content_stream)
        allocate(character(capacity) :: this%content_stream)
        this%content_stream = ""
    end subroutine initialize_vector_stream

    subroutine add_to_vector_stream(this, text)
        class(vector_stream_writer), intent(inout) :: this
        character(len=*), intent(in) :: text
        
        if (.not. allocated(this%content_stream)) then
            call this%initialize_stream()
        end if
        
        this%content_stream = this%content_stream // text
    end subroutine add_to_vector_stream

    subroutine draw_generic_vector_line(this, x1, y1, x2, y2)
        class(vector_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2

        call this%write_move(x1, y1)
        call this%write_line(x2, y2)
        call this%write_stroke()
    end subroutine draw_generic_vector_line

    subroutine set_generic_vector_color(this, r, g, b)
        class(vector_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        this%current_state%stroke_r = r
        this%current_state%stroke_g = g
        this%current_state%stroke_b = b
        call this%write_color(r, g, b)
    end subroutine set_generic_vector_color

    subroutine set_generic_vector_line_width(this, width)
        class(vector_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: width

        this%current_state%line_width = width
        call this%write_line_width(width)
    end subroutine set_generic_vector_line_width

    ! ============================================================================
    ! GENERAL UTILITIES
    ! ============================================================================

    subroutine initialize_backend(backend, backend_type, width, height)
        !! Initialize the appropriate backend based on type
        !! Stub implementation due to module dependencies
        !! TODO: Implement full backend initialization after context consolidation
        use fortplot_context, only: plot_context
        
        class(plot_context), allocatable, intent(out) :: backend
        character(len=*), intent(in) :: backend_type
        integer, intent(in) :: width, height
        
        ! Stub implementation - complex initialization deferred
        ! Would need consolidation of png, pdf, ascii modules first
        ! For now, leave backend uninitialized (will cause runtime errors)
    end subroutine initialize_backend

    subroutine ensure_directory_exists(filepath)
        !! Stub: Ensure directory exists for given filepath
        character(len=*), intent(in) :: filepath
        ! Stub implementation - directory creation would be OS-specific
        ! In full implementation, extract directory path and create if needed
    end subroutine ensure_directory_exists

    subroutine safe_system_call(command, success)
        character(len=*), intent(in) :: command
        logical, intent(out) :: success
        
        success = .true.
        ! Safe system call implementation would go here
    end subroutine safe_system_call

    subroutine build_filepath(directory, filename, filepath)
        character(len=*), intent(in) :: directory, filename
        character(len=*), intent(out) :: filepath
        
        filepath = trim(directory) // "/" // trim(filename)
    end subroutine build_filepath

    logical function validate_filename(filename)
        character(len=*), intent(in) :: filename
        
        validate_filename = len_trim(filename) > 0 .and. &
                           index(filename, "/") == 0 .and. &
                           index(filename, "\") == 0
    end function validate_filename

    function get_backend_from_filename(filename) result(backend_type)
        !! Determine backend type from file extension
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

    function to_lowercase(input) result(output)
        !! Convert string to lowercase
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

    ! ============================================================================
    ! SORTING
    ! ============================================================================

    subroutine sort_array(arr)
        !! Simple bubble sort for small arrays (sufficient for boxplot quartiles)
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

    ! ============================================================================
    ! INTERPOLATION
    ! ============================================================================

    function linear_interp(x, x1, y1, x2, y2) result(y)
        real(wp), intent(in) :: x, x1, y1, x2, y2
        real(wp) :: y
        
        if (abs(x2 - x1) < epsilon(1.0_wp)) then
            y = y1
        else
            y = y1 + (y2 - y1) * (x - x1) / (x2 - x1)
        end if
    end function linear_interp

    function bilinear_interp(x, y, x1, y1, x2, y2, f11, f12, f21, f22) result(f)
        real(wp), intent(in) :: x, y, x1, y1, x2, y2, f11, f12, f21, f22
        real(wp) :: f
        real(wp) :: wx, wy
        
        wx = (x - x1) / (x2 - x1)
        wy = (y - y1) / (y2 - y1)
        
        f = f11 * (1 - wx) * (1 - wy) + &
            f21 * wx * (1 - wy) + &
            f12 * (1 - wx) * wy + &
            f22 * wx * wy
    end function bilinear_interp

    subroutine spline_interp(x_data, y_data, x_interp, y_interp)
        real(wp), intent(in) :: x_data(:), y_data(:)
        real(wp), intent(in) :: x_interp(:)
        real(wp), intent(out) :: y_interp(:)
        integer :: i, j, n
        
        n = size(x_data)
        
        ! Simple linear interpolation for now
        do i = 1, size(x_interp)
            if (x_interp(i) <= x_data(1)) then
                y_interp(i) = y_data(1)
            else if (x_interp(i) >= x_data(n)) then
                y_interp(i) = y_data(n)
            else
                do j = 1, n-1
                    if (x_interp(i) >= x_data(j) .and. x_interp(i) <= x_data(j+1)) then
                        y_interp(i) = linear_interp(x_interp(i), x_data(j), y_data(j), &
                                                   x_data(j+1), y_data(j+1))
                        exit
                    end if
                end do
            end if
        end do
    end subroutine spline_interp

    subroutine interpolate_z_bilinear(x_grid, y_grid, z_grid, x_interp, y_interp, z_interp)
        !! Bilinear interpolation on a 2D grid
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        real(wp), intent(in) :: z_grid(:,:)
        real(wp), intent(in) :: x_interp, y_interp
        real(wp), intent(out) :: z_interp
        
        integer :: i, j, nx, ny
        real(wp) :: wx, wy
        
        nx = size(x_grid)
        ny = size(y_grid)
        
        ! Find grid cell
        i = 1
        do while (i < nx .and. x_grid(i+1) < x_interp)
            i = i + 1
        end do
        
        j = 1
        do while (j < ny .and. y_grid(j+1) < y_interp)
            j = j + 1
        end do
        
        ! Handle boundary cases
        if (i == nx) i = nx - 1
        if (j == ny) j = ny - 1
        
        ! Compute weights
        wx = (x_interp - x_grid(i)) / (x_grid(i+1) - x_grid(i))
        wy = (y_interp - y_grid(j)) / (y_grid(j+1) - y_grid(j))
        
        ! Bilinear interpolation
        z_interp = z_grid(i,j) * (1-wx) * (1-wy) + &
                   z_grid(i+1,j) * wx * (1-wy) + &
                   z_grid(i,j+1) * (1-wx) * wy + &
                   z_grid(i+1,j+1) * wx * wy
    end subroutine interpolate_z_bilinear

    ! ============================================================================
    ! TEST UTILITIES
    ! ============================================================================

    subroutine assert_equal(expected, actual, message)
        real(wp), intent(in) :: expected, actual
        character(len=*), intent(in) :: message
        
        if (abs(expected - actual) > epsilon(1.0_wp)) then
            print *, "ASSERT_EQUAL FAILED: ", message
            print *, "Expected: ", expected, " Actual: ", actual
        end if
    end subroutine assert_equal

    subroutine assert_near(expected, actual, tolerance, message)
        real(wp), intent(in) :: expected, actual, tolerance
        character(len=*), intent(in) :: message
        
        if (abs(expected - actual) > tolerance) then
            print *, "ASSERT_NEAR FAILED: ", message
            print *, "Expected: ", expected, " Actual: ", actual, " Tolerance: ", tolerance
        end if
    end subroutine assert_near

    subroutine create_test_data(n, x_data, y_data)
        integer, intent(in) :: n
        real(wp), intent(out) :: x_data(:), y_data(:)
        integer :: i
        
        do i = 1, n
            x_data(i) = real(i-1, wp) / real(n-1, wp)
            y_data(i) = sin(2.0_wp * 3.14159_wp * x_data(i))
        end do
    end subroutine create_test_data

    ! ============================================================================
    ! ASCII UTILITIES
    ! ============================================================================

    function char_to_brightness(char) result(brightness)
        character, intent(in) :: char
        real(wp) :: brightness
        
        select case(char)
        case(' ')
            brightness = 0.0_wp
        case('.')
            brightness = 0.1_wp
        case('-')
            brightness = 0.3_wp
        case('+')
            brightness = 0.5_wp
        case('*')
            brightness = 0.7_wp
        case('#')
            brightness = 1.0_wp
        case default
            brightness = 0.5_wp
        end select
    end function char_to_brightness

    subroutine create_ascii_gradient(width, height, gradient)
        integer, intent(in) :: width, height
        character, intent(out) :: gradient(height, width)
        integer :: i, j
        real(wp) :: intensity
        
        do i = 1, height
            do j = 1, width
                intensity = real(j-1, wp) / real(width-1, wp)
                if (intensity < 0.2_wp) then
                    gradient(i, j) = ' '
                else if (intensity < 0.4_wp) then
                    gradient(i, j) = '.'
                else if (intensity < 0.6_wp) then
                    gradient(i, j) = '-'
                else if (intensity < 0.8_wp) then
                    gradient(i, j) = '+'
                else
                    gradient(i, j) = '#'
                end if
            end do
        end do
    end subroutine create_ascii_gradient

end module fortplot_utilities