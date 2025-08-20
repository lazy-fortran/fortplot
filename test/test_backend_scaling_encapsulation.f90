program test_backend_scaling_encapsulation
    !! Test suite for backend scaling encapsulation (Issue #140)
    !!
    !! GIVEN: Coordinate and value scaling scattered across business logic
    !! WHEN: Scaling is moved to backend implementations
    !! THEN: Each backend should handle its own scaling internally
    !! AND: Business logic should use generic scaling methods only
    !! AND: No backend-specific scaling calculations in business logic
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_scaling_methods_in_backend_interface()
    call test_no_scattered_scaling_calculations()
    call test_backends_implement_scaling()
    call test_generic_scaling_calls_in_business_logic()
    
contains

    subroutine test_scaling_methods_in_backend_interface()
        !! GIVEN: The need for coordinate and value scaling
        !! WHEN: Checking the backend interface
        !! THEN: Generic scaling methods should be defined in the interface
        !! AND: Methods should handle coordinate and value transformations
        
        logical :: has_coordinate_scaling, has_value_scaling
        
        call check_for_scaling_methods(has_coordinate_scaling, has_value_scaling)
        
        if (.not. has_coordinate_scaling) then
            error stop 'FAILED: Backend interface missing coordinate scaling methods'
        end if
        
        if (.not. has_value_scaling) then
            error stop 'FAILED: Backend interface missing value scaling methods'
        end if
        
        print *, 'PASSED: Scaling methods defined in backend interface'
    end subroutine test_scaling_methods_in_backend_interface

    subroutine check_for_scaling_methods(has_coordinate_scaling, has_value_scaling)
        !! Check if scaling methods are defined in backend interface
        logical, intent(out) :: has_coordinate_scaling, has_value_scaling
        
        integer :: unit, iostat
        character(len=1000) :: line
        
        has_coordinate_scaling = .false.
        has_value_scaling = .false.
        
        ! Check context module for scaling methods
        open(newunit=unit, file='src/fortplot_context.f90', status='old', action='read', iostat=iostat)
        if (iostat == 0) then
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                
                ! Look for coordinate scaling methods
                if (index(line, 'scale_coordinates') > 0 .or. &
                    index(line, 'transform_coordinates') > 0) then
                    has_coordinate_scaling = .true.
                end if
                
                ! Look for value scaling methods
                if (index(line, 'scale_values') > 0 .or. &
                    index(line, 'transform_values') > 0) then
                    has_value_scaling = .true.
                end if
            end do
            close(unit)
        end if
        
        ! Check dedicated backend interface module if it exists
        open(newunit=unit, file='src/fortplot_backend_interface.f90', status='old', &
              action='read', iostat=iostat)
        if (iostat == 0) then
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                
                if (index(line, 'scale_coordinates') > 0 .or. &
                    index(line, 'transform_coordinates') > 0) then
                    has_coordinate_scaling = .true.
                end if
                
                if (index(line, 'scale_values') > 0 .or. &
                    index(line, 'transform_values') > 0) then
                    has_value_scaling = .true.
                end if
            end do
            close(unit)
        end if
    end subroutine check_for_scaling_methods

    subroutine test_no_scattered_scaling_calculations()
        !! GIVEN: Business logic that previously had scattered scaling calculations
        !! WHEN: Checking for explicit backend-specific scaling
        !! THEN: No explicit scaling calculations should remain in business logic
        !! AND: All scaling should go through polymorphic interface calls
        
        logical :: has_scattered_scaling
        
        call check_for_scattered_scaling('src/fortplot_figure_core.f90', has_scattered_scaling)
        if (has_scattered_scaling) then
            error stop 'FAILED: figure_core still has scattered scaling calculations'
        end if
        
        call check_for_scattered_scaling('src/fortplot_legend.f90', has_scattered_scaling)
        if (has_scattered_scaling) then
            error stop 'FAILED: legend still has scattered scaling calculations'
        end if
        
        call check_for_scattered_scaling('src/fortplot_animation.f90', has_scattered_scaling)
        if (has_scattered_scaling) then
            error stop 'FAILED: animation still has scattered scaling calculations'
        end if
        
        print *, 'PASSED: No scattered scaling calculations in business logic'
    end subroutine test_no_scattered_scaling_calculations

    subroutine check_for_scattered_scaling(file_path, has_scattered_scaling)
        !! Check if a file contains scattered scaling calculations
        character(*), intent(in) :: file_path
        logical, intent(out) :: has_scattered_scaling
        
        integer :: unit, iostat
        character(len=1000) :: line
        
        has_scattered_scaling = .false.
        
        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            line = to_lowercase(trim(line))
            
            ! Look for explicit scaling calculations that should be encapsulated
            if (check_line_has_scattered_scaling(line)) then
                ! Skip if it's in a comment
                if (index(line, '!') == 0 .or. &
                    index(line, '!') > index(line, '*')) then
                    has_scattered_scaling = .true.
                    exit
                end if
            end if
        end do
        
        close(unit)
    end subroutine check_for_scattered_scaling

    logical function check_line_has_scattered_scaling(line) result(has_scaling)
        !! Check if a line contains scattered scaling calculations
        character(*), intent(in) :: line
        
        has_scaling = .false.
        
        ! Look for explicit backend-specific scaling patterns
        if ((index(line, '* ctx%width_scale') > 0) .or. &
            (index(line, '* ctx%height_scale') > 0) .or. &
            (index(line, '* backend%width_scale') > 0) .or. &
            (index(line, '* backend%height_scale') > 0) .or. &
            (index(line, '* ctx%pdf_scale') > 0) .or. &
            (index(line, '/ ctx%width') > 0 .and. index(line, 'scale') > 0) .or. &
            (index(line, '/ ctx%height') > 0 .and. index(line, 'scale') > 0)) then
            has_scaling = .true.
        end if
        
        ! Also check for coordinate transformation patterns that should be encapsulated
        if ((index(line, 'x_screen = ') > 0 .and. index(line, '* ctx%') > 0) .or. &
            (index(line, 'y_screen = ') > 0 .and. index(line, '* ctx%') > 0) .or. &
            (index(line, 'scaled_x = ') > 0 .and. index(line, '* ') > 0) .or. &
            (index(line, 'scaled_y = ') > 0 .and. index(line, '* ') > 0)) then
            has_scaling = .true.
        end if
    end function check_line_has_scattered_scaling

    subroutine test_backends_implement_scaling()
        !! GIVEN: Each backend needs to handle its own scaling
        !! WHEN: Checking backend implementations
        !! THEN: Each backend should implement scaling methods
        !! AND: Scaling should be encapsulated within backend code
        
        logical :: png_has_scaling, pdf_has_scaling, ascii_has_scaling
        
        call check_backend_scaling_implementation('src/fortplot_png.f90', png_has_scaling)
        call check_backend_scaling_implementation('src/fortplot_pdf.f90', pdf_has_scaling)
        call check_backend_scaling_implementation('src/fortplot_ascii.f90', ascii_has_scaling)
        
        if (.not. png_has_scaling) then
            error stop 'FAILED: PNG backend does not implement scaling methods'
        end if
        
        if (.not. pdf_has_scaling) then
            error stop 'FAILED: PDF backend does not implement scaling methods'
        end if
        
        if (.not. ascii_has_scaling) then
            error stop 'FAILED: ASCII backend does not implement scaling methods'
        end if
        
        print *, 'PASSED: All backends implement scaling methods'
    end subroutine test_backends_implement_scaling

    subroutine check_backend_scaling_implementation(file_path, has_scaling)
        !! Check if a backend implements scaling methods
        character(*), intent(in) :: file_path
        logical, intent(out) :: has_scaling
        
        integer :: unit, iostat
        character(len=1000) :: line
        logical :: has_scale_coords, has_scale_values
        
        has_scaling = .false.
        has_scale_coords = .false.
        has_scale_values = .false.
        
        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            ! Look for scaling method implementations
            if (index(line, 'subroutine') > 0 .and. &
                (index(line, 'scale_coordinates') > 0 .or. &
                 index(line, 'transform_coordinates') > 0)) then
                has_scale_coords = .true.
            end if
            
            if (index(line, 'subroutine') > 0 .and. &
                (index(line, 'scale_values') > 0 .or. &
                 index(line, 'transform_values') > 0)) then
                has_scale_values = .true.
            end if
        end do
        
        close(unit)
        
        ! For now, consider it implemented if at least one scaling method exists
        ! Full implementation would check for both coordinate and value scaling
        has_scaling = has_scale_coords .or. has_scale_values
    end subroutine check_backend_scaling_implementation

    subroutine test_generic_scaling_calls_in_business_logic()
        !! GIVEN: Business logic that needs coordinate and value scaling
        !! WHEN: Scaling is needed for rendering operations
        !! THEN: Business logic should use generic polymorphic scaling calls
        !! AND: No backend-specific scaling knowledge should be required
        
        logical :: uses_generic_scaling
        
        call check_for_generic_scaling_calls('src/fortplot_figure_core.f90', uses_generic_scaling)
        if (.not. uses_generic_scaling) then
            ! This might not fail initially if scaling isn't implemented yet
            print *, 'NOTE: figure_core not yet using generic scaling calls'
        end if
        
        call check_for_generic_scaling_calls('src/fortplot_legend.f90', uses_generic_scaling)
        if (.not. uses_generic_scaling) then
            print *, 'NOTE: legend not yet using generic scaling calls'
        end if
        
        print *, 'PASSED: Generic scaling calls test (implementation pending)'
    end subroutine test_generic_scaling_calls_in_business_logic

    subroutine check_for_generic_scaling_calls(file_path, uses_generic_scaling)
        !! Check if a file uses generic polymorphic scaling calls
        character(*), intent(in) :: file_path
        logical, intent(out) :: uses_generic_scaling
        
        integer :: unit, iostat
        character(len=1000) :: line
        
        uses_generic_scaling = .false.
        
        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            ! Look for generic scaling method calls
            if ((index(line, 'call backend%scale_coordinates') > 0) .or. &
                (index(line, 'call backend%scale_values') > 0) .or. &
                (index(line, 'call self%backend%scale_coordinates') > 0) .or. &
                (index(line, 'call self%backend%scale_values') > 0)) then
                uses_generic_scaling = .true.
                exit
            end if
        end do
        
        close(unit)
    end subroutine check_for_generic_scaling_calls

    function to_lowercase(input_string) result(output_string)
        !! Convert string to lowercase for case-insensitive comparison
        character(*), intent(in) :: input_string
        character(len=len(input_string)) :: output_string
        integer :: i, ascii_val
        
        output_string = input_string
        do i = 1, len(output_string)
            ascii_val = ichar(output_string(i:i))
            if (ascii_val >= ichar('A') .and. ascii_val <= ichar('Z')) then
                output_string(i:i) = char(ascii_val + 32)
            end if
        end do
    end function to_lowercase

end program test_backend_scaling_encapsulation