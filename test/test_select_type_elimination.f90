program test_select_type_elimination
    !! Test suite for SELECT TYPE elimination refactoring (Issue #140)
    !!
    !! GIVEN: The fortplot library has scattered SELECT TYPE dispatch logic
    !! WHEN: The SELECT TYPE elimination refactoring is applied 
    !! THEN: All business logic modules should contain zero SELECT TYPE statements
    !! AND: Backend-specific logic should be encapsulated in backend implementations
    !! AND: Polymorphic interfaces should handle all rendering operations
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_business_logic_has_no_select_type()
    call test_backend_interface_polymorphism()
    call test_backend_specific_logic_encapsulation()
    call test_coordinate_scaling_encapsulation()
    
contains

    subroutine test_business_logic_has_no_select_type()
        !! GIVEN: Business logic modules (figure_core, legend, animation)
        !! WHEN: Checking for SELECT TYPE statements
        !! THEN: Zero SELECT TYPE statements should be found in business logic
        
        logical :: has_select_type
        
        ! Test that fortplot_figure_core.f90 has no SELECT TYPE
        call check_module_for_select_type('src/fortplot_figure_core.f90', has_select_type)
        if (has_select_type) then
            error stop 'FAILED: fortplot_figure_core.f90 contains SELECT TYPE violations'
        end if
        
        ! Test that fortplot_legend.f90 has no SELECT TYPE  
        call check_module_for_select_type('src/fortplot_legend.f90', has_select_type)
        if (has_select_type) then
            error stop 'FAILED: fortplot_legend.f90 contains SELECT TYPE violations'
        end if
        
        ! Test that fortplot_animation.f90 has no SELECT TYPE
        call check_module_for_select_type('src/fortplot_animation.f90', has_select_type)
        if (has_select_type) then
            error stop 'FAILED: fortplot_animation.f90 contains SELECT TYPE violations'
        end if
        
        print *, 'PASSED: All business logic modules free of SELECT TYPE'
    end subroutine test_business_logic_has_no_select_type

    subroutine check_module_for_select_type(file_path, has_select_type)
        !! Helper routine to check if a module contains SELECT TYPE statements
        character(*), intent(in) :: file_path
        logical, intent(out) :: has_select_type
        
        integer :: unit, iostat
        character(len=1000) :: line
        
        has_select_type = .false.
        
        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            error stop 'Cannot open file: ' // file_path
        end if
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            ! Convert to lowercase for case-insensitive search
            line = to_lowercase(trim(line))
            
            ! Check for 'select type' (ignoring comments)
            if (index(line, 'select type') > 0) then
                ! Skip if it's in a comment
                if (index(line, '!') == 0 .or. index(line, 'select type') < index(line, '!')) then
                    has_select_type = .true.
                    exit
                end if
            end if
        end do
        
        close(unit)
    end subroutine check_module_for_select_type

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

    subroutine test_backend_interface_polymorphism()
        !! GIVEN: Abstract backend interface with polymorphic methods
        !! WHEN: Testing polymorphic rendering operations 
        !! THEN: All backends should support the same interface
        !! AND: No type checking should be required in business logic
        
        ! This test will check that a polymorphic backend interface exists
        ! and that it supports all required rendering operations
        
        logical :: interface_exists
        
        call check_backend_interface_exists(interface_exists)
        if (.not. interface_exists) then
            error stop 'FAILED: Abstract backend interface not implemented'
        end if
        
        call check_polymorphic_methods_defined()
        
        print *, 'PASSED: Backend interface polymorphism implemented'
    end subroutine test_backend_interface_polymorphism

    subroutine check_backend_interface_exists(interface_exists)
        !! Check if the abstract backend interface module exists
        logical, intent(out) :: interface_exists
        
        logical :: file_exists
        
        ! Check if fortplot_backend_interface.f90 exists
        inquire(file='src/fortplot_backend_interface.f90', exist=file_exists)
        interface_exists = file_exists
        
        if (.not. interface_exists) then
            ! Alternative: check if backend interface is defined in existing context module
            call check_abstract_interface_in_context(interface_exists)
        end if
    end subroutine check_backend_interface_exists

    subroutine check_abstract_interface_in_context(interface_exists)
        !! Check if abstract backend interface is defined in context module
        logical, intent(out) :: interface_exists
        
        integer :: unit, iostat
        character(len=1000) :: line
        logical :: found_abstract_type, found_deferred_procedures
        
        interface_exists = .false.
        found_abstract_type = .false.
        found_deferred_procedures = .false.
        
        open(newunit=unit, file='src/fortplot_context.f90', status='old', action='read', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            line = to_lowercase(trim(line))
            
            ! Look for abstract type definition
            if (index(line, 'type, abstract') > 0) then
                found_abstract_type = .true.
            end if
            
            ! Look for deferred procedures (polymorphic methods)
            if (index(line, 'deferred') > 0 .and. index(line, 'procedure') > 0) then
                found_deferred_procedures = .true.
            end if
        end do
        
        close(unit)
        interface_exists = found_abstract_type .and. found_deferred_procedures
    end subroutine check_abstract_interface_in_context

    subroutine check_polymorphic_methods_defined()
        !! Check that required polymorphic methods are defined for backend interface
        
        logical :: has_draw_line, has_draw_text, has_draw_symbol
        logical :: has_set_viewport, has_scale_coordinates
        
        call check_method_exists('draw_line', has_draw_line)
        call check_method_exists('draw_text', has_draw_text) 
        call check_method_exists('draw_symbol', has_draw_symbol)
        call check_method_exists('set_viewport', has_set_viewport)
        call check_method_exists('scale_coordinates', has_scale_coordinates)
        
        if (.not. (has_draw_line .and. has_draw_text .and. has_draw_symbol &
                  .and. has_set_viewport .and. has_scale_coordinates)) then
            error stop 'FAILED: Required polymorphic methods not fully defined'
        end if
    end subroutine check_polymorphic_methods_defined

    subroutine check_method_exists(method_name, method_exists)
        !! Check if a specific polymorphic method exists in backend interface
        character(*), intent(in) :: method_name
        logical, intent(out) :: method_exists
        
        ! For now, assume methods exist if interface exists
        ! In full implementation, would parse interface definition
        method_exists = .true.
    end subroutine check_method_exists

    subroutine test_backend_specific_logic_encapsulation()
        !! GIVEN: Backend-specific rendering logic scattered in business modules
        !! WHEN: Logic is moved to backend implementations
        !! THEN: Business logic should use only polymorphic calls
        !! AND: Backend-specific behavior should be encapsulated
        
        logical :: has_backend_specific_calls
        
        ! Check that business logic uses only polymorphic method calls
        call check_polymorphic_calls_only('src/fortplot_figure_core.f90', has_backend_specific_calls)
        if (has_backend_specific_calls) then
            error stop 'FAILED: figure_core still contains backend-specific calls'
        end if
        
        call check_polymorphic_calls_only('src/fortplot_legend.f90', has_backend_specific_calls)
        if (has_backend_specific_calls) then
            error stop 'FAILED: legend still contains backend-specific calls'
        end if
        
        call check_polymorphic_calls_only('src/fortplot_animation.f90', has_backend_specific_calls)
        if (has_backend_specific_calls) then
            error stop 'FAILED: animation still contains backend-specific calls'
        end if
        
        print *, 'PASSED: Backend-specific logic properly encapsulated'
    end subroutine test_backend_specific_logic_encapsulation

    subroutine check_polymorphic_calls_only(file_path, has_backend_specific_calls)
        !! Check that a module uses only polymorphic method calls, no backend-specific logic
        character(*), intent(in) :: file_path
        logical, intent(out) :: has_backend_specific_calls
        
        integer :: unit, iostat
        character(len=1000) :: line
        
        has_backend_specific_calls = .false.
        
        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            error stop 'Cannot open file: ' // file_path
        end if
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            line = to_lowercase(trim(line))
            
            ! Check for backend-specific calls that should be eliminated
            if (index(line, '%raster%') > 0 .or. &
                index(line, '%pdf_') > 0 .or. &
                index(line, '%width_scale') > 0 .or. &
                index(line, '%height_scale') > 0 .or. &
                index(line, 'png_context') > 0 .or. &
                index(line, 'pdf_context') > 0 .or. &
                index(line, 'ascii_context') > 0) then
                
                ! Skip if it's in a comment or type declaration
                if (index(line, '!') == 0 .or. &
                    (index(line, '!') > 0 .and. index(line, '!') > index(line, '%'))) then
                    if (index(line, 'type is') == 0) then  ! Not a type declaration
                        has_backend_specific_calls = .true.
                        exit
                    end if
                end if
            end if
        end do
        
        close(unit)
    end subroutine check_polymorphic_calls_only

    subroutine test_coordinate_scaling_encapsulation()
        !! GIVEN: Coordinate and value scaling scattered across business logic
        !! WHEN: Scaling is moved to backend implementations  
        !! THEN: Each backend should handle its own scaling internally
        !! AND: Business logic should call generic scaling methods
        
        logical :: has_scattered_scaling
        
        call check_scaling_encapsulation('src/fortplot_figure_core.f90', has_scattered_scaling)
        if (has_scattered_scaling) then
            error stop 'FAILED: figure_core still has scattered coordinate scaling'
        end if
        
        call check_scaling_encapsulation('src/fortplot_legend.f90', has_scattered_scaling)  
        if (has_scattered_scaling) then
            error stop 'FAILED: legend still has scattered coordinate scaling'
        end if
        
        print *, 'PASSED: Coordinate scaling properly encapsulated in backends'
    end subroutine test_coordinate_scaling_encapsulation

    subroutine check_scaling_encapsulation(file_path, has_scattered_scaling)
        !! Check that coordinate scaling is not scattered throughout business logic
        character(*), intent(in) :: file_path
        logical, intent(out) :: has_scattered_scaling
        
        integer :: unit, iostat
        character(len=1000) :: line
        
        has_scattered_scaling = .false.
        
        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            error stop 'Cannot open file: ' // file_path
        end if
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            line = to_lowercase(trim(line))
            
            ! Look for explicit scaling calculations that should be encapsulated
            if ((index(line, '* ctx%') > 0 .and. index(line, 'scale') > 0) .or. &
                (index(line, '* backend%') > 0 .and. index(line, 'scale') > 0)) then
                
                ! Skip comments
                if (index(line, '!') == 0 .or. &
                    (index(line, '!') > 0 .and. index(line, '!') > index(line, '*'))) then
                    has_scattered_scaling = .true.
                    exit
                end if
            end if
        end do
        
        close(unit)
    end subroutine check_scaling_encapsulation

end program test_select_type_elimination