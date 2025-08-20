program test_trampoline_elimination
    !! Test suite for trampoline elimination (Issue #149)
    !!
    !! GIVEN: The fortplot library contains nested functions that generate trampolines
    !! WHEN: Compiling with -Wtrampolines and -Werror=trampolines flags
    !! THEN: No trampolines should be generated (compilation should succeed)
    !! AND: Nested functions should be refactored to module-level procedures
    !! AND: Executable stack protection should be enabled
    
    implicit none
    
    call test_no_nested_functions_in_streamline()
    call test_security_flags_compilation()
    call test_executable_stack_protection()
    
    print *, 'All trampoline elimination tests PASSED!'
    
contains

    subroutine test_no_nested_functions_in_streamline()
        !! GIVEN: fortplot_streamline.f90 previously contained nested functions
        !! WHEN: Checking for nested function definitions  
        !! THEN: No nested functions should be found (all moved to module level)
        
        logical :: has_nested_functions
        
        call check_file_for_nested_functions('src/fortplot_streamline.f90', has_nested_functions)
        if (has_nested_functions) then
            error stop 'FAILED: fortplot_streamline.f90 still contains nested functions'
        end if
        
        print *, 'PASSED: fortplot_streamline.f90 has no nested functions'
    end subroutine test_no_nested_functions_in_streamline

    subroutine check_file_for_nested_functions(file_path, has_nested)
        !! Helper routine to detect nested function definitions
        character(*), intent(in) :: file_path
        logical, intent(out) :: has_nested
        
        integer :: unit, iostat
        character(len=1000) :: line, prev_line
        logical :: in_contains_block, in_procedure
        integer :: procedure_depth
        
        has_nested = .false.
        in_contains_block = .false.
        in_procedure = .false.
        procedure_depth = 0
        prev_line = ''
        
        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            error stop 'Cannot open file: ' // file_path
        end if
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            line = trim(adjustl(line))
            if (len(line) == 0) then
                prev_line = line
                cycle
            end if
            
            ! Skip comments
            if (line(1:1) == '!') then
                prev_line = line
                cycle
            end if
            
            line = to_lowercase(line)
            
            ! Track procedure starts (subroutines and functions)
            if ((index(line, 'subroutine ') > 0 .and. index(line, 'end subroutine') == 0) .or. &
                (index(line, 'function ') > 0 .and. index(line, 'end function') == 0)) then
                procedure_depth = procedure_depth + 1
                in_procedure = .true.
            end if
            
            ! Track contains blocks within procedures
            if (in_procedure .and. index(line, 'contains') > 0) then
                in_contains_block = .true.
            end if
            
            ! Check for nested function/subroutine definitions (trampolines!)
            if (in_contains_block .and. procedure_depth >= 1) then
                ! Look for function/subroutine definitions after 'contains'
                if ((index(line, 'function ') > 0 .and. index(line, 'end function') == 0) .or. &
                    (index(line, 'subroutine ') > 0 .and. index(line, 'end subroutine') == 0)) then
                    ! This is a nested function/subroutine - potential trampoline!
                    has_nested = .true.
                    exit
                end if
            end if
            
            ! Track procedure ends
            if (index(line, 'end ') > 0) then
                if ((index(line, 'end function') > 0 .or. index(line, 'end subroutine') > 0) .and. &
                    index(line, 'end module') == 0) then
                    procedure_depth = max(0, procedure_depth - 1)
                    if (procedure_depth == 0) then
                        in_procedure = .false.
                        in_contains_block = .false.
                    end if
                end if
            end if
            
            prev_line = line
        end do
        
        close(unit)
    end subroutine check_file_for_nested_functions

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

    subroutine test_security_flags_compilation()
        !! GIVEN: Security compiler flags are configured
        !! WHEN: Building the library with trampoline detection
        !! THEN: No trampoline warnings should be generated
        
        ! This test verifies that the build system includes the security flags
        ! The actual compilation test is handled by the build system
        
        logical :: flags_configured
        call check_security_flags_configured(flags_configured)
        
        if (.not. flags_configured) then
            error stop 'FAILED: Security compiler flags not properly configured'
        end if
        
        print *, 'PASSED: Security compiler flags are properly configured'
    end subroutine test_security_flags_compilation

    subroutine check_security_flags_configured(flags_configured)
        !! Check if security flags are configured in build system
        logical, intent(out) :: flags_configured
        
        logical :: fpm_configured, cmake_configured
        
        call check_fpm_security_flags(fpm_configured)
        call check_cmake_security_flags(cmake_configured)
        
        ! At least one build system should have security flags
        flags_configured = fpm_configured .or. cmake_configured
    end subroutine check_security_flags_configured

    subroutine check_fpm_security_flags(fpm_configured)
        !! Check if fpm.toml has security flags configured
        logical, intent(out) :: fpm_configured
        
        integer :: unit, iostat
        character(len=1000) :: line
        logical :: found_trampolines, found_werror
        
        fpm_configured = .false.
        found_trampolines = .false.
        found_werror = .false.
        
        open(newunit=unit, file='fpm.toml', status='old', action='read', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            line = to_lowercase(trim(line))
            
            if (index(line, 'trampolines') > 0) then
                found_trampolines = .true.
            end if
            
            if (index(line, 'werror') > 0 .and. index(line, 'trampolines') > 0) then
                found_werror = .true.
            end if
        end do
        
        close(unit)
        fpm_configured = found_trampolines .and. found_werror
    end subroutine check_fpm_security_flags

    subroutine check_cmake_security_flags(cmake_configured)
        !! Check if CMakeLists.txt has security flags configured
        logical, intent(out) :: cmake_configured
        
        integer :: unit, iostat
        character(len=1000) :: line
        logical :: found_trampolines, found_werror, found_noexecstack
        
        cmake_configured = .false.
        found_trampolines = .false.
        found_werror = .false.
        found_noexecstack = .false.
        
        open(newunit=unit, file='CMakeLists.txt', status='old', action='read', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            line = to_lowercase(trim(line))
            
            if (index(line, 'trampolines') > 0) then
                found_trampolines = .true.
            end if
            
            if (index(line, 'werror') > 0 .and. index(line, 'trampolines') > 0) then
                found_werror = .true.
            end if
            
            if (index(line, 'noexecstack') > 0) then
                found_noexecstack = .true.
            end if
        end do
        
        close(unit)
        cmake_configured = found_trampolines .and. found_werror .and. found_noexecstack
    end subroutine check_cmake_security_flags

    subroutine test_executable_stack_protection()
        !! GIVEN: Linker flags for executable stack protection
        !! WHEN: Building the library
        !! THEN: Executable stack should be disabled (-Wl,-z,noexecstack)
        
        logical :: protection_enabled
        call check_executable_stack_protection(protection_enabled)
        
        if (.not. protection_enabled) then
            error stop 'FAILED: Executable stack protection not enabled'
        end if
        
        print *, 'PASSED: Executable stack protection is enabled'
    end subroutine test_executable_stack_protection

    subroutine check_executable_stack_protection(protection_enabled)
        !! Check if executable stack protection is configured
        logical, intent(out) :: protection_enabled
        
        integer :: unit, iostat
        character(len=1000) :: line
        
        protection_enabled = .false.
        
        open(newunit=unit, file='CMakeLists.txt', status='old', action='read', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            line = to_lowercase(trim(line))
            
            if (index(line, 'noexecstack') > 0) then
                protection_enabled = .true.
                exit
            end if
        end do
        
        close(unit)
    end subroutine check_executable_stack_protection

end program test_trampoline_elimination