module fortplot_png_validation
    !! PNG file validation using external pngcheck tool
    !! CRITICAL: Every PNG file MUST be validated after creation
    implicit none
    
    private
    public :: validate_png_file, png_validation_available
    
contains

    function png_validation_available() result(available)
        !! Check if pngcheck tool is available on system
        logical :: available
        integer :: exit_code
        
        ! Test if pngcheck is available
        call execute_command_line("which pngcheck > /dev/null 2>&1", exitstat=exit_code)
        available = (exit_code == 0)
    end function png_validation_available

    subroutine validate_png_file(filename, test_name)
        !! Validate PNG file using pngcheck and fail test if invalid
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: test_name
        
        character(len=512) :: command
        integer :: exit_code
        logical :: file_exists
        
        ! Check if file exists first
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) then
            print *, "FATAL: PNG file does not exist: ", trim(filename)
            print *, "TEST FAILED: ", trim(test_name)
            error stop "PNG file missing"
        end if
        
        ! Skip validation if pngcheck not available (CI environments)
        if (.not. png_validation_available()) then
            print *, "WARNING: pngcheck not available, skipping PNG validation"
            return
        end if
        
        ! Run pngcheck on the PNG file
        write(command, '(A,A,A)') 'pngcheck "', trim(filename), '" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code)
        
        if (exit_code /= 0) then
            print *, "FATAL: PNG file failed validation: ", trim(filename)
            print *, "TEST FAILED: ", trim(test_name)
            
            ! Show the actual pngcheck error
            write(command, '(A,A,A)') 'echo "PNG validation error:"; pngcheck "', trim(filename), '"'
            call execute_command_line(command)
            
            error stop "PNG validation failed - file is corrupted"
        else
            print *, "✓ PNG validation passed: ", trim(filename)
        end if
    end subroutine validate_png_file

end module fortplot_png_validation