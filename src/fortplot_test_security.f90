!! Security hardening module for test operations
!! Provides secure alternatives to execute_command_line in tests
module fortplot_test_security
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use fortplot_security, only: safe_remove_file, safe_check_program_available, &
                                  safe_validate_mpeg_with_ffprobe, is_safe_path
    implicit none
    private

    public :: secure_test_cleanup_file
    public :: secure_test_check_ffprobe
    public :: secure_test_create_dummy_file
    public :: secure_test_validate_mpeg
    public :: secure_test_system_command

contains

    !> Securely remove test files without shell injection
    subroutine secure_test_cleanup_file(filename)
        character(len=*), intent(in) :: filename
        logical :: success
        
        call safe_remove_file(filename, success)
        if (.not. success) then
            write(error_unit, '(A)') 'Warning: Could not remove test file: ' // trim(filename)
        end if
    end subroutine secure_test_cleanup_file

    !> Securely check if ffprobe is available
    function secure_test_check_ffprobe() result(available)
        logical :: available
        
        ! Use secure checking - assume not available for safety
        available = safe_check_program_available('ffprobe')
        if (.not. available) then
            write(error_unit, '(A)') 'Info: ffprobe not available in secure mode - tests will be limited'
        end if
    end function secure_test_check_ffprobe

    !> Create dummy file for testing
    subroutine secure_test_create_dummy_file(filename, content)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: content
        
        integer :: unit, iostat
        character(len=:), allocatable :: file_content
        
        ! Validate filename safety
        if (.not. is_safe_path(filename)) then
            write(error_unit, '(A)') 'Error: Unsafe filename rejected: ' // trim(filename)
            return
        end if
        
        if (present(content)) then
            file_content = content
        else
            file_content = 'dummy test content'
        end if
        
        open(newunit=unit, file=trim(filename), iostat=iostat, status='replace')
        if (iostat == 0) then
            write(unit, '(A)', iostat=iostat) file_content
            close(unit)
            if (iostat /= 0) then
                write(error_unit, '(A)') 'Warning: Could not write test file: ' // trim(filename)
            end if
        else
            write(error_unit, '(A)') 'Warning: Could not create test file: ' // trim(filename)
        end if
    end subroutine secure_test_create_dummy_file

    !> Securely validate MPEG files in tests
    function secure_test_validate_mpeg(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        valid = safe_validate_mpeg_with_ffprobe(filename)
    end function secure_test_validate_mpeg

    !> Secure replacement for system commands in tests
    subroutine secure_test_system_command(command, description, exit_status)
        character(len=*), intent(in) :: command
        character(len=*), intent(in) :: description
        integer, intent(out), optional :: exit_status
        
        ! Log that command was blocked for security
        write(error_unit, '(A)') 'Security: System command blocked in secure mode'
        write(error_unit, '(A)') 'Command: ' // trim(command)
        write(error_unit, '(A)') 'Description: ' // trim(description)
        write(error_unit, '(A)') 'Note: Tests adapted for secure execution'
        
        ! Set exit status to indicate command was not executed
        if (present(exit_status)) then
            exit_status = -1  ! Indicates security block
        end if
    end subroutine secure_test_system_command

end module fortplot_test_security