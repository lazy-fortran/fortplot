module fortplot_system_timeout
    !! Timeout-safe system operations for Windows CI reliability
    use iso_fortran_env, only: int32
    use fortplot_system_runtime, only: is_windows
    implicit none
    private

    public :: execute_command_line_timeout
    public :: system_command_timeout
    public :: get_windows_timeout_ms

    ! Windows CI timeout settings
    integer, parameter :: WINDOWS_CI_TIMEOUT_MS = 5000  ! 5 seconds max for any command
    integer, parameter :: UNIX_DEFAULT_TIMEOUT_MS = 10000  ! 10 seconds for Unix

contains

    function get_windows_timeout_ms() result(timeout_ms)
        !! Get appropriate timeout for Windows CI operations
        integer :: timeout_ms
        character(len=256) :: ci_env
        integer :: status
        
        ! Default timeout
        if (is_windows()) then
            timeout_ms = WINDOWS_CI_TIMEOUT_MS
        else
            timeout_ms = UNIX_DEFAULT_TIMEOUT_MS
        end if
        
        ! Check if in CI - use shorter timeout
        call get_environment_variable("CI", ci_env, status=status)
        if (status == 0 .and. len_trim(ci_env) > 0) then
            timeout_ms = WINDOWS_CI_TIMEOUT_MS  ! Force short timeout in CI
            return
        end if
        
        call get_environment_variable("GITHUB_ACTIONS", ci_env, status=status)
        if (status == 0 .and. len_trim(ci_env) > 0) then
            timeout_ms = WINDOWS_CI_TIMEOUT_MS  ! Force short timeout in GitHub Actions
            return
        end if
    end function get_windows_timeout_ms

    subroutine execute_command_line_timeout(command, success, timeout_ms, debug_info)
        !! Execute command line with timeout for hang prevention
        character(len=*), intent(in) :: command
        logical, intent(out) :: success
        integer, intent(in), optional :: timeout_ms
        character(len=*), intent(in), optional :: debug_info
        
        character(len=:), allocatable :: safe_command
        character(len=256) :: debug_context
        integer :: exitstat, cmdstat, effective_timeout
        character(len=256) :: cmdmsg
        
        success = .false.
        
        ! Determine timeout
        if (present(timeout_ms)) then
            effective_timeout = timeout_ms
        else
            effective_timeout = get_windows_timeout_ms()
        end if
        
        ! Debug context
        if (present(debug_info)) then
            debug_context = debug_info
        else
            debug_context = "system_command"
        end if
        
        ! Build timeout-wrapped command
        if (is_windows()) then
            ! Windows: use timeout command if available, otherwise execute directly
            safe_command = 'timeout /t ' // trim(adjustl(int_to_str(effective_timeout/1000 + 1))) // &
                          ' ' // trim(command) // ' 2>nul'
        else
            ! Unix: use timeout command
            safe_command = 'timeout ' // trim(adjustl(int_to_str(effective_timeout/1000 + 1))) // 's ' // &
                          trim(command) // ' 2>/dev/null'
        end if
        
        ! Log the attempt for debugging
        if (is_windows()) then
            write(*,'(A,A,A,I0,A)') 'DEBUG: [', trim(debug_context), '] Executing with timeout ', &
                                   effective_timeout, 'ms: ', trim(command)
        end if
        
        ! Execute with built-in timeout
        call execute_command_line(safe_command, exitstat=exitstat, cmdstat=cmdstat, cmdmsg=cmdmsg)
        
        ! Check results
        if (cmdstat /= 0) then
            write(*,'(A,A,A,I0,A,A)') 'DEBUG: [', trim(debug_context), '] Command failed (cmdstat=', &
                                     cmdstat, '): ', trim(cmdmsg)
            success = .false.
        else if (exitstat == 124 .or. exitstat == 1) then
            ! Timeout occurred (124 on Unix, 1 on Windows timeout command)
            write(*,'(A,A,A,I0,A)') 'DEBUG: [', trim(debug_context), '] Command timed out after ', &
                                   effective_timeout, 'ms'
            success = .false.
        else if (exitstat == 0) then
            success = .true.
        else
            write(*,'(A,A,A,I0)') 'DEBUG: [', trim(debug_context), '] Command exit status: ', exitstat
            success = .false.
        end if
    end subroutine execute_command_line_timeout

    subroutine system_command_timeout(command, success, timeout_ms)
        !! System command wrapper with timeout protection
        character(len=*), intent(in) :: command
        logical, intent(out) :: success
        integer, intent(in), optional :: timeout_ms
        
        call execute_command_line_timeout(command, success, timeout_ms, "system_cmd")
    end subroutine system_command_timeout

    function int_to_str(value) result(str)
        !! Convert integer to string
        integer, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=16) :: buffer
        
        write(buffer, '(I0)') value
        str = trim(adjustl(buffer))
    end function int_to_str

end module fortplot_system_timeout