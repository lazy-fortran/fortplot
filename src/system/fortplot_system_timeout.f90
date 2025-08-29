module fortplot_system_timeout
    !! Timeout-safe system operations for Windows CI reliability
    !! SECURITY: All timeout command execution disabled for security compliance
    use iso_fortran_env, only: int32
    use fortplot_system_runtime, only: is_windows
    use iso_c_binding
    implicit none
    private

    public :: execute_command_line_timeout
    public :: system_command_timeout
    public :: get_windows_timeout_ms
    public :: sleep_ms

    ! Windows CI timeout settings
    integer, parameter :: WINDOWS_CI_TIMEOUT_MS = 5000  ! 5 seconds max for any command
    integer, parameter :: UNIX_DEFAULT_TIMEOUT_MS = 10000  ! 10 seconds for Unix
    
    ! SECURITY NOTE: C interface bindings removed for security compliance

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
        !! SECURITY: Command execution with timeout disabled for security compliance
        character(len=*), intent(in) :: command
        logical, intent(out) :: success
        integer, intent(in), optional :: timeout_ms
        character(len=*), intent(in), optional :: debug_info
        
        character(len=256) :: debug_context
        integer :: effective_timeout
        
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
        
        ! SECURITY: External command execution disabled for security compliance
        write(*,'(A,A,A)') 'SECURITY: Command execution disabled: [', trim(debug_context), ']'
        success = .false.
    end subroutine execute_command_line_timeout

    subroutine system_command_timeout(command, success, timeout_ms)
        !! SECURITY: System command wrapper disabled for security compliance
        character(len=*), intent(in) :: command
        logical, intent(out) :: success
        integer, intent(in), optional :: timeout_ms
        
        call execute_command_line_timeout(command, success, timeout_ms, "system_cmd")
    end subroutine system_command_timeout
    
    subroutine sleep_ms(milliseconds)
        !! Sleep for specified milliseconds using Fortran intrinsic
        integer, intent(in) :: milliseconds
        real :: seconds
        integer :: start_count, end_count, count_rate, target_count
        
        ! Convert milliseconds to seconds for system_clock
        seconds = real(milliseconds) / 1000.0
        
        ! Use system_clock for precise timing
        call system_clock(start_count, count_rate)
        target_count = int(seconds * real(count_rate))
        
        do
            call system_clock(end_count)
            if (end_count - start_count >= target_count) exit
        end do
    end subroutine sleep_ms

    function int_to_str(value) result(str)
        !! Convert integer to string
        integer, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=16) :: buffer
        
        write(buffer, '(I0)') value
        str = trim(adjustl(buffer))
    end function int_to_str

end module fortplot_system_timeout