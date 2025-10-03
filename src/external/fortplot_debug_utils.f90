module fortplot_debug_utils
    !! Debug utilities for optional instrumentation
    implicit none

    private
    public :: is_debug_enabled, log_debug_message

    logical, save :: debug_initialized = .false.
    logical, save :: debug_enabled_state = .false.

contains

    pure function to_lower_char(ch) result(lower)
        !! Lower-case conversion for ASCII characters
        character(len=1), intent(in) :: ch
        character(len=1) :: lower
        integer :: code

        lower = ch
        code = iachar(ch)
        if (code >= iachar('A') .and. code <= iachar('Z')) then
            lower = achar(code + 32)
        end if
    end function to_lower_char

    logical function parse_bool_env(value)
        !! Interpret an environment variable as boolean
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: trimmed
        integer :: i

        trimmed = trim(adjustl(value))

        if (len(trimmed) == 0) then
            parse_bool_env = .true.
            return
        end if

        do i = 1, len(trimmed)
            trimmed(i:i) = to_lower_char(trimmed(i:i))
        end do

        select case (trimmed)
        case ('0', 'false', 'off', 'no')
            parse_bool_env = .false.
        case default
            parse_bool_env = .true.
        end select
    end function parse_bool_env

    subroutine ensure_debug_init(env_var)
        !! Lazy initialization for debug flag from environment variable
        character(len=*), intent(in) :: env_var
        character(len=32) :: env_value
        integer :: status

        if (debug_initialized) return

        call get_environment_variable(env_var, env_value, status=status)
        if (status == 0) then
            debug_enabled_state = parse_bool_env(env_value)
        else
            debug_enabled_state = .false.
        end if
        debug_initialized = .true.
    end subroutine ensure_debug_init

    logical function is_debug_enabled(env_var)
        !! Query if debug is enabled for given environment variable
        character(len=*), intent(in) :: env_var
        call ensure_debug_init(env_var)
        is_debug_enabled = debug_enabled_state
    end function is_debug_enabled

    subroutine log_debug_message(prefix, message)
        !! Emit debug message with prefix
        character(len=*), intent(in) :: prefix, message
        print '(a)', '['//trim(prefix)//'] '//trim(message)
    end subroutine log_debug_message

end module fortplot_debug_utils
