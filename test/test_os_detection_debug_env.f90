program test_os_detection_debug_env
    !! Verify is_debug_enabled reads FORTPLOT_DEBUG (cross-platform)

    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char
    use fortplot_os_detection, only: is_debug_enabled

    implicit none

    ! Provide both POSIX and Windows interfaces; dispatch at runtime.
    interface
        function c_setenv(name, value, overwrite) bind(C, name="setenv") result(ret)
            import :: c_int, c_char
            character(kind=c_char), dimension(*) :: name
            character(kind=c_char), dimension(*) :: value
            integer(c_int), value :: overwrite
            integer(c_int) :: ret
        end function c_setenv
        function c_unsetenv(name) bind(C, name="unsetenv") result(ret)
            import :: c_int, c_char
            character(kind=c_char), dimension(*) :: name
            integer(c_int) :: ret
        end function c_unsetenv
        function c_putenv(s) bind(C, name="putenv") result(ret)
            import :: c_int, c_char
            character(kind=c_char), dimension(*) :: s
            integer(c_int) :: ret
        end function c_putenv
    end interface

    call ensure(.true., 'sanity')

    ! Ensure clean state
    call unset_env('FORTPLOT_DEBUG')

    call ensure(.not. is_debug_enabled(), 'debug disabled when FORTPLOT_DEBUG unset')

    call set_env('FORTPLOT_DEBUG', '1')
    call ensure(is_debug_enabled(), 'debug enabled when FORTPLOT_DEBUG=1')

    call set_env('FORTPLOT_DEBUG', 'true')
    call ensure(is_debug_enabled(), 'debug enabled when FORTPLOT_DEBUG=true')

    call set_env('FORTPLOT_DEBUG', '0')
    call ensure(.not. is_debug_enabled(), 'debug disabled when FORTPLOT_DEBUG=0')

contains

    subroutine set_env(k, v)
        use fortplot_os_detection, only: is_windows
        character(len=*), intent(in) :: k, v
        integer(c_int) :: rc
        character(len=:), allocatable :: kv
        if (is_windows()) then
            kv = trim(k)//'='//trim(v)
            rc = c_putenv(kv//c_null_char)
            call ensure(rc == 0, 'putenv succeeded for '//trim(k))
        else
            rc = c_setenv(trim(k)//c_null_char, trim(v)//c_null_char, 1_c_int)
            call ensure(rc == 0, 'setenv succeeded for '//trim(k))
        end if
    end subroutine set_env

    subroutine unset_env(k)
        use fortplot_os_detection, only: is_windows
        character(len=*), intent(in) :: k
        integer(c_int) :: rc
        character(len=:), allocatable :: kv
        if (is_windows()) then
            kv = trim(k)//'='
            rc = c_putenv(kv//c_null_char)
            call ensure(rc == 0, 'putenv unset succeeded for '//trim(k))
        else
            rc = c_unsetenv(trim(k)//c_null_char)
            call ensure(rc == 0, 'unsetenv succeeded for '//trim(k))
        end if
    end subroutine unset_env

    subroutine ensure(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        if (.not. cond) then
            print *, 'FAIL: ', trim(msg)
            stop 1
        else
            print *, 'PASS: ', trim(msg)
        end if
    end subroutine ensure

end program test_os_detection_debug_env
