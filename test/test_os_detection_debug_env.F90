program test_os_detection_debug_env
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fortplot_os_detection, only: is_debug_enabled
    implicit none

    interface
#if defined(_WIN32) || defined(__MINGW32__)
        function c_putenv_s(name, value) bind(C, name="_putenv_s") result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            character(kind=c_char), intent(in) :: value(*)
            integer(c_int) :: rc
        end function c_putenv_s
#else
        function c_setenv(name, value, overwrite) bind(C, name="setenv") result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            character(kind=c_char), intent(in) :: value(*)
            integer(c_int), value, intent(in) :: overwrite
            integer(c_int) :: rc
        end function c_setenv

        function c_unsetenv(name) bind(C, name="unsetenv") result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int) :: rc
        end function c_unsetenv
#endif
    end interface

    call ensure(unset_env('FORTPLOT_DEBUG') == 0, 'unset FORTPLOT_DEBUG')
    call ensure(.not. is_debug_enabled(), 'debug disabled when FORTPLOT_DEBUG unset')

    call ensure(set_env('FORTPLOT_DEBUG', '1') == 0, 'set FORTPLOT_DEBUG=1')
    call ensure(is_debug_enabled(), 'debug enabled when FORTPLOT_DEBUG=1')

    call ensure(set_env('FORTPLOT_DEBUG', 'true') == 0, 'set FORTPLOT_DEBUG=true')
    call ensure(is_debug_enabled(), 'debug enabled when FORTPLOT_DEBUG=true')

    call ensure(set_env('FORTPLOT_DEBUG', 'TRUE') == 0, 'set FORTPLOT_DEBUG=TRUE')
    call ensure(is_debug_enabled(), 'debug enabled when FORTPLOT_DEBUG=TRUE (case-insensitive)')

    call ensure(set_env('FORTPLOT_DEBUG', 'False') == 0, 'set FORTPLOT_DEBUG=False')
    call ensure(.not. is_debug_enabled(), 'debug disabled when FORTPLOT_DEBUG=False (case-insensitive)')

    call ensure(set_env('FORTPLOT_DEBUG', '0') == 0, 'set FORTPLOT_DEBUG=0')
    call ensure(.not. is_debug_enabled(), 'debug disabled when FORTPLOT_DEBUG=0')

contains

    integer function set_env(name, value) result(rc)
        character(len=*), intent(in) :: name, value

#if defined(_WIN32) || defined(__MINGW32__)
        rc = c_putenv_s(c_string(name), c_string(value))
#else
        rc = c_setenv(c_string(name), c_string(value), 1_c_int)
#endif
    end function set_env

    integer function unset_env(name) result(rc)
        character(len=*), intent(in) :: name

#if defined(_WIN32) || defined(__MINGW32__)
        rc = c_putenv_s(c_string(name), c_string(''))
#else
        rc = c_unsetenv(c_string(name))
#endif
    end function unset_env

    function c_string(text) result(buffer)
        character(len=*), intent(in) :: text
        character(kind=c_char), allocatable :: buffer(:)
        integer :: i

        allocate(buffer(len_trim(text) + 1))
        do i = 1, len_trim(text)
            buffer(i) = transfer(text(i:i), buffer(i))
        end do
        buffer(size(buffer)) = c_null_char
    end function c_string

    subroutine ensure(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (.not. cond) then
            print *, 'FAIL: ', trim(msg)
            stop 1
        end if

        print *, 'PASS: ', trim(msg)
    end subroutine ensure

end program test_os_detection_debug_env
