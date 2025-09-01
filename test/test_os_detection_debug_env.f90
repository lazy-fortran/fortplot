program test_os_detection_debug_env
    !! Verify is_debug_enabled reads FORTPLOT_DEBUG (cross-platform)

    use, intrinsic :: iso_c_binding, only: c_int, c_char
    use fortplot_os_detection, only: is_debug_enabled

    implicit none

    interface
        function fortplot_debug_set_1() bind(C, name="fortplot_debug_set_1") result(rc)
            import :: c_int
            integer(c_int) :: rc
        end function fortplot_debug_set_1
        function fortplot_debug_set_true() bind(C, name="fortplot_debug_set_true") result(rc)
            import :: c_int
            integer(c_int) :: rc
        end function fortplot_debug_set_true
        function fortplot_debug_set_0() bind(C, name="fortplot_debug_set_0") result(rc)
            import :: c_int
            integer(c_int) :: rc
        end function fortplot_debug_set_0
        function fortplot_debug_unset() bind(C, name="fortplot_debug_unset") result(rc)
            import :: c_int
            integer(c_int) :: rc
        end function fortplot_debug_unset
        function c_putenv(s) bind(C, name="putenv") result(ret)
            import :: c_int, c_char
            character(kind=c_char), dimension(*) :: s
            integer(c_int) :: ret
        end function c_putenv
    end interface

    call ensure(.true., 'sanity')

    ! Ensure clean state
    call ensure(fortplot_debug_unset() == 0, 'unset FORTPLOT_DEBUG')

    call ensure(.not. is_debug_enabled(), 'debug disabled when FORTPLOT_DEBUG unset')

    call ensure(fortplot_debug_set_1() == 0, 'set FORTPLOT_DEBUG=1')
    call ensure(is_debug_enabled(), 'debug enabled when FORTPLOT_DEBUG=1')

    call ensure(fortplot_debug_set_true() == 0, 'set FORTPLOT_DEBUG=true')
    call ensure(is_debug_enabled(), 'debug enabled when FORTPLOT_DEBUG=true')

    ! Case-insensitivity checks via direct environment mutation from C helpers
    call set_env('FORTPLOT_DEBUG', 'TRUE')
    call ensure(is_debug_enabled(), 'debug enabled when FORTPLOT_DEBUG=TRUE (case-insensitive)')

    call set_env('FORTPLOT_DEBUG', 'False')
    call ensure(.not. is_debug_enabled(), 'debug disabled when FORTPLOT_DEBUG=False (case-insensitive)')

    call ensure(fortplot_debug_set_0() == 0, 'set FORTPLOT_DEBUG=0')
    call ensure(.not. is_debug_enabled(), 'debug disabled when FORTPLOT_DEBUG=0')

contains

    subroutine set_env(k, v)
        !! Portable putenv-based setter using a persistent C buffer
        character(len=*), intent(in) :: k, v
        integer(c_int) :: rc
        call make_c_string(trim(k)//'='//trim(v), rc)
        call ensure(rc == 0, 'putenv succeeded for '//trim(k))
    end subroutine set_env

    subroutine make_c_string(txt, rc)
        use, intrinsic :: iso_c_binding, only: c_char
        character(len=*), intent(in) :: txt
        integer(c_int), intent(out) :: rc
        character(kind=c_char), save :: buf(256)
        integer :: i, n
        n = len_trim(txt)
        if (n > size(buf)-1) n = size(buf)-1
        do i = 1, n
            buf(i) = transfer(txt(i:i), buf(i))
        end do
        buf(n+1) = c_char_null()
        rc = c_putenv(buf)
    end subroutine make_c_string

    pure function c_char_null() result(z)
        use, intrinsic :: iso_c_binding, only: c_char
        character(kind=c_char) :: z
        z = achar(0)
    end function c_char_null

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
