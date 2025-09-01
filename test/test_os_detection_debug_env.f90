program test_os_detection_debug_env
    !! Verify is_debug_enabled reads FORTPLOT_DEBUG (cross-platform)

    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char
    use fortplot_os_detection, only: is_debug_enabled

    implicit none

    interface
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

    ! Use portable C putenv with a persistent C buffer to avoid lifetime issues
    subroutine set_env(k, v)
        character(len=*), intent(in) :: k, v
        integer(c_int) :: rc
        character(len=:), allocatable :: kv
        call make_c_string(trim(k)//'='//trim(v), rc)
        call ensure(rc == 0, 'putenv succeeded for '//trim(k))
    end subroutine set_env

    subroutine unset_env(k)
        character(len=*), intent(in) :: k
        integer(c_int) :: rc
        call make_c_string(trim(k)//'=', rc)
        call ensure(rc == 0, 'putenv unset succeeded for '//trim(k))
    end subroutine unset_env

    subroutine make_c_string(txt, rc)
        character(len=*), intent(in) :: txt
        integer(c_int), intent(out) :: rc
        character(kind=c_char), save :: buf(256)
        integer :: i, n
        n = len_trim(txt)
        if (n > size(buf)-1) n = size(buf)-1
        do i = 1, n
            buf(i) = transfer(txt(i:i), buf(i))
        end do
        buf(n+1) = c_null_char
        rc = c_putenv(buf)
    end subroutine make_c_string

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
