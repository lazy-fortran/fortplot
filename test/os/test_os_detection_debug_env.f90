program test_os_detection_debug_env
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fortplot_os_detection, only: is_debug_enabled
    implicit none

    type :: env_buffer_t
        character(kind=c_char), allocatable :: value(:)
    end type env_buffer_t

    interface
        function c_putenv(name_value) bind(C, name="putenv") result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name_value(*)
            integer(c_int) :: rc
        end function c_putenv
    end interface

    call ensure(set_env('FORTPLOT_DEBUG', '') == 0, 'clear FORTPLOT_DEBUG')
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
        type(env_buffer_t), save :: saved_env(16)
        integer, save :: saved_count = 0
        integer :: slot

        slot = min(saved_count + 1, size(saved_env))
        saved_count = slot

        if (allocated(saved_env(slot)%value)) deallocate(saved_env(slot)%value)
        saved_env(slot)%value = c_string(trim(name) // '=' // trim(value))
        rc = c_putenv(saved_env(slot)%value)
    end function set_env

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
