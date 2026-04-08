module fortplot_os_detection
    !! Operating system detection and lightweight environment helpers.
    implicit none
    private

    public :: is_debug_enabled
    public :: is_windows

contains

    function is_debug_enabled() result(debug_enabled)
        logical :: debug_enabled
        character(len=32) :: value
        integer :: status

        call get_environment_variable("FORTPLOT_DEBUG", value, status=status)
        if (status /= 0) then
            debug_enabled = .false.
            return
        end if

        call lowercase_in_place(value)
        select case (trim(value))
        case ("1", "true", "yes", "on", "t", "y")
            debug_enabled = .true.
        case default
            debug_enabled = .false.
        end select
    end function is_debug_enabled

    function is_windows() result(windows)
        logical :: windows
        character(len=256) :: value
        integer :: status

        call get_environment_variable("OS", value, status=status)
        if (status == 0 .and. index(value, "Windows") > 0) then
            windows = .true.
            return
        end if

        call get_environment_variable("ComSpec", value, status=status)
        if (status == 0 .and. len_trim(value) > 0) then
            windows = .true.
            return
        end if

        call get_environment_variable("WINDIR", value, status=status)
        windows = (status == 0 .and. len_trim(value) > 0)
    end function is_windows

    subroutine lowercase_in_place(text)
        character(len=*), intent(inout) :: text
        integer :: i, code

        do i = 1, len_trim(text)
            code = iachar(text(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                text(i:i) = achar(code + iachar('a') - iachar('A'))
            end if
        end do
    end subroutine lowercase_in_place

end module fortplot_os_detection
