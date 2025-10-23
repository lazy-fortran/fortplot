module fortplot_system_viewer
    !! System viewer launching for show() functionality
    !! Handles platform-specific viewer launching and graphical session detection
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_logging, only: log_info, log_error
    implicit none
    private

    public :: launch_system_viewer
    public :: has_graphical_session

contains

    function has_graphical_session() result(has_display)
        !! Check if a graphical session is available
        !! Returns .true. if X11, Wayland, or other display is available
        logical :: has_display
        character(len=256) :: display_var, wayland_var, session_type

        has_display = .false.

        call get_environment_variable('DISPLAY', display_var)
        if (len_trim(display_var) > 0) then
            has_display = .true.
            return
        end if

        call get_environment_variable('WAYLAND_DISPLAY', wayland_var)
        if (len_trim(wayland_var) > 0) then
            has_display = .true.
            return
        end if

        call get_environment_variable('XDG_SESSION_TYPE', session_type)
        if (trim(session_type) == 'wayland' .or. trim(session_type) == 'x11') then
            has_display = .true.
            return
        end if
    end function has_graphical_session

    subroutine launch_system_viewer(filename, success)
        !! Launch system viewer for file
        !! Uses xdg-open on Linux, open on macOS, start on Windows
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        character(len=1024) :: command
        integer :: stat

        success = .false.

        command = get_viewer_command(filename)
        if (len_trim(command) == 0) then
            call log_error("No suitable viewer command found for platform")
            return
        end if

        call log_info("Launching viewer: " // trim(command))
        call execute_command_line(trim(command), exitstat=stat)

        if (stat == 0) then
            success = .true.
        else
            call log_error("Failed to launch viewer (exit status " // &
                          trim(int_to_str(stat)) // ")")
        end if
    end subroutine launch_system_viewer

    function get_viewer_command(filename) result(command)
        !! Get platform-specific viewer command
        character(len=*), intent(in) :: filename
        character(len=1024) :: command
        character(len=32) :: os_type

        command = ''
        os_type = get_os_type()

        select case (trim(os_type))
        case ('linux')
            command = 'xdg-open "' // trim(filename) // '" >/dev/null 2>&1 &'
        case ('darwin')
            command = 'open "' // trim(filename) // '"'
        case ('windows')
            command = 'start "" "' // trim(filename) // '"'
        case default
            command = ''
        end select
    end function get_viewer_command

    function get_os_type() result(os_type)
        !! Detect operating system type
        character(len=32) :: os_type
        character(len=256) :: ostype_var
        integer :: stat

        call get_environment_variable('OSTYPE', ostype_var)
        if (len_trim(ostype_var) > 0) then
            if (index(ostype_var, 'linux') > 0) then
                os_type = 'linux'
                return
            else if (index(ostype_var, 'darwin') > 0) then
                os_type = 'darwin'
                return
            else if (index(ostype_var, 'win') > 0 .or. index(ostype_var, 'msys') > 0) then
                os_type = 'windows'
                return
            end if
        end if

        call execute_command_line('uname', exitstat=stat)
        if (stat == 0) then
            os_type = 'linux'
            return
        end if

        call get_environment_variable('OS', ostype_var)
        if (index(ostype_var, 'Windows') > 0) then
            os_type = 'windows'
            return
        end if

        os_type = 'linux'
    end function get_os_type

    function int_to_str(i) result(str)
        !! Convert integer to string
        integer, intent(in) :: i
        character(len=32) :: str
        write(str, '(I0)') i
    end function int_to_str

end module fortplot_system_viewer
