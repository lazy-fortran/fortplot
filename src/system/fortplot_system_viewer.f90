module fortplot_system_viewer
    !! System viewer launching for show() functionality
    !! Handles platform-specific viewer launching and graphical session detection
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: iso_c_binding, only: c_int
    use fortplot_logging, only: log_info, log_error
    implicit none
    private

    public :: launch_system_viewer
    public :: has_graphical_session
    public :: get_temp_filename
    public :: cleanup_temp_file

    interface
        function c_getpid() bind(C, name="getpid") result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_getpid
    end interface

contains

    function has_graphical_session() result(has_display)
        !! Check if a graphical session is available
        !! Returns .true. if X11, Wayland, macOS GUI, or Windows GUI available
        logical :: has_display
        character(len=256) :: display_var, wayland_var, session_type, ssh_var
        character(len=32) :: os_type

        has_display = .false.
        os_type = get_os_type()

        if (trim(os_type) == 'windows') then
            has_display = .true.
            return
        end if

        if (trim(os_type) == 'darwin') then
            call get_environment_variable('SSH_CONNECTION', ssh_var)
            has_display = len_trim(ssh_var) == 0
            return
        end if

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

        call log_info("Launching viewer: "//trim(command))
        call execute_command_line(trim(command), exitstat=stat)

        if (stat == 0) then
            success = .true.
        else
            call log_error("Failed to launch viewer (exit status "// &
                           trim(int_to_str(stat))//")")
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
            command = 'xdg-open "'//trim(filename)//'" >/dev/null 2>&1 &'
        case ('darwin')
            command = 'open "'//trim(filename)//'"'
        case ('windows')
            command = 'start "" "'//trim(filename)//'"'
        case default
            command = ''
        end select
    end function get_viewer_command

    function get_os_type() result(os_type)
        !! Detect operating system type
        character(len=32) :: os_type
        character(len=256) :: ostype_var, uname_s
        integer :: stat, uname_unit, ios

        call get_environment_variable('OS', ostype_var)
        if (index(ostype_var, 'Windows') > 0) then
            os_type = 'windows'
            return
        end if

        call execute_command_line('uname -s > /tmp/fortplot_uname.txt', exitstat=stat)
        if (stat == 0) then
            open (newunit=uname_unit, file='/tmp/fortplot_uname.txt', status='old', &
                  iostat=ios)
            if (ios == 0) then
                read (uname_unit, '(A)', iostat=ios) uname_s
                close (uname_unit, status='delete')
                if (index(uname_s, 'Darwin') > 0) then
                    os_type = 'darwin'
                    return
                else if (index(uname_s, 'Linux') > 0) then
                    os_type = 'linux'
                    return
                end if
            end if
        end if

        call get_environment_variable('OSTYPE', ostype_var)
        if (index(ostype_var, 'darwin') > 0 .or. index(ostype_var, 'Darwin') > 0) then
            os_type = 'darwin'
        else if (index(ostype_var, 'linux') > 0 .or. index(ostype_var, &
                                                           'Linux') > 0) then
            os_type = 'linux'
        else if (index(ostype_var, 'win') > 0 .or. index(ostype_var, 'msys') > 0) then
            os_type = 'windows'
        else
            os_type = 'linux'
        end if
    end function get_os_type

    function get_temp_dir() result(tempdir)
        !! Get platform-specific temporary directory
        character(len=256) :: tempdir
        character(len=256) :: temp_env
        character(len=32) :: os_type

        os_type = get_os_type()

        if (trim(os_type) == 'windows') then
            call get_environment_variable('TEMP', temp_env)
            if (len_trim(temp_env) > 0) then
                tempdir = trim(temp_env)
                return
            end if
            call get_environment_variable('TMP', temp_env)
            if (len_trim(temp_env) > 0) then
                tempdir = trim(temp_env)
                return
            end if
            tempdir = 'C:\Temp'
        else
            tempdir = '/tmp'
        end if
    end function get_temp_dir

    subroutine get_temp_filename(extension, filename)
        !! Generate unique temporary filename with proper platform handling
        character(len=*), intent(in) :: extension
        character(len=*), intent(out) :: filename
        character(len=256) :: tempdir
        character(len=1) :: sep
        character(len=32) :: os_type
        integer(c_int) :: pid
        integer :: clk_count, clk_rate, clk_max

        tempdir = get_temp_dir()
        os_type = get_os_type()
        pid = c_getpid()
        call system_clock(clk_count, clk_rate, clk_max)

        if (trim(os_type) == 'windows') then
            sep = '\'
        else
            sep = '/'
        end if

        write (filename, '(A,A,A,I0,A,I0,A)') trim(tempdir), sep, 'fortplot_show_', &
            int(pid), '_', clk_count, trim(extension)
    end subroutine get_temp_filename

    subroutine cleanup_temp_file(filename)
        !! Remove temporary file (best effort, no error on failure)
        character(len=*), intent(in) :: filename
        integer :: stat
        logical :: exists

        inquire (file=trim(filename), exist=exists)
        if (exists) then
            open (newunit=stat, file=trim(filename), status='old', iostat=stat)
            if (stat == 0) then
                close (stat, status='delete', iostat=stat)
            end if
        end if
    end subroutine cleanup_temp_file

    function int_to_str(i) result(str)
        !! Convert integer to string
        integer, intent(in) :: i
        character(len=32) :: str
        write (str, '(I0)') i
    end function int_to_str

end module fortplot_system_viewer
