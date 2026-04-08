module fortplot_system_commands
    use fortplot_os_detection, only: is_windows
    use fortplot_system_viewer, only: launch_system_viewer
    implicit none
    private

    public :: open_with_default_app_runtime
    public :: check_command_available_runtime
    public :: use_system_mkdir_ci
    public :: try_system_mkdir

contains

    subroutine open_with_default_app_runtime(filename, success)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success

        call launch_system_viewer(filename, success)
    end subroutine open_with_default_app_runtime

    subroutine use_system_mkdir_ci(path, success)
        character(len=*), intent(in) :: path
        logical, intent(out) :: success

        call run_mkdir(path, success)
    end subroutine use_system_mkdir_ci

    subroutine try_system_mkdir(path, success)
        character(len=*), intent(in) :: path
        logical, intent(out) :: success

        call run_mkdir(path, success)
    end subroutine try_system_mkdir

    subroutine check_command_available_runtime(command_name, available)
        character(len=*), intent(in) :: command_name
        logical, intent(out) :: available
        character(len=:), allocatable :: command
        integer :: cmdstat, exitstat

        if (len_trim(command_name) == 0) then
            available = .false.
            return
        end if

        if (is_windows()) then
            command = 'where ' // trim(command_name) // ' >NUL 2>NUL'
            call execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
            if (cmdstat == 0 .and. exitstat == 0) then
                available = .true.
                return
            end if
        end if

        command = 'command -v ' // trim(command_name) // ' >/dev/null 2>&1'
        call execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
        available = (cmdstat == 0 .and. exitstat == 0)
    end subroutine check_command_available_runtime

    subroutine run_mkdir(path, success)
        character(len=*), intent(in) :: path
        logical, intent(out) :: success
        character(len=:), allocatable :: command
        integer :: cmdstat, exitstat

        success = .false.
        if (len_trim(path) == 0) return

        if (is_windows()) then
            command = 'mkdir "' // trim(path) // '" >NUL 2>NUL'
        else
            command = 'mkdir -p "' // trim(path) // '"'
        end if

        call execute_command_line(command, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
        success = (cmdstat == 0 .and. exitstat == 0)
    end subroutine run_mkdir

end module fortplot_system_commands
