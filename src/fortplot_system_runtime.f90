module fortplot_system_runtime
    !! Runtime OS detection and cross-platform system operations
    !! This module avoids preprocessor issues by detecting OS at runtime
    use iso_c_binding
    use iso_fortran_env, only: int32, int64
    implicit none
    private

    public :: is_windows
    public :: create_directory_runtime
    public :: delete_file_runtime
    public :: open_with_default_app_runtime
    public :: check_command_available_runtime
    public :: map_unix_to_windows_path
    public :: normalize_path_separators

contains

    function is_windows() result(windows)
        !! Detect if running on Windows at runtime
        logical :: windows
        character(len=256) :: os_name
        integer :: status
        
        ! Try Windows-specific environment variable first
        call get_environment_variable("OS", os_name, status=status)
        if (status == 0 .and. index(os_name, "Windows") > 0) then
            windows = .true.
            return
        end if
        
        ! Check path separator convention
        call get_environment_variable("PATH", os_name, status=status)
        if (status == 0) then
            if (index(os_name, ";") > 0 .and. index(os_name, "\") > 0) then
                windows = .true.
            else
                windows = .false.
            end if
        else
            ! Fallback: check if backslash is valid in paths
            windows = .false.
        end if
    end function is_windows

    function map_unix_to_windows_path(path) result(mapped_path)
        !! Map Unix-style /tmp paths to Windows-compatible paths
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: mapped_path
        
        if (path == "/tmp") then
            mapped_path = "tmp"
        else if (len(path) >= 5 .and. path(1:5) == "/tmp/") then
            ! Map /tmp/filename to tmp/filename
            mapped_path = "tmp" // path(5:)
        else
            mapped_path = path
        end if
    end function map_unix_to_windows_path

    function normalize_path_separators(path, to_windows) result(normalized)
        !! Normalize path separators for the target platform
        character(len=*), intent(in) :: path
        logical, intent(in) :: to_windows
        character(len=:), allocatable :: normalized
        integer :: i
        
        normalized = path
        
        if (to_windows) then
            ! Convert forward slashes to backslashes
            do i = 1, len(normalized)
                if (normalized(i:i) == '/') then
                    normalized(i:i) = '\'
                end if
            end do
        else
            ! Convert backslashes to forward slashes
            do i = 1, len(normalized)
                if (normalized(i:i) == '\') then
                    normalized(i:i) = '/'
                end if
            end do
        end if
    end function normalize_path_separators

    function get_parent_directory(path) result(parent)
        !! Extract parent directory from a path
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: parent
        integer :: last_sep, i
        
        last_sep = 0
        do i = len_trim(path), 1, -1
            if (path(i:i) == '/' .or. path(i:i) == '\') then
                last_sep = i - 1
                exit
            end if
        end do
        
        if (last_sep > 0) then
            parent = path(1:last_sep)
        else
            parent = ""
        end if
    end function get_parent_directory

    recursive subroutine create_directory_runtime(path, success)
        !! Create directory with cross-platform support
        character(len=*), intent(in) :: path
        logical, intent(out) :: success
        character(len=:), allocatable :: effective_path
        character(len=:), allocatable :: command
        character(len=:), allocatable :: parent_dir
        integer :: exitstat, cmdstat
        character(len=256) :: cmdmsg
        
        success = .false.
        
        ! Map Unix paths for Windows if needed
        if (is_windows()) then
            effective_path = map_unix_to_windows_path(path)
            effective_path = normalize_path_separators(effective_path, .true.)
            
            ! First try to create parent directories if needed
            parent_dir = get_parent_directory(effective_path)
            if (len_trim(parent_dir) > 0) then
                ! Create parent first (recursive)
                call create_directory_runtime(parent_dir, success)
            end if
            
            ! Use Windows mkdir command (doesn't need -p flag)
            command = 'mkdir "' // trim(effective_path) // '" 2>nul'
        else
            effective_path = path
            ! Use Unix mkdir with -p for parent directories
            command = 'mkdir -p "' // trim(effective_path) // '" 2>/dev/null'
        end if
        
        call execute_command_line(command, exitstat=exitstat, &
                                 cmdstat=cmdstat, cmdmsg=cmdmsg)
        
        ! Check if directory exists (either created or already existed)
        if (is_windows()) then
            command = 'if exist "' // trim(effective_path) // '\" exit 0'
        else
            command = 'test -d "' // trim(effective_path) // '"'
        end if
        
        call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
        success = (exitstat == 0 .and. cmdstat == 0)
    end subroutine create_directory_runtime

    subroutine delete_file_runtime(filename, success)
        !! Delete file with cross-platform support
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        character(len=:), allocatable :: effective_path
        character(len=:), allocatable :: command
        integer :: exitstat, cmdstat
        character(len=256) :: cmdmsg
        
        success = .false.
        
        if (is_windows()) then
            effective_path = map_unix_to_windows_path(filename)
            effective_path = normalize_path_separators(effective_path, .true.)
            command = 'del /f /q "' // trim(effective_path) // '" 2>nul'
        else
            effective_path = filename
            command = 'rm -f "' // trim(effective_path) // '" 2>/dev/null'
        end if
        
        call execute_command_line(command, exitstat=exitstat, &
                                 cmdstat=cmdstat, cmdmsg=cmdmsg)
        
        ! Success if file is gone (deleted or didn't exist)
        success = (exitstat == 0 .and. cmdstat == 0)
    end subroutine delete_file_runtime

    subroutine open_with_default_app_runtime(filename, success)
        !! Open file with default application
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        character(len=:), allocatable :: effective_path
        character(len=:), allocatable :: command
        integer :: exitstat, cmdstat
        character(len=256) :: cmdmsg
        
        success = .false.
        
        if (is_windows()) then
            effective_path = map_unix_to_windows_path(filename)
            effective_path = normalize_path_separators(effective_path, .true.)
            ! Use start command to open with default app
            command = 'start "" "' // trim(effective_path) // '"'
        else
            ! Try xdg-open first (Linux), then open (macOS)
            effective_path = filename
            command = 'xdg-open "' // trim(effective_path) // '" 2>/dev/null || ' // &
                     'open "' // trim(effective_path) // '" 2>/dev/null'
        end if
        
        call execute_command_line(command, exitstat=exitstat, &
                                 cmdstat=cmdstat, cmdmsg=cmdmsg)
        
        success = (exitstat == 0 .and. cmdstat == 0)
    end subroutine open_with_default_app_runtime

    subroutine check_command_available_runtime(command_name, available)
        !! Check if command is available on system
        character(len=*), intent(in) :: command_name
        logical, intent(out) :: available
        character(len=:), allocatable :: command
        integer :: exitstat, cmdstat
        
        available = .false.
        
        if (is_windows()) then
            command = 'where "' // trim(command_name) // '" >nul 2>&1'
        else
            command = 'which "' // trim(command_name) // '" >/dev/null 2>&1'
        end if
        
        call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
        available = (exitstat == 0 .and. cmdstat == 0)
    end subroutine check_command_available_runtime

end module fortplot_system_runtime