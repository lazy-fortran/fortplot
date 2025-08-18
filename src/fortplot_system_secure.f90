module fortplot_system_secure
    !! Secure system operations module
    !! Provides safe alternatives to execute_command_line for common operations
    use iso_c_binding
    implicit none
    private

    public :: create_directory_secure, open_file_secure, validate_path_secure, check_command_available, delete_file_secure

contains

    function create_directory_secure(dir_path) result(success)
        !! Securely create a directory path using C system calls
        character(len=*), intent(in) :: dir_path
        logical :: success
        
        interface
            function create_directory_c(path) result(status) bind(C, name="create_directory_c")
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: path(*)
                integer(c_int) :: status
            end function create_directory_c
        end interface
        
        character(len=len_trim(dir_path)+1, kind=c_char) :: c_path
        integer :: status
        
        ! Validate path first
        if (.not. validate_path_secure(dir_path)) then
            success = .false.
            return
        end if
        
        c_path = trim(dir_path) // c_null_char
        status = create_directory_c(c_path)
        success = (status == 0)
    end function create_directory_secure

    function open_file_secure(filename) result(success)
        !! Securely open a file with system default application
        character(len=*), intent(in) :: filename
        logical :: success
        
        interface
            function open_file_with_default_app_c(path) result(status) bind(C, name="open_file_with_default_app_c")
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: path(*)
                integer(c_int) :: status
            end function open_file_with_default_app_c
        end interface
        
        character(len=len_trim(filename)+1, kind=c_char) :: c_filename
        integer :: status
        
        ! Validate filename first
        if (.not. validate_path_secure(filename)) then
            success = .false.
            return
        end if
        
        c_filename = trim(filename) // c_null_char
        status = open_file_with_default_app_c(c_filename)
        success = (status == 0)
    end function open_file_secure

    function validate_path_secure(path) result(valid)
        !! Validate a file or directory path for security
        character(len=*), intent(in) :: path
        logical :: valid
        integer :: i, len_path
        character :: c
        
        valid = .false.
        len_path = len_trim(path)
        
        ! Check basic constraints
        if (len_path == 0 .or. len_path > 4096) return  ! PATH_MAX is typically 4096
        
        ! Check for directory traversal
        if (index(path, '..') > 0) return
        
        ! Check for null bytes
        if (index(path, char(0)) > 0) return
        
        ! Check for dangerous characters (allow reasonable path characters)
        do i = 1, len_path
            c = path(i:i)
            ! Allow alphanumeric, dash, underscore, dot, forward slash, space
            if (.not. ((c >= 'a' .and. c <= 'z') .or. &
                       (c >= 'A' .and. c <= 'Z') .or. &
                       (c >= '0' .and. c <= '9') .or. &
                       c == '-' .or. c == '_' .or. c == '.' .or. c == '/' .or. c == ' ')) then
                return
            end if
        end do
        
        ! Path passed all checks
        valid = .true.
    end function validate_path_secure

    function check_command_available(command_name) result(available)
        !! Securely check if a command is available
        character(len=*), intent(in) :: command_name
        logical :: available
        
        interface
            function check_command_available_c(cmd) result(status) bind(C, name="check_command_available_c")
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: cmd(*)
                integer(c_int) :: status
            end function check_command_available_c
        end interface
        
        character(len=len_trim(command_name)+1, kind=c_char) :: c_command
        integer :: status
        
        ! Validate command name first (only allow safe characters)
        if (.not. is_safe_command_name(command_name)) then
            available = .false.
            return
        end if
        
        c_command = trim(command_name) // c_null_char
        status = check_command_available_c(c_command)
        available = (status == 0)
    end function check_command_available

    function is_safe_command_name(cmd) result(safe)
        !! Validate command name contains only safe characters
        character(len=*), intent(in) :: cmd
        logical :: safe
        integer :: i, len_cmd
        character :: c
        
        safe = .false.
        len_cmd = len_trim(cmd)
        
        ! Check basic constraints
        if (len_cmd == 0 .or. len_cmd > 64) return  ! Reasonable command name length
        
        ! Check for safe characters only (no shell metacharacters)
        do i = 1, len_cmd
            c = cmd(i:i)
            ! Allow only alphanumeric, dash, underscore
            if (.not. ((c >= 'a' .and. c <= 'z') .or. &
                       (c >= 'A' .and. c <= 'Z') .or. &
                       (c >= '0' .and. c <= '9') .or. &
                       c == '-' .or. c == '_')) then
                return
            end if
        end do
        
        safe = .true.
    end function is_safe_command_name

    function delete_file_secure(filename) result(success)
        !! Securely delete a file using system calls
        character(len=*), intent(in) :: filename
        logical :: success
        
        interface
            function delete_file_c(path) result(status) bind(C, name="delete_file_c")
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: path(*)
                integer(c_int) :: status
            end function delete_file_c
        end interface
        
        character(len=len_trim(filename)+1, kind=c_char) :: c_filename
        integer :: status
        
        ! Validate filename first
        if (.not. validate_path_secure(filename)) then
            success = .false.
            return
        end if
        
        c_filename = trim(filename) // c_null_char
        status = delete_file_c(c_filename)
        success = (status == 0)
    end function delete_file_secure

end module fortplot_system_secure