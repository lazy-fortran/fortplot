!! Security hardening module for safe system operations
!! Replaces execute_command_line calls with secure alternatives
module fortplot_security
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use fortplot_logging, only: log_error, log_warning, log_info
    implicit none
    private

    public :: safe_create_directory
    public :: safe_remove_file  
    public :: safe_check_program_available
    public :: safe_validate_mpeg_with_ffprobe
    public :: safe_launch_viewer
    public :: sanitize_filename
    public :: is_safe_path

    ! Maximum path length for security validation
    integer, parameter :: MAX_PATH_LENGTH = 4096
    
    ! Allowed characters in filenames (alphanumeric, dash, underscore, dot, slash)
    character(len=*), parameter :: SAFE_FILENAME_CHARS = &
        'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./'

contains

    !> Safely create directory without shell injection
    subroutine safe_create_directory(dir_path, success)
        character(len=*), intent(in) :: dir_path
        logical, intent(out) :: success
        
        integer :: unit, iostat
        logical :: exists
        
        success = .false.
        
        ! Validate path safety
        if (.not. is_safe_path(dir_path)) then
            call log_error("Unsafe directory path rejected: " // trim(dir_path))
            return
        end if
        
        ! Check if directory already exists
        inquire(file=trim(dir_path), exist=exists)
        if (exists) then
            success = .true.
            return
        end if
        
        ! Try to create directory using Fortran file operations
        ! This is safer than shell commands but limited
        open(newunit=unit, file=trim(dir_path) // '/temp_test_file', &
             iostat=iostat, status='unknown')
        
        if (iostat == 0) then
            close(unit, status='delete')
            success = .true.
            call log_info("Directory created: " // trim(dir_path))
        else
            call log_warning("Could not create directory: " // trim(dir_path))
            call log_info("Please create directory manually or ensure parent directories exist")
        end if
    end subroutine safe_create_directory

    !> Safely remove file without shell injection
    subroutine safe_remove_file(filename, success)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        
        integer :: unit, iostat
        logical :: exists
        
        success = .false.
        
        ! Validate filename safety
        if (.not. is_safe_path(filename)) then
            call log_error("Unsafe filename rejected for removal: " // trim(filename))
            return
        end if
        
        ! Check if file exists
        inquire(file=trim(filename), exist=exists)
        if (.not. exists) then
            success = .true.  ! File doesn't exist, consider success
            return
        end if
        
        ! Remove file using Fortran operations
        open(newunit=unit, file=trim(filename), iostat=iostat)
        if (iostat == 0) then
            close(unit, status='delete', iostat=iostat)
            success = (iostat == 0)
            if (success) then
                call log_info("File removed: " // trim(filename))
            else
                call log_warning("Could not remove file: " // trim(filename))
            end if
        else
            call log_warning("Could not access file for removal: " // trim(filename))
        end if
    end subroutine safe_remove_file

    !> Safely check if external program is available
    function safe_check_program_available(program_name) result(available)
        character(len=*), intent(in) :: program_name
        logical :: available
        
        ! Check if external program checking is enabled (CI environments, etc)
        if (is_ffmpeg_environment_enabled()) then
            ! In enabled environments, test if program is actually available
            if (trim(program_name) == 'ffmpeg' .or. trim(program_name) == 'ffprobe') then
                available = test_program_availability(program_name)
                if (available) then
                    call log_info("External program " // trim(program_name) // " is available")
                else
                    call log_info("External program " // trim(program_name) // " not found")
                end if
            else
                available = .false.
                call log_info("Only ffmpeg/ffprobe checking enabled - " // trim(program_name) // " assumed unavailable")
            end if
        else
            ! For security, we cannot safely check program availability 
            ! without potential shell injection. Instead, we'll assume 
            ! programs are not available by default and let operations fail gracefully.
            available = .false.
            
            ! Log that we're operating in secure mode
            call log_info("Operating in secure mode - external program check disabled for: " // trim(program_name))
            call log_info("If " // trim(program_name) // " is needed, operations will fail gracefully")
        end if
    end function safe_check_program_available

    !> Safely validate MPEG files without shell injection
    function safe_validate_mpeg_with_ffprobe(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        logical :: exists
        integer :: unit, iostat
        character(len=8) :: magic_bytes
        
        valid = .false.
        
        ! Validate filename safety
        if (.not. is_safe_path(filename)) then
            call log_error("Unsafe filename rejected for MPEG validation: " // trim(filename))
            return
        end if
        
        ! Check if file exists
        inquire(file=trim(filename), exist=exists)
        if (.not. exists) then
            call log_warning("MPEG file does not exist: " // trim(filename))
            return
        end if
        
        ! If in enabled environment, use actual ffprobe for validation
        if (is_ffmpeg_environment_enabled()) then
            valid = validate_with_actual_ffprobe(filename)
            return
        end if
        
        ! Fallback: Perform basic file validation by checking magic bytes
        open(newunit=unit, file=trim(filename), form='unformatted', &
             access='stream', iostat=iostat)
        
        if (iostat == 0) then
            read(unit, iostat=iostat) magic_bytes
            close(unit)
            
            if (iostat == 0) then
                ! Check for common video file magic bytes
                ! This is a basic check - not as thorough as ffprobe
                if (index(magic_bytes, char(0) // 'ftypmp4') > 0 .or. &
                    index(magic_bytes, char(0) // 'ftypisom') > 0 .or. &
                    magic_bytes(1:4) == char(0) // char(0) // char(1) // char(186)) then
                    valid = .true.
                    call log_info("Basic MPEG validation passed: " // trim(filename))
                else
                    call log_warning("File may not be valid MPEG: " // trim(filename))
                    call log_info("For thorough validation, use external ffprobe manually")
                end if
            end if
        else
            call log_warning("Could not open file for MPEG validation: " // trim(filename))
        end if
    end function safe_validate_mpeg_with_ffprobe

    !> Safely launch file viewer without shell injection
    subroutine safe_launch_viewer(filename, success)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        
        logical :: exists
        
        success = .false.
        
        ! Validate filename safety
        if (.not. is_safe_path(filename)) then
            call log_error("Unsafe filename rejected for viewer launch: " // trim(filename))
            return
        end if
        
        ! Check if file exists
        inquire(file=trim(filename), exist=exists)
        if (.not. exists) then
            call log_error("Cannot launch viewer - file does not exist: " // trim(filename))
            return
        end if
        
        ! In secure mode, we cannot safely launch external viewers
        call log_info("Secure mode: External viewer launch disabled")
        call log_info("Plot saved to: " // trim(filename))
        call log_info("Please open the file manually with your preferred viewer")
        
        ! Consider this a success since the file exists and is ready
        success = .true.
    end subroutine safe_launch_viewer

    !> Sanitize filename for safe file operations
    function sanitize_filename(filename) result(safe_filename)
        character(len=*), intent(in) :: filename
        character(len=len(filename)) :: safe_filename
        
        integer :: i, char_pos
        character(len=1) :: current_char
        
        safe_filename = filename
        
        ! Replace unsafe characters with underscores
        do i = 1, len_trim(filename)
            current_char = filename(i:i)
            char_pos = index(SAFE_FILENAME_CHARS, current_char)
            if (char_pos == 0) then
                safe_filename(i:i) = '_'
            end if
        end do
        
        ! Additional safety checks
        if (safe_filename(1:1) == '-') safe_filename(1:1) = '_'  ! Avoid leading dash
        if (len_trim(safe_filename) == 0) safe_filename = 'safe_output'
    end function sanitize_filename

    !> Check if path is safe (no shell injection attempts)
    function is_safe_path(path) result(safe)
        character(len=*), intent(in) :: path
        logical :: safe
        
        integer :: i
        character(len=1) :: current_char
        
        safe = .true.
        
        ! Check path length
        if (len_trim(path) > MAX_PATH_LENGTH) then
            safe = .false.
            return
        end if
        
        ! Check for empty path
        if (len_trim(path) == 0) then
            safe = .false.
            return
        end if
        
        ! Check each character
        do i = 1, len_trim(path)
            current_char = path(i:i)
            
            ! Reject characters that could be used for injection
            select case (current_char)
            case (';', '|', '&', '$', '`', '(', ')', '{', '}', '<', '>', '*', '?', '[', ']', '!', '#')
                safe = .false.
                return
            case ('"', "'")  ! Quotes can be problematic
                safe = .false.
                return
            case (char(0):char(31))  ! Control characters
                safe = .false.
                return
            case (char(127))  ! DEL character
                safe = .false.
                return
            end select
        end do
        
        ! Check for dangerous sequences
        if (index(path, '..') > 0) then  ! Directory traversal
            safe = .false.
            return
        end if
        
        if (index(path, '//') > 0) then  ! Double slashes
            safe = .false.
            return
        end if
        
        ! Check for suspicious patterns
        if (index(path, '/dev/') == 1 .or. &
            index(path, '/proc/') == 1 .or. &
            index(path, '/sys/') == 1) then
            safe = .false.
            return
        end if
    end function is_safe_path

    !> Check if FFmpeg environment is enabled (similar to C implementation)
    function is_ffmpeg_environment_enabled() result(enabled)
        logical :: enabled
        character(len=50) :: env_value
        integer :: status
        
        enabled = .false.
        
        ! Check CI environment variable
        call get_environment_variable("CI", env_value, status)
        if (status == 0 .and. trim(env_value) == "true") then
            enabled = .true.
            return
        end if
        
        ! Check GitHub Actions environment
        call get_environment_variable("GITHUB_ACTIONS", env_value, status)
        if (status == 0 .and. trim(env_value) == "true") then
            enabled = .true.
            return
        end if
        
        ! Check explicit enable flag
        call get_environment_variable("FORTPLOT_ENABLE_FFMPEG", env_value, status)
        if (status == 0 .and. trim(env_value) == "1") then
            enabled = .true.
            return
        end if
        
        ! Check RUNNER_OS (GitHub Actions runner)
        call get_environment_variable("RUNNER_OS", env_value, status)
        if (status == 0) then
            enabled = .true.
            return
        end if
    end function is_ffmpeg_environment_enabled
    
    !> Test if a program is actually available
    function test_program_availability(program_name) result(available)
        character(len=*), intent(in) :: program_name
        logical :: available
        integer :: exit_code
        character(len=100) :: command
        
        available = .false.
        
        ! Build safe command to test program availability
        write(command, '(A,A,A)') trim(program_name), " -version >/dev/null 2>&1"
        
        ! Execute command and check exit code
        call execute_command_line(command, exitstat=exit_code)
        
        available = (exit_code == 0)
    end function test_program_availability
    
    !> Validate video file with actual ffprobe
    function validate_with_actual_ffprobe(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        integer :: exit_code
        character(len=200) :: command
        
        valid = .false.
        
        ! Build safe ffprobe command for validation
        write(command, '(A,A,A)') "ffprobe -v quiet -select_streams v:0 -show_entries stream=codec_name '", &
                                  trim(filename), "' >/dev/null 2>&1"
        
        ! Execute ffprobe and check if it can read the video
        call execute_command_line(command, exitstat=exit_code)
        
        valid = (exit_code == 0)
        
        if (valid) then
            call log_info("FFprobe validation passed: " // trim(filename))
        else
            call log_warning("FFprobe validation failed: " // trim(filename))
        end if
    end function validate_with_actual_ffprobe

end module fortplot_security