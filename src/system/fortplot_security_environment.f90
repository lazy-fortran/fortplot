!! Environment detection and program validation for security
!! Handles program availability checking, environment detection, and MPEG validation
module fortplot_security_environment
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use fortplot_logging, only: log_error, log_warning, log_info
    use fortplot_security_core, only: is_safe_path, check_path_exists
    implicit none
    private

    public :: safe_check_program_available
    public :: safe_validate_mpeg_with_ffprobe
    public :: safe_launch_viewer
    public :: is_imagemagick_environment_enabled

    ! Security-related constants
    integer, parameter :: MAX_COMMAND_LENGTH = 1024  ! Maximum command line length
    integer, parameter :: SMALL_COMMAND_LENGTH = 512  ! Small command buffer size

contains

    !> Safely check if a program is available without shell execution
    function safe_check_program_available(program_name) result(available)
        character(len=*), intent(in) :: program_name
        logical :: available
        
        if (.not. is_safe_path(program_name)) then
            call log_error("Security: Invalid program name: " // trim(program_name))
            available = .false.
            return
        end if
        
        available = check_program_in_enabled_env(program_name)
    end function safe_check_program_available

    !> Check program availability in enabled environments
    function check_program_in_enabled_env(program_name) result(available)
        character(len=*), intent(in) :: program_name
        logical :: available
        
        available = .false.
        
        select case (trim(program_name))
        case ('ffmpeg', 'ffprobe')
            if (is_ffmpeg_environment_enabled()) then
                available = test_program_availability(program_name)
            else
                call log_secure_mode_message(program_name)
            end if
        case ('convert', 'identify')
            if (is_imagemagick_environment_enabled()) then
                available = test_program_availability(program_name)
            else
                call log_secure_mode_message(program_name)
            end if
        case default
            if (is_development_environment_enabled()) then
                available = test_program_availability(program_name)
            else
                call log_secure_mode_message(program_name)
            end if
        end select
    end function check_program_in_enabled_env

    !> Log secure mode message for disabled programs
    subroutine log_secure_mode_message(program_name)
        character(len=*), intent(in) :: program_name
        
        call log_info("Secure mode: " // trim(program_name) // " disabled in current environment")
    end subroutine log_secure_mode_message

    !> Safely validate MPEG file using ffprobe or magic bytes
    function safe_validate_mpeg_with_ffprobe(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        if (.not. is_safe_path(filename)) then
            call log_error("Security: Invalid filename for MPEG validation: " // trim(filename))
            valid = .false.
            return
        end if
        
        if (.not. check_path_exists(filename)) then
            call log_error("File does not exist for MPEG validation: " // trim(filename))
            valid = .false.
            return
        end if
        
        ! Try ffprobe validation first if available
        if (is_ffmpeg_environment_enabled()) then
            valid = validate_with_actual_ffprobe(filename)
        else
            ! Fallback to magic bytes validation
            valid = validate_mpeg_magic_bytes(filename)
        end if
    end function safe_validate_mpeg_with_ffprobe

    !> Validate MPEG using magic bytes inspection
    function validate_mpeg_magic_bytes(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        integer :: unit, iostat
        character(len=12) :: magic_bytes
        
        valid = .false.
        
        open(newunit=unit, file=trim(filename), form='unformatted', &
             access='stream', iostat=iostat, status='old')
        
        if (iostat /= 0) then
            call log_error("Cannot open file for magic byte validation: " // trim(filename))
            return
        end if
        
        read(unit, iostat=iostat) magic_bytes
        close(unit)
        
        if (iostat /= 0) then
            call log_error("Cannot read magic bytes from file: " // trim(filename))
            return
        end if
        
        valid = check_mp4_magic_bytes(magic_bytes, filename)
    end function validate_mpeg_magic_bytes

    !> Check MP4 magic bytes
    function check_mp4_magic_bytes(magic_bytes, filename) result(valid)
        character(len=*), intent(in) :: magic_bytes
        character(len=*), intent(in) :: filename
        logical :: valid
        
        valid = .false.
        
        ! Check for various MP4/MPEG container signatures
        if (len(magic_bytes) >= 8) then
            ! Check for "ftyp" at bytes 4-7 (MP4/MOV signature)
            if (magic_bytes(5:8) == 'ftyp') then
                valid = .true.
                call log_info("Valid MP4 file detected: " // trim(filename))
            end if
        end if
        
        if (.not. valid) then
            call log_warning("No valid MPEG magic bytes found in: " // trim(filename))
        end if
    end function check_mp4_magic_bytes

    !> Safely launch file viewer
    subroutine safe_launch_viewer(filename, success)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        
        if (.not. is_safe_path(filename)) then
            call log_error("Security: Invalid filename for viewer: " // trim(filename))
            success = .false.
            return
        end if
        
        if (.not. check_path_exists(filename)) then
            call log_error("File does not exist for viewing: " // trim(filename))
            success = .false.
            return
        end if
        
        ! In secure mode, we don't launch external viewers
        if (.not. is_development_environment_enabled()) then
            call log_info("Secure mode: External viewer disabled for: " // trim(filename))
            success = .false.
            return
        end if
        
        ! For development environments, we could implement safe viewer launching
        call log_info("Development mode: Would launch viewer for: " // trim(filename))
        success = .true.  ! Simulate successful launch
    end subroutine safe_launch_viewer

    !> Check if FFmpeg environment is enabled
    function is_ffmpeg_environment_enabled() result(enabled)
        logical :: enabled
        
        enabled = check_ci_environment() .or. &
                  check_ffmpeg_explicit_flag() .or. &
                  is_development_environment_enabled()
    end function is_ffmpeg_environment_enabled

    !> Check if ImageMagick environment is enabled
    function is_imagemagick_environment_enabled() result(enabled)
        logical :: enabled
        
        enabled = check_ci_environment() .or. &
                  check_imagemagick_explicit_flag() .or. &
                  is_development_environment_enabled()
    end function is_imagemagick_environment_enabled

    !> Check if running in CI environment
    function check_ci_environment() result(is_ci)
        logical :: is_ci
        
        is_ci = check_github_actions_environment() .or. &
                check_runner_os_environment()
    end function check_ci_environment

    !> Check if running in GitHub Actions
    function check_github_actions_environment() result(is_github)
        logical :: is_github
        
        character(len=256) :: github_actions
        
        call get_environment_variable('GITHUB_ACTIONS', github_actions)
        is_github = trim(github_actions) == 'true'
    end function check_github_actions_environment

    !> Check FFmpeg explicit enable flag
    function check_ffmpeg_explicit_flag() result(is_enabled)
        logical :: is_enabled
        
        character(len=256) :: ffmpeg_flag
        
        call get_environment_variable('FORTPLOT_ENABLE_FFMPEG', ffmpeg_flag)
        is_enabled = trim(ffmpeg_flag) == '1' .or. trim(ffmpeg_flag) == 'true'
    end function check_ffmpeg_explicit_flag

    !> Check ImageMagick explicit enable flag
    function check_imagemagick_explicit_flag() result(is_enabled)
        logical :: is_enabled
        
        character(len=256) :: imagemagick_flag
        
        call get_environment_variable('FORTPLOT_ENABLE_IMAGEMAGICK', imagemagick_flag)
        is_enabled = trim(imagemagick_flag) == '1' .or. trim(imagemagick_flag) == 'true'
    end function check_imagemagick_explicit_flag

    !> Check runner OS environment variable
    function check_runner_os_environment() result(has_runner)
        logical :: has_runner
        
        character(len=256) :: runner_os
        
        call get_environment_variable('RUNNER_OS', runner_os)
        has_runner = len_trim(runner_os) > 0
    end function check_runner_os_environment

    !> Check if development environment is enabled
    function is_development_environment_enabled() result(enabled)
        logical :: enabled
        
        enabled = check_fmp_explicit_flag() .or. &
                  check_development_explicit_flag()
    end function is_development_environment_enabled

    !> Check FPM explicit development flag
    function check_fmp_explicit_flag() result(is_enabled)
        logical :: is_enabled
        
        character(len=256) :: fpm_flag
        
        call get_environment_variable('FPM_DEV', fpm_flag)
        is_enabled = trim(fpm_flag) == '1' .or. trim(fpm_flag) == 'true'
    end function check_fmp_explicit_flag

    !> Check development explicit enable flag
    function check_development_explicit_flag() result(is_enabled)
        logical :: is_enabled
        
        character(len=256) :: dev_flag
        
        call get_environment_variable('FORTPLOT_DEV', dev_flag)
        is_enabled = trim(dev_flag) == '1' .or. trim(dev_flag) == 'true'
    end function check_development_explicit_flag

    !> Test program availability (placeholder implementation)
    function test_program_availability(program_name) result(available)
        character(len=*), intent(in) :: program_name
        logical :: available
        
        ! Placeholder: In real implementation, this would safely test program availability
        available = .false.
    end function test_program_availability

    !> Validate with actual ffprobe (placeholder implementation)
    function validate_with_actual_ffprobe(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        ! Placeholder: In real implementation, this would use ffprobe safely
        ! For now, fallback to magic bytes
        valid = validate_mpeg_magic_bytes(filename)
    end function validate_with_actual_ffprobe

end module fortplot_security_environment