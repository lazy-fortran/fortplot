module fortplot_logging
    !! Simple logging facility for fortplot library
    !! Allows control over console output verbosity and warning suppression
    !! 
    !! Supports environment variable-based warning suppression:
    !! - FORTPLOT_SUPPRESS_WARNINGS: Manual warning suppression control
    !! - Automatic CI detection: GITHUB_ACTIONS, CI, CONTINUOUS_INTEGRATION
    !! - FORTPLOT_FORCE_WARNINGS: Force warnings even in CI environments
    
    implicit none
    private
    
    public :: set_log_level, log_info, log_warning, log_error, log_debug
    public :: LOG_LEVEL_SILENT, LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, LOG_LEVEL_INFO, LOG_LEVEL_DEBUG
    public :: initialize_warning_suppression, is_warnings_suppressed
    
    ! Log levels (in increasing verbosity)
    integer, parameter :: LOG_LEVEL_SILENT = 0
    integer, parameter :: LOG_LEVEL_ERROR = 1  
    integer, parameter :: LOG_LEVEL_WARNING = 2
    integer, parameter :: LOG_LEVEL_INFO = 3
    integer, parameter :: LOG_LEVEL_DEBUG = 4
    
    ! Default log level (warnings and errors only)
    integer :: current_log_level = LOG_LEVEL_WARNING
    
    ! Warning suppression state
    logical :: warnings_suppressed = .false.
    logical :: suppression_initialized = .false.
    
contains

    subroutine set_log_level(level)
        !! Set the global logging level
        !! 
        !! Arguments:
        !!   level: LOG_LEVEL_SILENT, LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, 
        !!          LOG_LEVEL_INFO, or LOG_LEVEL_DEBUG
        integer, intent(in) :: level
        current_log_level = level
    end subroutine set_log_level

    subroutine log_info(message)
        !! Log an informational message
        character(len=*), intent(in) :: message
        if (current_log_level >= LOG_LEVEL_INFO) then
            print *, "[INFO] ", trim(message)
        end if
    end subroutine log_info

    subroutine log_warning(message)
        !! Log a warning message with suppression support
        !! Respects FORTPLOT_SUPPRESS_WARNINGS and CI environment detection
        character(len=*), intent(in) :: message
        
        ! Initialize suppression state if not already done
        if (.not. suppression_initialized) then
            call initialize_warning_suppression()
        end if
        
        ! Check if warnings should be suppressed
        if (warnings_suppressed) then
            return  ! Suppress warning output
        end if
        
        if (current_log_level >= LOG_LEVEL_WARNING) then
            print *, "[WARNING] ", trim(message)
        end if
    end subroutine log_warning

    subroutine log_error(message)
        !! Log an error message
        character(len=*), intent(in) :: message
        if (current_log_level >= LOG_LEVEL_ERROR) then
            print *, "[ERROR] ", trim(message)
        end if
    end subroutine log_error

    subroutine log_debug(message)
        !! Log a debug message
        character(len=*), intent(in) :: message
        if (current_log_level >= LOG_LEVEL_DEBUG) then
            print *, "[DEBUG] ", trim(message)
        end if
    end subroutine log_debug

    subroutine initialize_warning_suppression()
        !! Initialize warning suppression based on environment variables
        !! Supports manual control and automatic CI detection
        character(len=256) :: env_value
        integer :: status
        logical :: ci_detected, force_warnings
        
        if (suppression_initialized) return
        
        ! Check for manual warning suppression override
        call get_environment_variable('FORTPLOT_SUPPRESS_WARNINGS', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            warnings_suppressed = parse_boolean_env(env_value)
            suppression_initialized = .true.
            return
        end if
        
        ! Check for force warnings override (takes precedence)
        call get_environment_variable('FORTPLOT_FORCE_WARNINGS', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            force_warnings = parse_boolean_env(env_value)
            if (force_warnings) then
                warnings_suppressed = .false.
                suppression_initialized = .true.
                return
            end if
        end if
        
        ! Auto-detect CI environment
        ci_detected = detect_ci_environment()
        if (ci_detected) then
            warnings_suppressed = .true.
        else
            warnings_suppressed = .false.
        end if
        
        suppression_initialized = .true.
    end subroutine initialize_warning_suppression
    
    logical function detect_ci_environment()
        !! Detect common CI environments
        character(len=256) :: env_value
        integer :: status
        
        detect_ci_environment = .false.
        
        ! GitHub Actions
        call get_environment_variable('GITHUB_ACTIONS', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            if (parse_boolean_env(env_value)) then
                detect_ci_environment = .true.
                return
            end if
        end if
        
        ! Generic CI indicator
        call get_environment_variable('CI', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            if (parse_boolean_env(env_value)) then
                detect_ci_environment = .true.
                return
            end if
        end if
        
        ! Jenkins CI
        call get_environment_variable('CONTINUOUS_INTEGRATION', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            if (parse_boolean_env(env_value)) then
                detect_ci_environment = .true.
                return
            end if
        end if
        
        ! Jenkins BUILD_ID
        call get_environment_variable('BUILD_ID', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            detect_ci_environment = .true.
            return
        end if
        
        ! Travis CI
        call get_environment_variable('TRAVIS', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            if (parse_boolean_env(env_value)) then
                detect_ci_environment = .true.
                return
            end if
        end if
        
        ! CircleCI
        call get_environment_variable('CIRCLECI', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            if (parse_boolean_env(env_value)) then
                detect_ci_environment = .true.
                return
            end if
        end if
    end function detect_ci_environment
    
    logical function parse_boolean_env(env_value)
        !! Parse environment variable as boolean value
        !! Supports: '1', 'true', 'yes', 'on' (case-insensitive) = .true.
        !! All other values = .false.
        character(len=*), intent(in) :: env_value
        character(len=256) :: lower_value
        
        parse_boolean_env = .false.
        
        ! Convert to lowercase for comparison
        lower_value = to_lowercase(trim(env_value))
        
        if (lower_value == '1' .or. &
            lower_value == 'true' .or. &
            lower_value == 'yes' .or. &
            lower_value == 'on') then
            parse_boolean_env = .true.
        end if
    end function parse_boolean_env
    
    function to_lowercase(input_string) result(output_string)
        !! Convert string to lowercase
        character(len=*), intent(in) :: input_string
        character(len=len(input_string)) :: output_string
        integer :: i, ascii_val
        
        output_string = input_string
        do i = 1, len(input_string)
            ascii_val = iachar(input_string(i:i))
            ! Convert A-Z (65-90) to a-z (97-122)
            if (ascii_val >= 65 .and. ascii_val <= 90) then
                output_string(i:i) = achar(ascii_val + 32)
            end if
        end do
    end function to_lowercase
    
    logical function is_warnings_suppressed()
        !! Check if warnings are currently suppressed
        if (.not. suppression_initialized) then
            call initialize_warning_suppression()
        end if
        is_warnings_suppressed = warnings_suppressed
    end function is_warnings_suppressed

end module fortplot_logging