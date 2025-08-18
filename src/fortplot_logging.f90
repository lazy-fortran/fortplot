module fortplot_logging
    !! Simple logging facility for fortplot library
    !! Allows control over console output verbosity
    
    implicit none
    private
    
    public :: set_log_level, log_info, log_warning, log_error, log_debug
    public :: LOG_LEVEL_SILENT, LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, LOG_LEVEL_INFO, LOG_LEVEL_DEBUG
    
    ! Log levels (in increasing verbosity)
    integer, parameter :: LOG_LEVEL_SILENT = 0
    integer, parameter :: LOG_LEVEL_ERROR = 1  
    integer, parameter :: LOG_LEVEL_WARNING = 2
    integer, parameter :: LOG_LEVEL_INFO = 3
    integer, parameter :: LOG_LEVEL_DEBUG = 4
    
    ! Default log level (warnings and errors only)
    integer :: current_log_level = LOG_LEVEL_WARNING
    
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
        !! Log a warning message
        character(len=*), intent(in) :: message
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

end module fortplot_logging