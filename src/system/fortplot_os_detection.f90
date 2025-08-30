module fortplot_os_detection
    !! Operating system detection and environment utilities
    !! 
    !! This module handles OS detection at runtime and environment
    !! variable operations for cross-platform compatibility.

    implicit none
    private

    public :: is_debug_enabled
    public :: is_windows

contains

    function is_debug_enabled() result(debug_enabled)
        !! Check if debug logging is enabled via environment variable
        logical :: debug_enabled
        character(len=256) :: debug_env
        integer :: status
        
        debug_enabled = .false.
        
        ! Check for FORTPLOT_DEBUG_TIMEOUT environment variable
        call get_environment_variable("FORTPLOT_DEBUG_TIMEOUT", debug_env, status=status)
        if (status == 0 .and. len_trim(debug_env) > 0) then
            if (trim(debug_env) == "1" .or. trim(debug_env) == "true") then
                debug_enabled = .true.
            end if
        end if
    end function is_debug_enabled

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
        
        ! Try with OSTYPE (Unix/Linux/macOS)
        call get_environment_variable("OSTYPE", os_name, status=status)
        if (status == 0) then
            ! If OSTYPE is set, we're probably on Unix-like system
            windows = .false.
            return
        end if
        
        ! Try ComSpec (Windows-specific)
        call get_environment_variable("ComSpec", os_name, status=status)
        if (status == 0 .and. len_trim(os_name) > 0) then
            windows = .true.
            return
        end if
        
        ! Try WINDIR (Windows-specific)
        call get_environment_variable("WINDIR", os_name, status=status)
        if (status == 0 .and. len_trim(os_name) > 0) then
            windows = .true.
            return
        end if
        
        ! Default to non-Windows (more common in scientific computing)
        windows = .false.
    end function is_windows

end module fortplot_os_detection