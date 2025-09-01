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
        !! Uses a C helper wrapping getenv() to ensure dynamic updates
        !! from the current process environment are observed reliably.
        use, intrinsic :: iso_c_binding, only: c_int
        logical :: debug_enabled
        interface
            function fortplot_is_debug_enabled() bind(C, name="fortplot_is_debug_enabled") &
                    result(res)
                import :: c_int
                integer(c_int) :: res
            end function fortplot_is_debug_enabled
        end interface

        debug_enabled = (fortplot_is_debug_enabled() /= 0)
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
