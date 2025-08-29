module fortplot_figure_utilities
    !! Utility functions for figure operations
    !! 
    !! This module contains utility functions that support figure operations
    !! including environment detection and user input handling.

    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none

    public :: is_interactive_environment, wait_for_user_input

contains

    function is_interactive_environment() result(interactive)
        !! Determine if we're running in an interactive environment
        !! Returns false for CI, batch, or piped environments
        logical :: interactive
        character(len=256) :: env_var
        integer :: status
        
        interactive = .false.
        
        ! Check for common CI environment variables
        call get_environment_variable("CI", env_var, status=status)
        if (status == 0) return  ! CI detected, non-interactive
        
        call get_environment_variable("GITHUB_ACTIONS", env_var, status=status) 
        if (status == 0) return  ! GitHub Actions detected, non-interactive
        
        call get_environment_variable("JENKINS_URL", env_var, status=status)
        if (status == 0) return  ! Jenkins detected, non-interactive
        
        call get_environment_variable("BUILDKITE", env_var, status=status)
        if (status == 0) return  ! Buildkite detected, non-interactive
        
        ! Check if stdin is a terminal (basic heuristic)
        ! In non-interactive environments, stdin is often redirected
        call get_environment_variable("TERM", env_var, status=status)
        if (status /= 0 .or. len_trim(env_var) == 0) return
        
        ! Default to interactive if no CI detected and TERM is set
        interactive = .true.
    end function is_interactive_environment

    subroutine wait_for_user_input()
        !! Wait for user input when blocking=true
        !! Reads a single character or line from stdin to pause execution
        !! This prevents the program from continuing until user interaction
        character(len=1) :: dummy_char
        integer :: iostat
        
        ! Prompt user for input to continue
        write(output_unit, '(A)', advance='no') 'Press Enter to continue...'
        flush(output_unit)
        
        ! Read from stdin to block until user presses Enter
        read(*, '(A)', iostat=iostat) dummy_char
        
        ! If read fails (e.g., EOF or pipe), just return without hanging
        if (iostat /= 0) then
            write(output_unit, '(A)') ' (non-interactive mode detected)'
        end if
    end subroutine wait_for_user_input

end module fortplot_figure_utilities