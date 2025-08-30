! fortplot_validation_context.f90 - Validation context and warning system
! 
! This module provides warning infrastructure and context management
! for parameter validation across the FortPlot library.
!
! Issue #854: Parameter validation warnings for user input safety
! Issue #871: Thread-safe validation context system
!
module fortplot_validation_context
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    private
    
    ! Warning modes (must be defined before types that use them)
    integer, parameter :: WARNING_MODE_ALL = 0      ! Show all warnings
    integer, parameter :: WARNING_MODE_ERRORS = 1   ! Show only errors
    integer, parameter :: WARNING_MODE_SILENT = 2   ! Suppress all output
    
    ! Validation context type for controlling validation behavior
    type :: validation_context_t
        integer :: warning_mode = WARNING_MODE_ALL
        logical :: suppress_output = .false.
        character(len=64) :: context_name = ""
    end type
    
    ! Validation result type for structured reporting
    type :: parameter_validation_result_t
        logical :: is_valid
        logical :: has_warning
        character(len=256) :: message
        character(len=64) :: context
        integer :: error_code
    end type
    
    ! Public interface for context system
    public :: validation_warning
    public :: validation_error
    public :: validation_warning_with_context
    public :: validation_error_with_context
    public :: default_validation_context
    public :: set_warning_mode
    public :: validation_context_t
    public :: parameter_validation_result_t
    public :: WARNING_MODE_ALL, WARNING_MODE_ERRORS, WARNING_MODE_SILENT
    
    ! Current warning mode (can be changed by advanced users)
    integer :: current_warning_mode = WARNING_MODE_ALL
    
contains
    
    ! Set warning output mode for advanced users
    ! DEPRECATED: Use validation_context_t instead for thread-safe operation
    subroutine set_warning_mode(mode)
        integer, intent(in) :: mode
        
        ! Issue #871: Global state deprecated for thread safety
        print *, "[DEPRECATED] set_warning_mode: Global warning state violates thread safety."
        print *, "             Use validation_context_t parameter in new validation functions."
        print *, "             This function will be removed in v2.0."
        
        if (mode >= WARNING_MODE_ALL .and. mode <= WARNING_MODE_SILENT) then
            current_warning_mode = mode
        else
            call validation_warning("Invalid warning mode, using default (all warnings)", &
                                   "set_warning_mode")
        end if
    end subroutine set_warning_mode
    
    ! Output warning message with context
    subroutine validation_warning(message, context)
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: context
        
        if (current_warning_mode == WARNING_MODE_SILENT) return
        
        if (present(context)) then
            print *, "Warning [", trim(context), "]: ", trim(message)
        else
            print *, "Warning: ", trim(message)
        end if
    end subroutine validation_warning
    
    ! Output error message with context
    subroutine validation_error(message, context)
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: context
        
        if (current_warning_mode == WARNING_MODE_SILENT) return
        
        if (present(context)) then
            print *, "Error [", trim(context), "]: ", trim(message)
        else
            print *, "Error: ", trim(message)
        end if
    end subroutine validation_error
    
    ! Context-aware warning output (NEW: eliminates global state dependency)
    subroutine validation_warning_with_context(message, context_param, validation_ctx)
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: context_param
        type(validation_context_t), intent(in), optional :: validation_ctx
        
        type(validation_context_t) :: ctx
        
        ! Use provided context or default
        if (present(validation_ctx)) then
            ctx = validation_ctx
        else
            ctx = validation_context_t()  ! Default context
        end if
        
        ! Respect context-specific warning mode
        if (ctx%warning_mode == WARNING_MODE_SILENT .or. ctx%suppress_output) return
        
        if (present(context_param)) then
            print *, "Warning [", trim(context_param), "]: ", trim(message)
        else if (len_trim(ctx%context_name) > 0) then
            print *, "Warning [", trim(ctx%context_name), "]: ", trim(message)
        else
            print *, "Warning: ", trim(message)
        end if
    end subroutine validation_warning_with_context
    
    ! Context-aware error output (NEW: eliminates global state dependency)
    subroutine validation_error_with_context(message, context_param, validation_ctx)
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: context_param
        type(validation_context_t), intent(in), optional :: validation_ctx
        
        type(validation_context_t) :: ctx
        
        ! Use provided context or default
        if (present(validation_ctx)) then
            ctx = validation_ctx
        else
            ctx = validation_context_t()  ! Default context
        end if
        
        ! Respect context-specific warning mode (errors shown unless silent)
        if (ctx%warning_mode == WARNING_MODE_SILENT .or. ctx%suppress_output) return
        
        if (present(context_param)) then
            print *, "Error [", trim(context_param), "]: ", trim(message)
        else if (len_trim(ctx%context_name) > 0) then
            print *, "Error [", trim(ctx%context_name), "]: ", trim(message)
        else
            print *, "Error: ", trim(message)
        end if
    end subroutine validation_error_with_context
    
    ! Helper function to create default validation context with current global state
    ! TRANSITIONAL: Bridges legacy global state to new context-based approach
    function default_validation_context() result(ctx)
        type(validation_context_t) :: ctx
        
        ctx%warning_mode = current_warning_mode
        ctx%suppress_output = .false.
        ctx%context_name = ""
    end function default_validation_context
    
end module fortplot_validation_context