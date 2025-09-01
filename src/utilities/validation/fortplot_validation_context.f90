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
    public :: validation_context_t
    public :: parameter_validation_result_t
    public :: WARNING_MODE_ALL, WARNING_MODE_ERRORS, WARNING_MODE_SILENT
    ! Cleanup and inspection helpers (for tests and long-running apps)
    public :: reset_warning_tracking
    public :: is_warning_tracking_active
    
    ! Current warning mode (can be changed by advanced users)
    integer :: current_warning_mode = WARNING_MODE_ALL
    
    ! Warning deduplication system to prevent spam
    character(len=512), allocatable :: issued_warnings(:)
    integer :: warning_count = 0
    integer, parameter :: MAX_TRACKED_WARNINGS = 1000
    
contains
    
    ! DEPRECATED: validation_warning is legacy; prefer validation_warning_with_context
    
    ! Output warning message with context (with spam prevention)
    subroutine validation_warning(message, context)
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: context
        character(len=512) :: warning_key
        integer :: i
        logical :: already_warned
        
        if (current_warning_mode == WARNING_MODE_SILENT) return
        
        ! Create unique warning key for deduplication
        if (present(context)) then
            warning_key = trim(context) // ": " // trim(message)
        else
            warning_key = trim(message)
        end if
        
        ! Check if we've already issued this warning
        already_warned = .false.
        if (allocated(issued_warnings)) then
            do i = 1, warning_count
                if (trim(issued_warnings(i)) == trim(warning_key)) then
                    already_warned = .true.
                    exit
                end if
            end do
        end if
        
        ! Only show warning if it's the first occurrence
        if (.not. already_warned) then
            ! Track this warning for future deduplication
            call track_warning(warning_key)
            
            if (present(context)) then
                print *, "Warning [", trim(context), "]: ", trim(message)
            else
                print *, "Warning: ", trim(message)
            end if
        end if
    end subroutine validation_warning
    
    ! DEPRECATED: validation_error is legacy; prefer validation_error_with_context
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
    
    ! Private helper to track issued warnings for spam prevention
    subroutine track_warning(warning_key)
        character(len=*), intent(in) :: warning_key
        character(len=512), allocatable :: temp_warnings(:)
        integer :: i
        
        if (.not. allocated(issued_warnings)) then
            allocate(issued_warnings(MAX_TRACKED_WARNINGS))
            warning_count = 0
        end if
        
        ! Add warning if we have space
        if (warning_count < MAX_TRACKED_WARNINGS) then
            warning_count = warning_count + 1
            issued_warnings(warning_count) = warning_key
        else
            ! If buffer is full, shift warnings and add new one
            do i = 1, MAX_TRACKED_WARNINGS - 1
                issued_warnings(i) = issued_warnings(i + 1)
            end do
            issued_warnings(MAX_TRACKED_WARNINGS) = warning_key
        end if
    end subroutine track_warning
    
    ! Reset and cleanup warning deduplication tracking to prevent permanent allocation
    subroutine reset_warning_tracking()
        if (allocated(issued_warnings)) then
            deallocate(issued_warnings)
        end if
        warning_count = 0
    end subroutine reset_warning_tracking
    
    ! Inquiry: check if warning tracking storage is currently allocated
    logical function is_warning_tracking_active()
        is_warning_tracking_active = allocated(issued_warnings)
    end function is_warning_tracking_active
    
end module fortplot_validation_context
