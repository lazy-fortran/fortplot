! fortplot_validation_core.f90 - Core validation functions for dimensions, colors, and files
! 
! This module provides core parameter validation functions for plot dimensions,
! color values, and file path validation.
!
! Issue #854: Parameter validation warnings for user input safety
! Issue #871: Thread-safe validation using context system
!
module fortplot_validation_core
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_validation_context, only: validation_context_t, parameter_validation_result_t, &
                                          validation_warning, validation_error, &
                                          validation_warning_with_context, validation_error_with_context, &
                                          default_validation_context
    implicit none
    private
    
    ! Public interface for core validation functions
    public :: validate_plot_dimensions
    public :: validate_plot_dimensions_with_context
    public :: validate_color_values
    public :: validate_file_path
    
    ! Parameter validation limits
    real(wp), parameter :: MIN_PLOT_DIMENSION = 0.1_wp
    real(wp), parameter :: MAX_PLOT_DIMENSION = 1000.0_wp
    real(wp), parameter :: MIN_COLOR_VALUE = 0.0_wp
    real(wp), parameter :: MAX_COLOR_VALUE = 1.0_wp
    integer, parameter :: MAX_PATH_LENGTH = 4096
    
contains
    
    ! Validate plot dimensions (width, height, figsize)
    ! LEGACY: Uses global state - prefer validate_plot_dimensions_with_context
    function validate_plot_dimensions(width, height, context) result(validation)
        real(wp), intent(in) :: width, height
        character(len=*), intent(in), optional :: context
        type(parameter_validation_result_t) :: validation
        
        ! Issue #871: Delegate to context-aware version using current global state
        validation = validate_plot_dimensions_with_context(width, height, &
                                                          default_validation_context(), context)
    end function validate_plot_dimensions
    
    ! Context-aware plot dimensions validation (NEW: eliminates global state dependency)
    function validate_plot_dimensions_with_context(width, height, validation_ctx, context) result(validation)
        real(wp), intent(in) :: width, height
        type(validation_context_t), intent(in), optional :: validation_ctx
        character(len=*), intent(in), optional :: context
        type(parameter_validation_result_t) :: validation
        character(len=64) :: ctx
        type(validation_context_t) :: vctx
        
        ! Use provided context or default
        if (present(validation_ctx)) then
            vctx = validation_ctx
        else
            vctx = validation_context_t()  ! Default context
        end if
        
        ctx = "plot_dimensions"
        if (present(context)) ctx = context
        if (len_trim(vctx%context_name) > 0) ctx = vctx%context_name
        
        validation%context = ctx
        validation%is_valid = .true.
        validation%has_warning = .false.
        validation%error_code = 0
        
        ! Check for negative dimensions
        if (width <= 0.0_wp .or. height <= 0.0_wp) then
            validation%is_valid = .false.
            validation%error_code = 1
            write(validation%message, '(A,F0.2,A,F0.2,A)') &
                "Negative or zero plot dimensions: width=", width, ", height=", height, &
                ". Dimensions must be positive."
            call validation_error_with_context(validation%message, ctx, vctx)
            return
        end if
        
        ! Check for unreasonably small dimensions
        if (width < MIN_PLOT_DIMENSION .or. height < MIN_PLOT_DIMENSION) then
            validation%has_warning = .true.
            write(validation%message, '(A,F0.2,A,F0.2,A,F0.1,A)') &
                "Very small plot dimensions: width=", width, ", height=", height, &
                ". Consider dimensions >= ", MIN_PLOT_DIMENSION, " for better visibility."
            call validation_warning_with_context(validation%message, ctx, vctx)
        end if
        
        ! Check for unreasonably large dimensions
        if (width > MAX_PLOT_DIMENSION .or. height > MAX_PLOT_DIMENSION) then
            validation%has_warning = .true.
            write(validation%message, '(A,F0.1,A,F0.1,A,F0.0,A)') &
                "Very large plot dimensions: width=", width, ", height=", height, &
                ". Consider dimensions <= ", MAX_PLOT_DIMENSION, " to avoid memory issues."
            call validation_warning_with_context(validation%message, ctx, vctx)
        end if
        
        ! Check for extreme aspect ratios
        if (width / height > 20.0_wp .or. height / width > 20.0_wp) then
            validation%has_warning = .true.
            write(validation%message, '(A,F0.2,A)') &
                "Extreme aspect ratio ", max(width/height, height/width), &
                ". Plot may appear distorted."
            call validation_warning_with_context(validation%message, ctx, vctx)
        end if
    end function validate_plot_dimensions_with_context
    
    ! Validate color values (RGB components, alpha values)  
    function validate_color_values(red, green, blue, alpha, context) result(validation)
        real(wp), intent(in) :: red, green, blue
        real(wp), intent(in), optional :: alpha
        character(len=*), intent(in), optional :: context
        type(parameter_validation_result_t) :: validation
        character(len=64) :: ctx
        
        ctx = "color_values"
        if (present(context)) ctx = context
        
        validation%context = ctx
        validation%is_valid = .true.
        validation%has_warning = .false.
        validation%error_code = 0
        
        ! Simplified implementation - just check basic range without complex formatting
        if (red < MIN_COLOR_VALUE .or. red > MAX_COLOR_VALUE .or. &
            green < MIN_COLOR_VALUE .or. green > MAX_COLOR_VALUE .or. &
            blue < MIN_COLOR_VALUE .or. blue > MAX_COLOR_VALUE) then
            validation%has_warning = .true.
            validation%message = "Color components outside range [0.0,1.0]. Will be clamped."
            call validation_warning(validation%message, ctx)
        end if
        
        if (present(alpha)) then
            if (alpha < MIN_COLOR_VALUE .or. alpha > MAX_COLOR_VALUE) then
                validation%has_warning = .true.
                validation%message = "Alpha component outside range [0.0,1.0]. Will be clamped."
                call validation_warning(validation%message, ctx)
            end if
        end if
    end function validate_color_values
    
    ! Validate file paths and operations
    function validate_file_path(file_path, check_parent, context) result(validation)
        character(len=*), intent(in) :: file_path
        logical, intent(in), optional :: check_parent
        character(len=*), intent(in), optional :: context
        type(parameter_validation_result_t) :: validation
        character(len=64) :: ctx
        logical :: check_parent_dir
        integer :: path_len
        character(len=:), allocatable :: parent_dir
        integer :: last_slash
        
        ctx = "file_path"
        if (present(context)) ctx = context
        
        check_parent_dir = .false.
        if (present(check_parent)) check_parent_dir = check_parent
        
        validation%context = ctx
        validation%is_valid = .true.
        validation%has_warning = .false.
        validation%error_code = 0
        
        path_len = len_trim(file_path)
        
        ! Check for empty path
        if (path_len == 0) then
            validation%is_valid = .false.
            validation%error_code = 7
            validation%message = "Empty file path provided."
            call validation_error(validation%message, ctx)
            return
        end if
        
        ! Check for path length limits
        if (path_len > MAX_PATH_LENGTH) then
            validation%is_valid = .false.
            validation%error_code = 8
            write(validation%message, '(A,I0,A,I0,A)') &
                "File path too long: ", path_len, " characters (max ", MAX_PATH_LENGTH, ")."
            call validation_error(validation%message, ctx)
            return
        end if
        
        ! Check for potentially problematic characters
        if (index(file_path, char(0)) > 0) then
            validation%is_valid = .false.
            validation%error_code = 9
            validation%message = "File path contains null characters."
            call validation_error(validation%message, ctx)
            return
        end if
        
        ! Check for relative path traversal patterns (security)
        if (index(file_path, "../") > 0 .or. index(file_path, "..\\") > 0) then
            validation%has_warning = .true.
            validation%message = "File path contains '..' which may access parent directories."
            call validation_warning(validation%message, ctx)
        end if
        
        ! Check parent directory if requested
        if (check_parent_dir) then
            last_slash = max(index(file_path, "/", back=.true.), &
                           index(file_path, "\", back=.true.))
            if (last_slash > 0) then
                parent_dir = file_path(1:last_slash-1)
                ! This is where we would check if parent directory exists
                ! For now, just provide informational message
                call validation_warning("Parent directory existence not verified: " // &
                                       parent_dir, ctx)
            end if
        end if
    end function validate_file_path
    
end module fortplot_validation_core