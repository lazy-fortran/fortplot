! fortplot_parameter_validation.f90 - Centralized parameter validation system
! 
! This module provides comprehensive parameter validation and warning infrastructure
! for user input safety across all public APIs.
!
! Issue #854: Parameter validation warnings for user input safety
!
module fortplot_parameter_validation
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private
    
    ! Public interface
    public :: validate_plot_dimensions
    public :: validate_color_values
    public :: validate_array_bounds
    public :: validate_file_path
    public :: validate_numeric_parameters
    public :: validation_warning
    public :: validation_error
    public :: set_warning_mode
    public :: parameter_validation_result_t
    public :: WARNING_MODE_ALL, WARNING_MODE_ERRORS, WARNING_MODE_SILENT
    
    ! Validation result type for structured reporting
    type :: parameter_validation_result_t
        logical :: is_valid
        logical :: has_warning
        character(len=256) :: message
        character(len=64) :: context
        integer :: error_code
    end type
    
    ! Warning modes
    integer, parameter :: WARNING_MODE_ALL = 0      ! Show all warnings
    integer, parameter :: WARNING_MODE_ERRORS = 1   ! Show only errors
    integer, parameter :: WARNING_MODE_SILENT = 2   ! Suppress all output
    
    ! Current warning mode (can be changed by advanced users)
    integer :: current_warning_mode = WARNING_MODE_ALL
    
    ! Parameter validation limits
    real(wp), parameter :: MIN_PLOT_DIMENSION = 0.1_wp
    real(wp), parameter :: MAX_PLOT_DIMENSION = 1000.0_wp
    real(wp), parameter :: MIN_COLOR_VALUE = 0.0_wp
    real(wp), parameter :: MAX_COLOR_VALUE = 1.0_wp
    integer, parameter :: MAX_PATH_LENGTH = 4096
    integer, parameter :: MAX_ARRAY_SIZE = 1000000  ! 1M elements safety limit
    
contains
    
    ! Set warning output mode for advanced users
    subroutine set_warning_mode(mode)
        integer, intent(in) :: mode
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
    
    ! Validate plot dimensions (width, height, figsize)
    function validate_plot_dimensions(width, height, context) result(validation)
        real(wp), intent(in) :: width, height
        character(len=*), intent(in), optional :: context
        type(parameter_validation_result_t) :: validation
        character(len=64) :: ctx
        
        ctx = "plot_dimensions"
        if (present(context)) ctx = context
        
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
            call validation_error(validation%message, ctx)
            return
        end if
        
        ! Check for unreasonably small dimensions
        if (width < MIN_PLOT_DIMENSION .or. height < MIN_PLOT_DIMENSION) then
            validation%has_warning = .true.
            write(validation%message, '(A,F0.2,A,F0.2,A,F0.1,A)') &
                "Very small plot dimensions: width=", width, ", height=", height, &
                ". Consider dimensions >= ", MIN_PLOT_DIMENSION, " for better visibility."
            call validation_warning(validation%message, ctx)
        end if
        
        ! Check for unreasonably large dimensions
        if (width > MAX_PLOT_DIMENSION .or. height > MAX_PLOT_DIMENSION) then
            validation%has_warning = .true.
            write(validation%message, '(A,F0.1,A,F0.1,A,F0.0,A)') &
                "Very large plot dimensions: width=", width, ", height=", height, &
                ". Consider dimensions <= ", MAX_PLOT_DIMENSION, " to avoid memory issues."
            call validation_warning(validation%message, ctx)
        end if
        
        ! Check for extreme aspect ratios
        if (width / height > 20.0_wp .or. height / width > 20.0_wp) then
            validation%has_warning = .true.
            write(validation%message, '(A,F0.2,A)') &
                "Extreme aspect ratio ", max(width/height, height/width), &
                ". Plot may appear distorted."
            call validation_warning(validation%message, ctx)
        end if
    end function validate_plot_dimensions
    
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
    
    ! Validate array bounds and sizes (simplified implementation)
    function validate_array_bounds(array_size, max_index, min_size, context) result(validation)
        integer, intent(in) :: array_size
        integer, intent(in), optional :: max_index, min_size
        character(len=*), intent(in), optional :: context
        type(parameter_validation_result_t) :: validation
        character(len=64) :: ctx
        
        ctx = "array_bounds"
        if (present(context)) ctx = context
        
        validation%context = ctx
        validation%is_valid = .true.
        validation%has_warning = .false.
        validation%error_code = 0
        
        ! Basic validation only - negative array size
        if (array_size < 0) then
            validation%is_valid = .false.
            validation%error_code = 4
            validation%message = "Negative array size not allowed."
            call validation_error(validation%message, ctx)
            return
        end if
        
        ! Check minimum size requirement if provided
        if (present(min_size)) then
            if (array_size < min_size) then
                if (min_size > 0) then
                    if (array_size == 0) then
                        ! Zero array size with minimum requirement is an error
                        validation%is_valid = .false.
                        validation%error_code = 5
                        write(validation%message, '(A,I0,A)') &
                            "Array size is zero but minimum required size is ", min_size, "."
                        call validation_error(validation%message, ctx)
                        return
                    else
                        ! Non-zero array size below minimum is a warning
                        validation%has_warning = .true.
                        write(validation%message, '(A,I0,A,I0,A)') &
                            "Array size ", array_size, " is below recommended minimum ", min_size, "."
                        call validation_warning(validation%message, ctx)
                    end if
                else
                    validation%has_warning = .true.
                    validation%message = "Minimum size requirement is non-positive."
                    call validation_warning(validation%message, ctx)
                end if
            end if
        end if
        
        ! Check bounds overflow if max_index is provided
        if (present(max_index)) then
            if (max_index > array_size) then
                validation%is_valid = .false.
                validation%error_code = 6
                validation%message = "Index would exceed array bounds."
                call validation_error(validation%message, ctx)
                return
            end if
        end if
        
        ! Warn about very large arrays
        if (array_size > MAX_ARRAY_SIZE) then
            validation%has_warning = .true.
            validation%message = "Very large array may cause memory issues."
            call validation_warning(validation%message, ctx)
        end if
    end function validate_array_bounds
    
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
    
    ! Validate numeric parameters for NaN/infinity handling
    function validate_numeric_parameters(values, param_name, context) result(validation)
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: param_name
        character(len=*), intent(in), optional :: context
        type(parameter_validation_result_t) :: validation
        character(len=64) :: ctx, param
        integer :: i, nan_count, inf_count
        
        ctx = "numeric_parameters"
        if (present(context)) ctx = context
        
        param = "values"
        if (present(param_name)) param = param_name
        
        validation%context = ctx
        validation%is_valid = .true.
        validation%has_warning = .false.
        validation%error_code = 0
        
        nan_count = 0
        inf_count = 0
        
        ! Count NaN and infinity values
        do i = 1, size(values)
            if (is_nan_safe(values(i))) then
                nan_count = nan_count + 1
            else if (.not. is_finite_safe(values(i))) then
                inf_count = inf_count + 1
            end if
        end do
        
        ! Report NaN values
        if (nan_count > 0) then
            if (nan_count == size(values)) then
                validation%is_valid = .false.
                validation%error_code = 10
                write(validation%message, '(A,A,A)') &
                    "All ", trim(param), " values are NaN (Not-a-Number)."
                call validation_error(validation%message, ctx)
                return
            else
                validation%has_warning = .true.
                write(validation%message, '(I0,A,I0,A,A,A)') &
                    nan_count, " of ", size(values), " ", trim(param), &
                    " values are NaN and will be skipped."
                call validation_warning(validation%message, ctx)
            end if
        end if
        
        ! Report infinity values
        if (inf_count > 0) then
            if (inf_count == size(values)) then
                validation%is_valid = .false.
                validation%error_code = 11
                write(validation%message, '(A,A,A)') &
                    "All ", trim(param), " values are infinite."
                call validation_error(validation%message, ctx)
                return
            else
                validation%has_warning = .true.
                write(validation%message, '(I0,A,I0,A,A,A)') &
                    inf_count, " of ", size(values), " ", trim(param), &
                    " values are infinite and may cause rendering issues."
                call validation_warning(validation%message, ctx)
            end if
        end if
        
        ! Check for extreme ranges that might cause numerical issues
        if (nan_count == 0 .and. inf_count == 0 .and. size(values) > 1) then
            if (maxval(values) - minval(values) >= 1.0e30_wp) then
                validation%has_warning = .true.
                write(validation%message, '(A,A,A,ES10.2,A)') &
                    trim(param), " range is extremely large (", &
                    "range = ", maxval(values) - minval(values), &
                    "). Consider scaling data."
                call validation_warning(validation%message, ctx)
            end if
        end if
    end function validate_numeric_parameters
    
    ! Safe NaN check that works across compilers
    pure function is_nan_safe(value) result(is_nan)
        real(wp), intent(in) :: value
        logical :: is_nan
        
        ! NaN is the only value that is not equal to itself
        is_nan = (value /= value)
    end function is_nan_safe
    
    ! Safe finite check that works across compilers
    pure function is_finite_safe(value) result(is_finite)
        real(wp), intent(in) :: value
        logical :: is_finite
        
        ! A finite value is neither NaN nor infinite
        ! Use a large but safe threshold instead of huge(value)
        is_finite = (value == value) .and. (abs(value) < 1.0e100_wp)
    end function is_finite_safe
    
end module fortplot_parameter_validation