module fortplot_coordinate_validation
    !! Input validation for coordinate arrays and edge case handling
    !! 
    !! This module provides comprehensive validation for coordinate data to prevent
    !! silent failures and ensure robust plotting across all backends.
    !! Addresses Issue #436 - single point plotting failures.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan
    use fortplot_logging, only: log_error, log_warning, log_info
    implicit none
    
    private
    public :: validate_coordinate_arrays, coordinate_validation_result_t
    public :: is_valid_single_point, is_empty_array, has_machine_precision_issues
    public :: validate_coordinate_ranges, suggest_marker_for_single_point
    
    ! Validation result type
    type :: coordinate_validation_result_t
        logical :: is_valid = .false.
        logical :: is_single_point = .false.  
        logical :: is_empty = .false.
        logical :: has_precision_issues = .false.
        logical :: has_large_values = .false.
        logical :: should_use_markers = .false.
        character(len=256) :: message = ""
        character(len=32) :: suggested_marker = ""
    end type coordinate_validation_result_t
    
    ! Constants for validation
    real(wp), parameter :: LARGE_COORDINATE_THRESHOLD = 1.0e10_wp
    real(wp), parameter :: PRECISION_THRESHOLD = 100.0_wp * epsilon(1.0_wp)
    
contains

    function validate_coordinate_arrays(x, y, context) result(validation)
        !! Comprehensive validation of coordinate arrays for plotting
        !! Returns validation result with detailed information about the data
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: context
        type(coordinate_validation_result_t) :: validation
        
        character(len=64) :: ctx
        integer :: n
        
        ! Set context for error messages
        if (present(context)) then
            ctx = context
        else
            ctx = "coordinate validation"
        end if
        
        n = size(x)
        
        ! Initialize result
        validation%is_valid = .false.
        validation%is_single_point = .false.
        validation%is_empty = .false.
        validation%has_precision_issues = .false.
        validation%has_large_values = .false.
        validation%should_use_markers = .false.
        validation%message = ""
        validation%suggested_marker = ""
        
        ! Check array size consistency
        if (size(x) /= size(y)) then
            validation%message = trim(ctx) // ": x and y arrays must have the same size"
            call log_error(trim(validation%message))
            return
        end if
        
        ! Check for empty arrays
        if (n == 0) then
            validation%is_empty = .true.
            validation%is_valid = .true.  ! Empty is valid, just a special case
            validation%message = trim(ctx) // ": empty arrays provided"
            call log_info(trim(validation%message))
            return
        end if
        
        ! Check for single point
        if (n == 1) then
            validation%is_single_point = .true.
            validation%should_use_markers = .true.
            validation%suggested_marker = "o"  ! Circle marker for single points
        end if
        
        ! Validate individual coordinate values
        if (.not. validate_coordinate_ranges(x, y, validation)) then
            validation%message = trim(ctx) // ": invalid coordinate values detected"
            call log_error(trim(validation%message))
            return
        end if
        
        ! Check for machine precision issues (coordinates too close together)
        if (n > 1) then
            if (has_machine_precision_issues(x, y)) then
                validation%has_precision_issues = .true.
                validation%message = trim(ctx) // ": coordinates may be too close for reliable rendering"
                call log_warning(trim(validation%message))
            end if
        end if
        
        ! All validations passed
        validation%is_valid = .true.
        
        ! Generate informative message
        if (validation%is_single_point) then
            validation%message = trim(ctx) // ": single point detected - consider using markers"
        else if (validation%has_precision_issues) then
            validation%message = trim(ctx) // ": coordinates validated with precision warnings"
        else
            validation%message = trim(ctx) // ": coordinates validated successfully"
        end if
        
    end function validate_coordinate_arrays

    function validate_coordinate_ranges(x, y, validation) result(is_valid)
        !! Validate that coordinate values are finite and reasonable
        real(wp), intent(in) :: x(:), y(:)
        type(coordinate_validation_result_t), intent(inout) :: validation
        logical :: is_valid
        
        integer :: i
        real(wp) :: max_abs_x, max_abs_y
        
        is_valid = .true.
        max_abs_x = 0.0_wp
        max_abs_y = 0.0_wp
        
        ! Check each coordinate
        do i = 1, size(x)
            ! Check for NaN or infinite values
            if (.not. ieee_is_finite(x(i)) .or. .not. ieee_is_finite(y(i))) then
                call log_error("Invalid coordinate detected: NaN or infinite value")
                is_valid = .false.
                return
            end if
            
            ! Track maximum values for large coordinate detection
            max_abs_x = max(max_abs_x, abs(x(i)))
            max_abs_y = max(max_abs_y, abs(y(i)))
        end do
        
        ! Check for excessively large coordinates
        if (max_abs_x > LARGE_COORDINATE_THRESHOLD .or. max_abs_y > LARGE_COORDINATE_THRESHOLD) then
            validation%has_large_values = .true.
            call log_warning("Large coordinate values detected - may cause rendering issues")
        end if
        
    end function validate_coordinate_ranges

    function has_machine_precision_issues(x, y) result(has_issues)
        !! Check if coordinates are too close together for reliable rendering
        real(wp), intent(in) :: x(:), y(:)
        logical :: has_issues
        
        integer :: i
        real(wp) :: range_x, range_y, min_diff_x, min_diff_y
        
        has_issues = .false.
        
        if (size(x) < 2) return
        
        ! Calculate data ranges
        range_x = maxval(x) - minval(x)
        range_y = maxval(y) - minval(y)
        
        ! Find minimum differences between adjacent points
        min_diff_x = huge(1.0_wp)
        min_diff_y = huge(1.0_wp)
        
        do i = 2, size(x)
            min_diff_x = min(min_diff_x, abs(x(i) - x(i-1)))
            min_diff_y = min(min_diff_y, abs(y(i) - y(i-1)))
        end do
        
        ! Check if minimum differences are too small relative to range
        if (range_x > 0.0_wp .and. min_diff_x / range_x < PRECISION_THRESHOLD) then
            has_issues = .true.
        end if
        
        if (range_y > 0.0_wp .and. min_diff_y / range_y < PRECISION_THRESHOLD) then
            has_issues = .true.
        end if
        
    end function has_machine_precision_issues

    function is_valid_single_point(x, y) result(is_single)
        !! Check if arrays represent a valid single point
        real(wp), intent(in) :: x(:), y(:)
        logical :: is_single
        
        is_single = (size(x) == 1 .and. size(y) == 1 .and. &
                    ieee_is_finite(x(1)) .and. ieee_is_finite(y(1)))
                    
    end function is_valid_single_point

    function is_empty_array(x, y) result(is_empty)
        !! Check if arrays are empty
        real(wp), intent(in) :: x(:), y(:)
        logical :: is_empty
        
        is_empty = (size(x) == 0 .or. size(y) == 0)
        
    end function is_empty_array

    function suggest_marker_for_single_point(backend_type) result(marker)
        !! Suggest appropriate marker type for single point based on backend
        character(len=*), intent(in) :: backend_type
        character(len=8) :: marker
        
        select case (trim(backend_type))
        case ('ascii')
            marker = "*"      ! Asterisk works well in ASCII
        case ('png', 'pdf')
            marker = "o"      ! Circle for graphical backends
        case default
            marker = "o"      ! Default to circle
        end select
        
    end function suggest_marker_for_single_point

end module fortplot_coordinate_validation