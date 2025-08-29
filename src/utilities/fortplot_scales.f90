module fortplot_scales
    !! Scale transformation module for coordinate system transformations
    !! 
    !! This module provides functions for transforming data coordinates
    !! using different scale types: linear, logarithmic, and symmetric logarithmic.
    !! Follows Single Responsibility Principle by focusing solely on scale transformations.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: apply_scale_transform, apply_inverse_scale_transform
    public :: transform_x_coordinate, transform_y_coordinate
    public :: clamp_extreme_log_range
    
    ! Safe log range limits to prevent precision loss and overflow
    ! Based on practical plotting limits and numerical stability
    real(wp), parameter :: MAX_LOG_RANGE = 50.0_wp      ! 50 orders of magnitude max
    real(wp), parameter :: MIN_LOG_VALUE = -25.0_wp     ! 10^-25 minimum
    real(wp), parameter :: MAX_LOG_VALUE = 25.0_wp      ! 10^25 maximum
    
contains

    function apply_scale_transform(value, scale_type, threshold) result(transformed)
        !! Apply forward scale transformation to a single value
        !! 
        !! @param value: Input value to transform
        !! @param scale_type: Type of scale ('linear', 'log', 'symlog')
        !! @param threshold: Threshold for symlog scale (ignored for others)
        !! @return transformed: Transformed value
        
        real(wp), intent(in) :: value
        character(len=*), intent(in) :: scale_type
        real(wp), intent(in) :: threshold
        real(wp) :: transformed
        
        select case (trim(scale_type))
        case ('linear')
            transformed = value
        case ('log')
            transformed = apply_log_transform(value)
        case ('symlog')
            transformed = apply_symlog_transform(value, threshold)
        case default
            transformed = value
        end select
    end function apply_scale_transform

    function apply_inverse_scale_transform(value, scale_type, threshold) result(original)
        !! Apply inverse scale transformation to recover original value
        !! 
        !! @param value: Transformed value to invert
        !! @param scale_type: Type of scale ('linear', 'log', 'symlog')
        !! @param threshold: Threshold for symlog scale (ignored for others)
        !! @return original: Original value before transformation
        
        real(wp), intent(in) :: value
        character(len=*), intent(in) :: scale_type
        real(wp), intent(in) :: threshold
        real(wp) :: original
        
        select case (trim(scale_type))
        case ('linear')
            original = value
        case ('log')
            original = apply_inverse_log_transform(value)
        case ('symlog')
            original = apply_inverse_symlog_transform(value, threshold)
        case default
            original = value
        end select
    end function apply_inverse_scale_transform

    function transform_x_coordinate(x, x_min, x_max, width) result(x_screen)
        !! Transform data x-coordinate to screen coordinate
        !! 
        !! @param x: Data x-coordinate
        !! @param x_min: Minimum x value in data range
        !! @param x_max: Maximum x value in data range
        !! @param width: Screen width in pixels
        !! @return x_screen: Screen x-coordinate
        
        real(wp), intent(in) :: x, x_min, x_max
        integer, intent(in) :: width
        real(wp) :: x_screen
        
        if (x_max > x_min) then
            x_screen = (x - x_min) / (x_max - x_min) * real(width, wp)
        else
            x_screen = 0.0_wp
        end if
    end function transform_x_coordinate

    function transform_y_coordinate(y, y_min, y_max, height, invert) result(y_screen)
        !! Transform data y-coordinate to screen coordinate
        !! 
        !! @param y: Data y-coordinate
        !! @param y_min: Minimum y value in data range
        !! @param y_max: Maximum y value in data range
        !! @param height: Screen height in pixels
        !! @param invert: Whether to invert y-axis (optional, default false)
        !! @return y_screen: Screen y-coordinate
        
        real(wp), intent(in) :: y, y_min, y_max
        integer, intent(in) :: height
        logical, intent(in), optional :: invert
        real(wp) :: y_screen
        logical :: do_invert
        
        do_invert = .false.
        if (present(invert)) do_invert = invert
        
        if (y_max > y_min) then
            if (do_invert) then
                y_screen = real(height, wp) - (y - y_min) / (y_max - y_min) * real(height, wp)
            else
                y_screen = (y - y_min) / (y_max - y_min) * real(height, wp)
            end if
        else
            y_screen = 0.0_wp
        end if
    end function transform_y_coordinate

    function apply_log_transform(value) result(transformed)
        !! Apply logarithmic transformation with extreme value protection
        !! 
        !! Clamps extreme values to prevent precision loss and overflow.
        !! This ensures scientifically meaningful plots while handling edge cases.
        real(wp), intent(in) :: value
        real(wp) :: transformed
        real(wp) :: log_val
        
        if (value > 0.0_wp) then
            log_val = log10(value)
            ! Clamp to safe logarithmic range
            transformed = max(MIN_LOG_VALUE, min(MAX_LOG_VALUE, log_val))
        else
            ! Handle non-positive values gracefully
            transformed = MIN_LOG_VALUE
        end if
    end function apply_log_transform

    function apply_inverse_log_transform(value) result(original)
        !! Apply inverse logarithmic transformation
        real(wp), intent(in) :: value
        real(wp) :: original
        
        original = 10.0_wp**value
    end function apply_inverse_log_transform

    function apply_symlog_transform(value, threshold) result(transformed)
        !! Apply symmetric logarithmic transformation
        !! Linear within [-threshold, threshold], logarithmic outside
        real(wp), intent(in) :: value, threshold
        real(wp) :: transformed
        
        if (abs(value) <= threshold) then
            ! Linear region
            transformed = value
        else if (value > threshold) then
            ! Positive logarithmic region
            transformed = threshold + log10(value / threshold)
        else
            ! Negative logarithmic region
            transformed = -threshold - log10(-value / threshold)
        end if
    end function apply_symlog_transform

    function apply_inverse_symlog_transform(value, threshold) result(original)
        !! Apply inverse symmetric logarithmic transformation
        real(wp), intent(in) :: value, threshold
        real(wp) :: original
        
        if (abs(value) <= threshold) then
            ! Linear region
            original = value
        else if (value > threshold) then
            ! Positive logarithmic region
            original = threshold * (10.0_wp**(value - threshold))
        else
            ! Negative logarithmic region
            original = -threshold * (10.0_wp**(-value - threshold))
        end if
    end function apply_inverse_symlog_transform

    subroutine clamp_extreme_log_range(data_min, data_max, clamped_min, clamped_max)
        !! Clamp extreme logarithmic ranges to ensure usable plots
        !! 
        !! This function prevents precision loss and overflow when dealing with 
        !! extreme ranges like huge() to tiny() values. It preserves scientific
        !! correctness while ensuring practical visualization.
        !! 
        !! @param data_min: Original minimum data value 
        !! @param data_max: Original maximum data value
        !! @param clamped_min: Output clamped minimum value
        !! @param clamped_max: Output clamped maximum value
        real(wp), intent(in) :: data_min, data_max
        real(wp), intent(out) :: clamped_min, clamped_max
        
        real(wp) :: log_min, log_max, log_range, log_center
        
        ! Handle non-positive values
        if (data_min <= 0.0_wp .and. data_max <= 0.0_wp) then
            clamped_min = 10.0_wp**MIN_LOG_VALUE
            clamped_max = 10.0_wp**(MIN_LOG_VALUE + 1.0_wp)
            return
        end if
        
        ! Ensure positive values for log transformation
        clamped_min = max(data_min, 10.0_wp**MIN_LOG_VALUE)
        clamped_max = max(data_max, clamped_min * 2.0_wp)
        
        ! Calculate log range
        log_min = log10(clamped_min)
        log_max = log10(clamped_max)
        log_range = log_max - log_min
        
        ! If range exceeds maximum, clamp symmetrically around geometric mean
        if (log_range > MAX_LOG_RANGE) then
            log_center = (log_min + log_max) * 0.5_wp
            log_min = log_center - MAX_LOG_RANGE * 0.5_wp
            log_max = log_center + MAX_LOG_RANGE * 0.5_wp
            
            ! Clamp to absolute bounds
            log_min = max(MIN_LOG_VALUE, log_min)
            log_max = min(MAX_LOG_VALUE, log_max)
            
            ! Convert back to linear space
            clamped_min = 10.0_wp**log_min
            clamped_max = 10.0_wp**log_max
        end if
    end subroutine clamp_extreme_log_range

end module fortplot_scales