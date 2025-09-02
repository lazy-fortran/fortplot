! fortplot_parameter_validation.f90 - Numeric and array parameter validation
! 
! This module provides numeric parameter validation and array bounds checking
! for user input safety across all public APIs.
!
! Issue #854: Parameter validation warnings for user input safety
! Issue #884: File size compliance - focused on numeric/array validation
!
module fortplot_parameter_validation
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
    use fortplot_validation_context, only: validation_context_t, parameter_validation_result_t, &
                                          validation_warning, validation_error, &
                                          WARNING_MODE_ALL, WARNING_MODE_ERRORS, WARNING_MODE_SILENT
    ! Re-export core validation functions for backward compatibility
    use fortplot_validation_core, only: validate_plot_dimensions, validate_plot_dimensions_with_context, &
                                       validate_color_values, validate_file_path
    ! Re-export context system for backward compatibility  
    use fortplot_validation_context, only: validation_warning, validation_error, &
                                          validation_warning_with_context, validation_error_with_context, &
                                          default_validation_context, &
                                          validation_context_t, parameter_validation_result_t, &
                                          WARNING_MODE_ALL, WARNING_MODE_ERRORS, WARNING_MODE_SILENT
    implicit none
    private
    
    ! Public interface - numeric and array validation (primary responsibility)
    public :: validate_numeric_parameters
    public :: validate_array_bounds
    public :: is_nan_safe
    public :: is_finite_safe
    
    ! Re-export backward compatibility functions
    public :: validate_plot_dimensions
    public :: validate_plot_dimensions_with_context
    public :: validate_color_values
    public :: validate_file_path
    public :: validation_warning
    public :: validation_error
    public :: validation_warning_with_context
    public :: validation_error_with_context
    public :: default_validation_context
    public :: validation_context_t
    public :: parameter_validation_result_t
    public :: WARNING_MODE_ALL, WARNING_MODE_ERRORS, WARNING_MODE_SILENT
    
    ! Parameter validation limits for arrays and numeric data
    integer, parameter :: MAX_ARRAY_SIZE = 1000000  ! 1M elements safety limit
    
contains
    
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
    function is_nan_safe(value) result(is_nan)
        real(wp), intent(in) :: value
        logical :: is_nan
        
        is_nan = ieee_is_nan(value)
    end function is_nan_safe
    
    ! Safe finite check that works across compilers
    function is_finite_safe(value) result(is_finite)
        real(wp), intent(in) :: value
        logical :: is_finite
        
        is_finite = ieee_is_finite(value)
    end function is_finite_safe
    
end module fortplot_parameter_validation
