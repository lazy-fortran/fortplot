module fortplot_2d_plots
    !! 2D plot operations module
    !!
    !! This module handles all 2D plot operations including line plots
    !! and their associated data initialization and property setting.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_LINE
    use fortplot_colors, only: parse_color, color_t
    use fortplot_logging, only: log_warning, log_error
    use fortplot_format_parser, only: parse_format_string
    use fortplot_coordinate_validation, only: validate_coordinate_arrays, &
                                              coordinate_validation_result_t

    implicit none

    private
    public :: add_plot_impl
    public :: add_line_plot_data
    public :: init_line_plot_data
    public :: set_line_plot_properties

    interface add_plot
        module procedure add_plot_impl
    end interface
    public :: add_plot

contains

    subroutine add_plot_impl(self, x, y, label, linestyle, color_rgb, &
                             color_str, marker, markercolor)
        !! Add 2D line plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: linestyle
        real(wp), intent(in), optional :: color_rgb(3)
        character(len=*), intent(in), optional :: color_str
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markercolor(3)

        call add_line_plot_data(self, x, y, label, linestyle, color_rgb, &
                                color_str, marker, markercolor)
    end subroutine add_plot_impl

    subroutine add_line_plot_data(self, x, y, label, linestyle, color_rgb, &
                                  color_str, marker, markercolor)
        !! Add line plot data with comprehensive validation
        !! Enhanced Issue #854: Added comprehensive parameter validation
        use fortplot_parameter_validation, only: validate_numeric_parameters, &
                                                 validate_color_values, &
                                                 validate_array_bounds, &
                                                 parameter_validation_result_t
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle, color_str, marker
        real(wp), intent(in), optional :: color_rgb(3)
        real(wp), intent(in), optional :: markercolor(3)

        integer :: plot_idx
        type(coordinate_validation_result_t) :: validation
        type(parameter_validation_result_t) :: param_validation

        ! Validate coordinate arrays (Issue #436 fix)
        validation = validate_coordinate_arrays(x, y, "add_line_plot_data")
        if (.not. validation%is_valid) then
            call log_error(trim(validation%message))
            return  ! Skip adding invalid data
        end if

        ! Issue #854: Validate numeric parameters for NaN/infinity
        param_validation = validate_numeric_parameters(x, "x_coordinates", &
                                                       "add_line_plot_data")
        if (.not. param_validation%is_valid) then
            call log_error("X coordinates: "//trim(param_validation%message))
            return
        end if

        param_validation = validate_numeric_parameters(y, "y_coordinates", &
                                                       "add_line_plot_data")
        if (.not. param_validation%is_valid) then
            call log_error("Y coordinates: "//trim(param_validation%message))
            return
        end if

        ! Issue #854: Validate array bounds
        param_validation = validate_array_bounds(size(x), min_size=1, &
                                                 context="add_line_plot_data")
        if (.not. param_validation%is_valid) then
            call log_error("Array bounds: "//trim(param_validation%message))
            return
        end if

        ! Issue #854: Validate color values if provided
        if (present(color_rgb)) then
            param_validation = validate_color_values(color_rgb(1), color_rgb(2), &
                                                     color_rgb(3), &
                                                     context="add_line_plot_data")
            ! Do not return on color validation failure; warn and continue
            if (.not. param_validation%is_valid) then
                call log_warning("Color: "//trim(param_validation%message))
            end if
        end if
        if (present(markercolor)) then
            param_validation = validate_color_values(markercolor(1), markercolor(2), &
                                                     markercolor(3), &
                                                     context="add_line_plot_data")
            ! Do not return on marker color validation failure; warn and continue
            if (.not. param_validation%is_valid) then
                call log_warning("Markercolor: "//trim(param_validation%message))
            end if
        end if

        ! For single points, suggest using markers for better visibility
        if (validation%is_single_point .and. .not. present(marker)) then
            call log_warning("Single point detected - consider adding markers "// &
                             "for better visibility")
        end if

        ! Get current plot index
        self%plot_count = self%plot_count + 1
        self%state%plot_count = self%plot_count
        plot_idx = self%plot_count

        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate (self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if

        ! Initialize plot data
        call init_line_plot_data(self%plots(plot_idx), x, y)

        ! Set optional properties
        call set_line_plot_properties(self%plots(plot_idx), plot_idx, &
                                      label, linestyle, marker, color_rgb, color_str, &
                                      markercolor, self%state)
    end subroutine add_line_plot_data

    subroutine init_line_plot_data(plot, x, y)
        !! Initialize basic line plot data
        type(plot_data_t), intent(inout) :: plot
        real(wp), intent(in) :: x(:), y(:)

        plot%plot_type = PLOT_TYPE_LINE

        allocate (plot%x(size(x)))
        allocate (plot%y(size(y)))
        plot%x = x
        plot%y = y
    end subroutine init_line_plot_data

    subroutine set_line_plot_properties(plot, plot_idx, label, linestyle, marker, &
                                        color_rgb, color_str, markercolor, state)
        !! Set line plot properties (style, color, etc.)
        !! Extracted from add_line_plot_data for QADS compliance
        type(plot_data_t), intent(inout) :: plot
        integer, intent(in) :: plot_idx
        character(len=*), intent(in), optional :: label, linestyle, marker, color_str
        real(wp), intent(in), optional :: color_rgb(3)
        real(wp), intent(in), optional :: markercolor(3)
        type(figure_state_t), intent(in) :: state

        character(len=20) :: parsed_marker, parsed_linestyle
        real(wp) :: rgb(3)
        logical :: success
        integer :: color_idx

        ! Set label
        if (present(label) .and. len_trim(label) > 0) then
            plot%label = label
        end if

        ! Parse linestyle to extract marker and line style components
        if (present(linestyle)) then
            call parse_format_string(linestyle, parsed_marker, parsed_linestyle)

            if (len_trim(parsed_marker) > 0) then
                plot%marker = parsed_marker
            end if

            if (len_trim(parsed_linestyle) > 0) then
                plot%linestyle = parsed_linestyle
            end if
        end if

        ! Handle explicit marker override
        if (present(marker)) then
            plot%marker = marker
        end if

        ! Set color
        if (present(color_rgb)) then
            plot%color = color_rgb
        else if (present(color_str)) then
            call parse_color(color_str, rgb, success)
            if (success) then
                plot%color = rgb
            else
                ! Use default color cycling
                color_idx = mod(plot_idx - 1, size(state%colors, 2)) + 1
                plot%color = state%colors(:, color_idx)
            end if
        else
            ! Use default color cycling
            color_idx = mod(plot_idx - 1, size(state%colors, 2)) + 1
            plot%color = state%colors(:, color_idx)
        end if

        ! Initialize linestyle if not already done
        if (.not. allocated(plot%linestyle)) then
            plot%linestyle = '-'  ! default solid line
        end if

        if (present(markercolor)) then
            plot%marker_color = markercolor
            plot%marker_color_set = .true.
        else
            plot%marker_color_set = .false.
        end if
    end subroutine set_line_plot_properties

end module fortplot_2d_plots
