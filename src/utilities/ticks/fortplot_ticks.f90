module fortplot_ticks
    !! Tick generation and formatting for all scale types
    !! 
    !! This module provides a unified interface to tick functionality
    !! by re-exporting from the split tick modules:
    !! - fortplot_tick_calculation: Linear tick algorithms
    !! - fortplot_tick_formatting: Value formatting functions
    !! - fortplot_tick_scales: Log and symlog scale functions
    
    use fortplot_tick_calculation, only: calculate_tick_labels, calculate_nice_axis_limits, &
        find_nice_tick_locations, calculate_minor_tick_positions, &
        calculate_log_minor_tick_positions
    use fortplot_tick_formatting, only: format_tick_value, format_tick_value_smart, &
        format_log_tick_value
    use fortplot_tick_scales, only: calculate_tick_labels_log, calculate_tick_labels_symlog, &
        generate_scale_aware_tick_labels
    implicit none

    private
    public :: calculate_tick_labels, calculate_tick_labels_log, calculate_tick_labels_symlog
    public :: format_tick_value, calculate_nice_axis_limits
    public :: generate_scale_aware_tick_labels, format_tick_value_smart, format_log_tick_value
    public :: find_nice_tick_locations
    public :: calculate_minor_tick_positions, calculate_log_minor_tick_positions

end module fortplot_ticks