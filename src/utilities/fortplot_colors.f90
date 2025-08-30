module fortplot_colors
    !! Color parsing and management for matplotlib-compatible color syntax
    !! 
    !! This module provides a unified interface to color functionality
    !! by re-exporting from the split color modules:
    !! - fortplot_color_definitions: Color types and constants  
    !! - fortplot_color_parsing: Core parsing functionality
    !! - fortplot_color_conversions: Color space conversions and colormaps
    
    use fortplot_color_definitions, only: color_t
    use fortplot_color_parsing, only: parse_color, parse_color_rgba, is_valid_color, &
        validate_color_for_backend, clear_color_cache, parse_colors_bulk, get_cache_hit_rate
    use fortplot_color_conversions, only: rgb_to_hsv, rgb_to_lab, apply_colormap_to_array
    implicit none
    
    private
    public :: color_t, parse_color, parse_color_rgba, is_valid_color
    public :: validate_color_for_backend, clear_color_cache
    public :: parse_colors_bulk, get_cache_hit_rate
    public :: rgb_to_hsv, rgb_to_lab
    public :: apply_colormap_to_array

end module fortplot_colors