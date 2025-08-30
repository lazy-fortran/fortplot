module fortplot_annotations
    !! Text annotation system for fortplot (Issue #55)
    !! 
    !! This module provides a unified interface to annotation functionality
    !! by re-exporting from the split annotation modules:
    !! - fortplot_annotation_types: Type definitions and basic operations
    !! - fortplot_annotation_coordinates: Coordinate transformations
    !! - fortplot_annotation_layout: Text layout and typography
    
    use fortplot_annotation_types, only: text_annotation_t, annotation_color_t, &
        COORD_DATA, COORD_FIGURE, COORD_AXIS, create_text_annotation, &
        destroy_text_annotation, validate_annotation_coordinates, &
        validate_annotation_parameters, validate_annotation
    use fortplot_annotation_coordinates, only: transform_annotation_coordinates, &
        transform_annotation_coordinates_log, is_annotation_visible
    use fortplot_annotation_layout, only: calculate_aligned_position, &
        calculate_rotated_bounds, calculate_text_metrics_safe, load_font_system, &
        validate_text_parameters, calculate_text_metrics, calculate_text_anchor, &
        calculate_rotated_text_bounds, select_font_family, validate_typography_parameters
    implicit none
    
    private
    
    ! Re-export types and constants
    public :: text_annotation_t, annotation_color_t
    public :: COORD_DATA, COORD_FIGURE, COORD_AXIS
    
    ! Re-export functions
    public :: create_text_annotation, destroy_text_annotation
    public :: transform_annotation_coordinates, transform_annotation_coordinates_log
    public :: calculate_aligned_position, calculate_rotated_bounds
    public :: is_annotation_visible
    public :: validate_annotation_coordinates, validate_annotation_parameters
    public :: validate_annotation, calculate_text_metrics_safe, load_font_system
    public :: validate_text_parameters
    public :: calculate_text_metrics, calculate_text_anchor
    public :: calculate_rotated_text_bounds, select_font_family
    public :: validate_typography_parameters

end module fortplot_annotations