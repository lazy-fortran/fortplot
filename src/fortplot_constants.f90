module fortplot_constants
    !! Numerical and physical constants for the fortplot library
    !!
    !! This module provides centralized definitions for commonly used constants
    !! throughout the fortplot codebase, ensuring consistency and maintainability.
    !!
    !! = Numerical Constants =
    !! - EPSILON_COMPARE: Standard epsilon for floating-point comparisons
    !! - EPSILON_GEOMETRY: Higher precision epsilon for geometric calculations
    !!
    !! = Usage Examples =
    !!   use fortplot_constants, only: EPSILON_COMPARE
    !!   if (abs(value1 - value2) < EPSILON_COMPARE) then
    !!       ! Values are considered equal
    !!   end if
    
    use iso_fortran_env, only: wp => real64
    implicit none
    private
    
    ! ========================================================================
    ! NUMERICAL COMPARISON CONSTANTS
    ! ========================================================================
    
    !! Standard epsilon for numerical comparisons in plotting operations
    !! 
    !! This constant is used for:
    !! - Range validation (checking if z_max - z_min is significant)
    !! - Coordinate comparisons in interpolation
    !! - General floating-point equality testing
    !! 
    !! Value chosen to be appropriate for typical plotting data precision
    !! while avoiding false positives from numerical noise.
    real(wp), parameter, public :: EPSILON_COMPARE = 1e-10_wp
    
    !! High-precision epsilon for geometric calculations
    !! 
    !! This constant is used for:
    !! - Contour region calculations requiring higher precision
    !! - Vector length calculations in drawing operations
    !! - Fine geometric operations where more stringent tolerances are needed
    !! 
    !! Smaller value provides better precision for critical geometric operations.
    real(wp), parameter, public :: EPSILON_GEOMETRY = 1e-12_wp

    ! ========================================================================
    ! LABEL POSITIONING CONSTANTS
    ! ========================================================================

    !! X-axis label vertical offset below plot area (pixels)
    !!
    !! This constant defines the vertical distance between the bottom of the
    !! plot area and the baseline of the x-axis label text.
    !! 
    !! Used by raster backend for xlabel positioning to ensure consistent
    !! spacing below tick labels while avoiding overlap.
    integer, parameter, public :: XLABEL_VERTICAL_OFFSET = 30

    !! Y-axis label horizontal offset from left edge (pixels)
    !!
    !! This constant defines the horizontal distance from the left edge of
    !! the figure to the left edge of the y-axis label text.
    !!
    !! Used by raster backend for ylabel positioning to ensure consistent
    !! left margin spacing and readability.
    integer, parameter, public :: YLABEL_HORIZONTAL_OFFSET = 10

    !! Tick mark length extending from axis line (pixels)
    !!
    !! This constant defines the length of tick marks extending from the
    !! axis lines into the plot area or margin.
    !!
    !! Used by raster backend for drawing consistent tick marks across
    !! all axes while maintaining visual hierarchy.
    integer, parameter, public :: TICK_MARK_LENGTH = 5

end module fortplot_constants