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

    !! Title vertical offset above plot area (pixels)
    !!
    !! This constant defines the vertical distance between the bottom of the
    !! plot area and the baseline of the title text.
    !!
    !! Used by raster backend for title positioning to ensure consistent
    !! spacing above the plot area while maintaining readability.
    integer, parameter, public :: TITLE_VERTICAL_OFFSET = 30

    ! ========================================================================
    ! DISPLAY AND RASTER CONSTANTS
    ! ========================================================================

    !! Standard figure width in pixels for matplotlib-compatible plots
    !!
    !! This constant represents the default plot area width in pixels for
    !! a typical 640x480 figure, used for data-to-pixel conversions.
    !! 
    !! Used by legend layout calculations to convert between data coordinates
    !! and pixel measurements for consistent text sizing.
    real(wp), parameter, public :: STANDARD_WIDTH_PIXELS = 496.0_wp

    !! Standard figure height in pixels for matplotlib-compatible plots
    !!
    !! This constant represents the default plot area height in pixels for
    !! a typical 640x480 figure, used for data-to-pixel conversions.
    !! 
    !! Used by legend layout calculations to convert between data coordinates
    !! and pixel measurements for consistent text sizing.
    real(wp), parameter, public :: STANDARD_HEIGHT_PIXELS = 369.6_wp

    !! Text width ratio for fallback text sizing calculations
    !!
    !! This constant provides a fallback ratio for estimating text width
    !! when the text measurement system is unavailable. Represents approximate
    !! character width as a fraction of data width.
    !! 
    !! Used by legend layout when text system measurements are not available.
    real(wp), parameter, public :: TEXT_WIDTH_RATIO = 0.012_wp

    !! Maximum safe pixel dimension for raster backends
    !!
    !! This constant defines the upper limit for width and height dimensions
    !! in raster-based backends to prevent memory issues and crashes.
    !! Matches typical matplotlib figure size limits.
    !! 
    !! Used by backend validation to ensure reasonable memory usage.
    integer, parameter, public :: MAX_SAFE_PIXELS = 10000

    ! ========================================================================
    ! TIME CONVERSION CONSTANTS
    ! ========================================================================

    !! Milliseconds per second conversion factor
    !!
    !! This constant provides the conversion factor from milliseconds to seconds
    !! for animation timing and delay calculations.
    !! 
    !! Used by animation pipeline for converting delay values between units.
    real(wp), parameter, public :: MILLISECONDS_PER_SECOND = 1000.0_wp

    ! ========================================================================
    ! SCIENTIFIC NOTATION AND FORMATTING CONSTANTS
    ! ========================================================================

    !! Threshold for switching to scientific notation in axis labels
    !!
    !! This constant defines the absolute value threshold above which
    !! numeric values are displayed in scientific notation for better readability.
    !! 
    !! Used by tick formatting and axis label generation.
    real(wp), parameter, public :: SCIENTIFIC_THRESHOLD_HIGH = 1000.0_wp

    !! Very long line pattern length for solid lines
    !!
    !! This constant defines a very long pattern length used to represent
    !! solid lines in line style calculations. Large value ensures the
    !! pattern doesn't repeat within typical plot ranges.
    !! 
    !! Used by line style pattern generation for solid line rendering.
    real(wp), parameter, public :: SOLID_LINE_PATTERN_LENGTH = 1000.0_wp

end module fortplot_constants