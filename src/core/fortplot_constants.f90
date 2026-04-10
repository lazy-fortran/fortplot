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
    !!
    !! Balanced to: ensure tick label clearance (35px min)
    !! while keeping text within canvas
    integer, parameter, public :: XLABEL_VERTICAL_OFFSET = 35

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

    ! Tick label padding (pixels) for raster backends.
    integer, parameter, public :: X_TICK_LABEL_PAD = 20
    integer, parameter, public :: X_TICK_LABEL_TOP_PAD = 20
    integer, parameter, public :: Y_TICK_LABEL_RIGHT_PAD = 10
    integer, parameter, public :: Y_TICK_LABEL_LEFT_PAD = 10

    ! Extra gap between y tick labels and rotated y label (pixels).
    integer, parameter, public :: YLABEL_EXTRA_GAP = 25

    ! ========================================================================
    ! RASTER RENDERING CONSTANTS
    ! ========================================================================

    integer, parameter, public :: FALLBACK_LABEL_HEIGHT_PX = 12
    integer, parameter, public :: MIN_TICK_LABEL_GAP_PX = 8
    integer, parameter, public :: MIN_LABEL_MARGIN_PX = 15
    integer, parameter, public :: CANVAS_EDGE_PADDING_PX = 5
    real(wp), parameter, public :: FALLBACK_GRID_GRAY = 0.69_wp
    real(wp), parameter, public :: APPROX_EQUAL_TOLERANCE = 1.0e-6_wp

    ! Vega-Lite stroke-dash thresholds for line style detection
    real(wp), parameter, public :: DASH_LONG = 6.0_wp
    real(wp), parameter, public :: DASH_GAP = 3.0_wp
    real(wp), parameter, public :: DASH_SHORT = 2.0_wp

    ! ========================================================================
    ! DPI CONSTANTS
    ! ========================================================================

    real(wp), parameter, public :: REFERENCE_DPI = 100.0_wp

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

    ! ========================================================================
    ! TAB10 COLOR PALETTE (single source of truth)
    ! ========================================================================

    integer, parameter, public :: TAB10_COUNT = 10

    character(len=7), parameter, public :: TAB10_HEX(10) = [ &
        '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', &
        '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf' ]

    real(wp), parameter, public :: TAB10_RGB(3, 10) = reshape([ &
        0.1216_wp, 0.4667_wp, 0.7059_wp, &
        1.0_wp,    0.498_wp,  0.0549_wp, &
        0.1725_wp, 0.6275_wp, 0.1725_wp, &
        0.8392_wp, 0.1529_wp, 0.1569_wp, &
        0.5804_wp, 0.4039_wp, 0.7412_wp, &
        0.549_wp,  0.3373_wp, 0.2941_wp, &
        0.8902_wp, 0.4667_wp, 0.7608_wp, &
        0.498_wp,  0.498_wp,  0.498_wp,  &
        0.7373_wp, 0.7412_wp, 0.1333_wp, &
        0.0902_wp, 0.7451_wp, 0.8118_wp], [3, 10])

end module fortplot_constants
