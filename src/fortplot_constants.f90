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

end module fortplot_constants