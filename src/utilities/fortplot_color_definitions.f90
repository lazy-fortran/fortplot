module fortplot_color_definitions
    !! Color type definitions and constants for matplotlib-compatible color support
    !! 
    !! Provides:
    !! - Unified color type with RGBA support
    !! - Named color constants (CSS4/matplotlib standard)
    !! - Single letter color mappings
    !! - Utility functions for color validation
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: color_t
    public :: NUM_NAMED_COLORS, named_color_names, named_color_values
    public :: single_letters, letter_to_named
    public :: clamp_to_unit, to_lowercase, to_lowercase_char
    
    ! Color type for unified representation
    type :: color_t
        real(wp) :: r = 0.0_wp
        real(wp) :: g = 0.0_wp  
        real(wp) :: b = 0.0_wp
        real(wp) :: a = 1.0_wp  ! Alpha channel
        logical :: valid = .false.
    end type color_t
    
    ! Named color constants - CSS4/matplotlib standard colors
    integer, parameter :: NUM_NAMED_COLORS = 20
    character(len=12), parameter :: named_color_names(NUM_NAMED_COLORS) = [ &
        'red         ', 'green       ', 'blue        ', 'cyan        ', &
        'magenta     ', 'yellow      ', 'black       ', 'white       ', &
        'gray        ', 'orange      ', 'purple      ', 'brown       ', &
        'pink        ', 'olive       ', 'navy        ', 'lime        ', &
        'teal        ', 'silver      ', 'maroon      ', 'indigo      ' &
    ]
    
    real(wp), parameter :: named_color_values(3, NUM_NAMED_COLORS) = reshape([ &
        1.0_wp, 0.0_wp, 0.0_wp,  & ! red
        0.0_wp, 0.5_wp, 0.0_wp,  & ! green (CSS4 green, not lime)
        0.0_wp, 0.0_wp, 1.0_wp,  & ! blue
        0.0_wp, 1.0_wp, 1.0_wp,  & ! cyan
        1.0_wp, 0.0_wp, 1.0_wp,  & ! magenta
        1.0_wp, 1.0_wp, 0.0_wp,  & ! yellow
        0.0_wp, 0.0_wp, 0.0_wp,  & ! black
        1.0_wp, 1.0_wp, 1.0_wp,  & ! white
        0.5_wp, 0.5_wp, 0.5_wp,  & ! gray
        1.0_wp, 0.647_wp, 0.0_wp, & ! orange
        0.5_wp, 0.0_wp, 0.5_wp,  & ! purple
        0.647_wp, 0.165_wp, 0.165_wp, & ! brown
        1.0_wp, 0.753_wp, 0.796_wp, & ! pink
        0.5_wp, 0.5_wp, 0.0_wp,  & ! olive
        0.0_wp, 0.0_wp, 0.5_wp,  & ! navy
        0.0_wp, 1.0_wp, 0.0_wp,  & ! lime
        0.0_wp, 0.5_wp, 0.5_wp,  & ! teal
        0.753_wp, 0.753_wp, 0.753_wp, & ! silver
        0.5_wp, 0.0_wp, 0.0_wp,  & ! maroon
        0.294_wp, 0.0_wp, 0.51_wp  & ! indigo
    ], [3, NUM_NAMED_COLORS])
    
    ! Single letter color mapping (matplotlib compatible)
    character(len=1), parameter :: single_letters(8) = ['r', 'g', 'b', 'c', 'm', 'y', 'k', 'w']
    integer, parameter :: letter_to_named(8) = [1, 2, 3, 4, 5, 6, 7, 8]  ! Map to named_color_values indices

contains

    function clamp_to_unit(value) result(clamped)
        !! Clamp value to [0,1] range
        real(wp), intent(in) :: value
        real(wp) :: clamped
        
        clamped = max(0.0_wp, min(1.0_wp, value))
    end function clamp_to_unit
    
    subroutine to_lowercase(str)
        !! Convert string to lowercase in-place
        character(len=:), allocatable, intent(inout) :: str
        integer :: i
        
        do i = 1, len(str)
            call to_lowercase_char(str(i:i))
        end do
    end subroutine to_lowercase
    
    subroutine to_lowercase_char(char)
        !! Convert single character to lowercase
        character(len=1), intent(inout) :: char
        
        if (char >= 'A' .and. char <= 'Z') then
            char = achar(iachar(char) + 32)
        end if
    end subroutine to_lowercase_char

end module fortplot_color_definitions