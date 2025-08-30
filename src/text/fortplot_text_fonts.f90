module fortplot_text_fonts
    use iso_c_binding
    use fortplot_stb_truetype
    use fortplot_logging, only: log_error
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: init_text_system, cleanup_text_system, get_font_metrics
    public :: get_font_ascent_ratio, find_font_by_name, find_any_available_font
    public :: get_global_font, get_font_scale, is_font_initialized
    
    ! Module state - shared with text rendering
    type(stb_fontinfo_t) :: global_font
    logical :: font_initialized = .false.
    real(wp) :: font_scale = 0.0_wp
    
    
contains

    function init_text_system() result(success)
        !! Initialize STB TrueType font system with robust font discovery
        logical :: success
        
        success = .false.
        
        if (font_initialized) then
            success = .true.
            return
        end if
        
        success = discover_and_init_font()
        
        if (.not. success) then
            call log_error("Could not initialize STB TrueType - no fonts found")
        end if
        
    end function init_text_system

    function discover_and_init_font() result(success)
        !! Discover and initialize font from system locations
        logical :: success
        character(len=256) :: font_path
        
        success = .false.
        
        ! Priority order: Helvetica -> Liberation Sans -> Arial -> DejaVu Sans
        if (find_font_by_name("Helvetica", font_path)) then
            success = try_init_font(font_path)
            if (success) return
        end if
        
        if (find_font_by_name("Liberation Sans", font_path)) then
            success = try_init_font(font_path)
            if (success) return
        end if
        
        if (find_font_by_name("Arial", font_path)) then
            success = try_init_font(font_path)
            if (success) return
        end if
        
        if (find_font_by_name("DejaVu Sans", font_path)) then
            success = try_init_font(font_path)
            if (success) return
        end if
        
    end function discover_and_init_font

    function find_font_by_name(font_name, font_path) result(found)
        !! Find font by name in typical system locations
        character(len=*), intent(in) :: font_name
        character(len=256), intent(out) :: font_path
        logical :: found
        
        found = .false.
        
        select case (trim(font_name))
        case ("Helvetica")
            call check_helvetica_paths(font_path, found)
        case ("Liberation Sans")
            call check_liberation_paths(font_path, found)
        case ("Arial")
            call check_arial_paths(font_path, found)
        case ("DejaVu Sans")
            call check_dejavu_paths(font_path, found)
        end select
        
    end function find_font_by_name

    function find_any_available_font(font_path) result(found)
        !! Find any available font using same priority order as system initialization
        character(len=256), intent(out) :: font_path
        logical :: found
        
        found = .false.
        
        ! Use same priority order as discover_and_init_font
        if (find_font_by_name("Helvetica", font_path)) then
            found = .true.
            return
        end if
        
        if (find_font_by_name("Liberation Sans", font_path)) then
            found = .true.
            return
        end if
        
        if (find_font_by_name("Arial", font_path)) then
            found = .true.
            return
        end if
        
        if (find_font_by_name("DejaVu Sans", font_path)) then
            found = .true.
            return
        end if
        
    end function find_any_available_font

    subroutine check_helvetica_paths(font_path, found)
        character(len=256), intent(out) :: font_path
        logical, intent(out) :: found
        character(len=256) :: candidates(4)
        integer :: i
        
        found = .false.
        
        candidates(1) = "/System/Library/Fonts/Helvetica.ttc"
        candidates(2) = "/System/Library/Fonts/HelveticaNeue.ttc"
        candidates(3) = "/Library/Fonts/Helvetica.ttf"
        candidates(4) = "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
        
        do i = 1, 4
            if (file_exists(candidates(i))) then
                font_path = candidates(i)
                found = .true.
                return
            end if
        end do
    end subroutine check_helvetica_paths

    subroutine check_liberation_paths(font_path, found)
        character(len=256), intent(out) :: font_path
        logical, intent(out) :: found
        character(len=256) :: candidates(6)
        integer :: i
        
        found = .false.
        
        candidates(1) = "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
        candidates(2) = "/usr/share/fonts/Liberation/LiberationSans-Regular.ttf"
        candidates(3) = "/System/Library/Fonts/Supplemental/LiberationSans-Regular.ttf"
        candidates(4) = "C:\Windows\Fonts\LiberationSans-Regular.ttf"
        candidates(5) = "/opt/local/share/fonts/liberation-fonts/LiberationSans-Regular.ttf"
        candidates(6) = "/usr/local/share/fonts/liberation/LiberationSans-Regular.ttf"
        
        do i = 1, 6
            if (file_exists(candidates(i))) then
                font_path = candidates(i)
                found = .true.
                return
            end if
        end do
    end subroutine check_liberation_paths

    subroutine check_arial_paths(font_path, found)
        character(len=256), intent(out) :: font_path
        logical, intent(out) :: found
        character(len=256) :: candidates(8)
        integer :: i
        
        found = .false.
        
        candidates(1) = "C:\Windows\Fonts\arial.ttf"
        candidates(2) = "C:\Windows\Fonts\Arial.ttf"
        candidates(3) = "/System/Library/Fonts/Arial.ttf"
        candidates(4) = "/Library/Fonts/Arial.ttf"
        candidates(5) = "/usr/share/fonts/truetype/arial/Arial.ttf"
        candidates(6) = "/usr/share/fonts/Arial.ttf"
        candidates(7) = "/opt/local/share/fonts/Arial.ttf"
        candidates(8) = "/usr/local/share/fonts/Arial.ttf"
        
        do i = 1, 8
            if (file_exists(candidates(i))) then
                font_path = candidates(i)
                found = .true.
                return
            end if
        end do
    end subroutine check_arial_paths

    subroutine check_dejavu_paths(font_path, found)
        character(len=256), intent(out) :: font_path
        logical, intent(out) :: found
        character(len=256) :: candidates(8)
        integer :: i
        
        found = .false.
        
        candidates(1) = "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
        candidates(2) = "/usr/share/fonts/TTF/DejaVuSans.ttf"
        candidates(3) = "/System/Library/Fonts/Supplemental/DejaVuSans.ttf"
        candidates(4) = "C:\Windows\Fonts\DejaVuSans.ttf"
        candidates(5) = "/opt/local/share/fonts/dejavu-fonts/DejaVuSans.ttf"
        candidates(6) = "/usr/local/share/fonts/dejavu/DejaVuSans.ttf"
        candidates(7) = "/Library/Fonts/DejaVuSans.ttf"
        candidates(8) = "/usr/share/fonts/DejaVu/DejaVuSans.ttf"
        
        do i = 1, 8
            if (file_exists(candidates(i))) then
                font_path = candidates(i)
                found = .true.
                return
            end if
        end do
    end subroutine check_dejavu_paths

    function file_exists(file_path) result(exists)
        character(len=*), intent(in) :: file_path
        logical :: exists
        
        inquire(file=trim(file_path), exist=exists)
    end function file_exists

    function try_init_font(font_path) result(success)
        character(len=*), intent(in) :: font_path
        logical :: success
        
        success = stb_init_font(global_font, font_path)
        if (success) then
            font_scale = stb_scale_for_pixel_height(global_font, 16.0_wp)
            font_initialized = .true.
        end if
        
    end function try_init_font

    subroutine cleanup_text_system()
        !! Clean up text system resources
        font_initialized = .false.
        font_scale = 0.0_wp
        
    end subroutine cleanup_text_system

    subroutine get_font_metrics(ascent_pixels, descent_pixels, line_gap_pixels, success)
        !! Get font metrics in pixels for current font and scale
        real(wp), intent(out) :: ascent_pixels, descent_pixels, line_gap_pixels
        logical, intent(out) :: success
        integer :: ascent, descent, line_gap
        
        success = .false.
        ascent_pixels = 0.0_wp
        descent_pixels = 0.0_wp
        line_gap_pixels = 0.0_wp
        
        if (.not. font_initialized) then
            return
        end if
        
        call stb_get_font_vmetrics(global_font, ascent, descent, line_gap)
        
        ascent_pixels = real(ascent, wp) * font_scale
        descent_pixels = real(descent, wp) * font_scale
        line_gap_pixels = real(line_gap, wp) * font_scale
        
        success = .true.
    end subroutine get_font_metrics

    function get_font_ascent_ratio() result(ratio)
        !! Get font ascent ratio for baseline positioning
        real(wp) :: ratio
        integer :: ascent, descent, line_gap
        
        if (.not. font_initialized) then
            ratio = 0.8_wp
            return
        end if
        
        call stb_get_font_vmetrics(global_font, ascent, descent, line_gap)
        
        if ((ascent - descent) > 0) then
            ratio = real(ascent, wp) / real(ascent - descent, wp)
        else
            ratio = 0.8_wp
        end if
        
    end function get_font_ascent_ratio

    ! Accessor functions for shared state
    function get_global_font() result(font)
        type(stb_fontinfo_t) :: font
        font = global_font
    end function get_global_font

    function get_font_scale() result(scale)
        real(wp) :: scale
        scale = font_scale
    end function get_font_scale

    function is_font_initialized() result(initialized)
        logical :: initialized
        initialized = font_initialized
    end function is_font_initialized

end module fortplot_text_fonts