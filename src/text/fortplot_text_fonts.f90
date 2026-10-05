module fortplot_text_fonts
    use fortplot_truetype
    use fortplot_logging, only: log_error
    use, intrinsic :: iso_fortran_env, only: wp => real64, int8
    implicit none
    
    private
    public :: init_text_system, cleanup_text_system, get_font_metrics
    public :: get_font_ascent_ratio, find_font_by_name, find_any_available_font
    public :: get_global_font, get_font_scale, is_font_initialized, get_font_scale_for_size
    public :: set_preferred_font
    public :: glyph_bitmap, glyph_hmetrics, glyph_bitmap_box

    ! Module state - shared with text rendering
    type(truetype_font_t) :: global_font
    logical :: font_initialized = .false.
    real(wp) :: font_scale = 0.0_wp
    character(len=40) :: preferred_font_name = ''

    ! Glyph fallback: fonts searched, in order, for codepoints the primary font
    ! lacks (arrows, set and relation symbols, ...). Loaded lazily, once.
    integer, parameter :: N_FALLBACK = 9
    character(len=*), parameter :: FALLBACK_PATHS(N_FALLBACK) = [character(len=64) :: &
        '/System/Library/Fonts/Supplemental/Arial Unicode.ttf', &
        '/Library/Fonts/Arial Unicode.ttf', &
        '/System/Library/Fonts/LucidaGrande.ttc', &
        '/System/Library/Fonts/Apple Symbols.ttf', &
        '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf', &
        '/usr/share/fonts/TTF/DejaVuSans.ttf', &
        '/usr/share/fonts/truetype/freefont/FreeSans.ttf', &
        'C:\Windows\Fonts\seguisym.ttf', &
        'C:\Windows\Fonts\arialuni.ttf']
    type(truetype_font_t) :: fallback_fonts(N_FALLBACK)
    ! 0 = not tried yet, 1 = loaded, -1 = unavailable
    integer :: fallback_state(N_FALLBACK) = 0
    
    
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
        !! Discover and initialize font from system locations.
        !! Priority: preferred_font_name (set by config) -> local override
        !! in cwd -> Helvetica -> Liberation Sans -> Arial -> DejaVu Sans.
        logical :: success
        character(len=256) :: font_path

        success = .false.

        if (len_trim(preferred_font_name) > 0) then
            if (find_font_by_name(trim(preferred_font_name), &
                                  font_path)) then
                success = try_init_font(font_path)
                if (success) return
            end if
        end if

        if (check_local_override(font_path)) then
            success = try_init_font(font_path)
            if (success) return
        end if

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

    subroutine set_preferred_font(font_name)
        !! Set preferred font and reinitialize if needed.
        !! Call before rendering to switch fonts for a style mode.
        character(len=*), intent(in) :: font_name

        logical :: ok

        if (trim(font_name) == trim(preferred_font_name)) return
        preferred_font_name = font_name

        ! Force reinit with new preference
        font_initialized = .false.
        ok = init_text_system()
    end subroutine set_preferred_font

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

    function check_local_override(font_path) result(found)
        !! Check the current working directory for custom fonts
        character(len=256), intent(out) :: font_path
        logical :: found
        character(len=256) :: candidates(4)
        integer :: i

        found = .false.

        ! Typical names a user might drop into their project folder
        candidates(1) = "./font.ttf"
        candidates(2) = "./custom.ttf"
        candidates(3) = "./Roboto-Regular.ttf"
        candidates(4) = "./arial.ttf"

        do i = 1, 4
            if (file_exists(candidates(i))) then
                font_path = candidates(i)
                found = .true.
                return
            end if
        end do
    end function check_local_override

    subroutine check_helvetica_paths(font_path, found)
        character(len=256), intent(out) :: font_path
        logical, intent(out) :: found
        character(len=256) :: candidates(5)
        integer :: i
        
        found = .false.
        
        candidates(1) = "/System/Library/Fonts/Helvetica.ttc"
        candidates(2) = "/System/Library/Fonts/HelveticaNeue.ttc"
        candidates(3) = "/Library/Fonts/Helvetica.ttf"
        candidates(4) = "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
        ! OpenSUSE: All fonts are in /usr/share/fonts/truetype/ (no subfolders)
        candidates(5) = "/usr/share/fonts/truetype/Helvetica.ttf"
        
        do i = 1, 5
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
        character(len=256) :: candidates(7)
        integer :: i
        
        found = .false.
        
        candidates(1) = "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
        candidates(2) = "/usr/share/fonts/Liberation/LiberationSans-Regular.ttf"
        candidates(3) = "/System/Library/Fonts/Supplemental/LiberationSans-Regular.ttf"
        candidates(4) = "C:\Windows\Fonts\LiberationSans-Regular.ttf"
        candidates(5) = "/opt/local/share/fonts/liberation-fonts/LiberationSans-Regular.ttf"
        candidates(6) = "/usr/local/share/fonts/liberation/LiberationSans-Regular.ttf"
        candidates(7) = "/usr/share/fonts/truetype/LiberationSans-Regular.ttf"
        
        do i = 1, 7
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
        character(len=256) :: candidates(10)
        integer :: i

        found = .false.

        candidates(1) = "C:\Windows\Fonts\arial.ttf"
        candidates(2) = "C:\Windows\Fonts\Arial.ttf"
        candidates(3) = "/System/Library/Fonts/Supplemental/Arial.ttf"
        candidates(4) = "/System/Library/Fonts/Arial.ttf"
        candidates(5) = "/Library/Fonts/Arial.ttf"
        candidates(6) = "/usr/share/fonts/truetype/arial/Arial.ttf"
        candidates(7) = "/usr/share/fonts/Arial.ttf"
        candidates(8) = "/opt/local/share/fonts/Arial.ttf"
        candidates(9) = "/usr/local/share/fonts/Arial.ttf"
        candidates(10) = "/usr/share/fonts/truetype/Arial.ttf"

        do i = 1, size(candidates)
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
        character(len=256) :: candidates(9)
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
        candidates(9) = "/usr/share/fonts/truetype/DejaVuSans.ttf"
        
        do i = 1, 9
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
        
        success = global_font%init(font_path)
        if (success) then
            font_scale = global_font%scale_for_pixel_height(16.0_wp)
            font_initialized = .true.
        end if
        
    end function try_init_font

    subroutine cleanup_text_system()
        !! Clean up text system resources
        integer :: k

        if (font_initialized) call global_font%cleanup()
        font_initialized = .false.
        do k = 1, N_FALLBACK
            if (fallback_state(k) == 1) call fallback_fonts(k)%cleanup()
        end do
        fallback_state = 0
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
        
        call global_font%get_vmetrics(ascent, descent, line_gap)
        
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
        
        call global_font%get_vmetrics(ascent, descent, line_gap)
        
        if ((ascent - descent) > 0) then
            ratio = real(ascent, wp) / real(ascent - descent, wp)
        else
            ratio = 0.8_wp
        end if
        
    end function get_font_ascent_ratio

    ! Accessor functions for shared state
    function get_global_font() result(font)
        type(truetype_font_t) :: font
        font = global_font
    end function get_global_font

    function get_font_scale() result(scale)
        real(wp) :: scale
        scale = font_scale
    end function get_font_scale
    
    function get_font_scale_for_size(pixel_height) result(scale)
        !! Get font scale for a specific pixel height
        real(wp), intent(in) :: pixel_height
        real(wp) :: scale
        
        if (font_initialized) then
            scale = global_font%scale_for_pixel_height(pixel_height)
        else
            ! Fallback: assume the default scale and adjust proportionally
            scale = font_scale * (pixel_height / 16.0_wp)
        end if
    end function get_font_scale_for_size

    function is_font_initialized() result(initialized)
        logical :: initialized
        initialized = font_initialized
    end function is_font_initialized

    subroutine resolve_glyph_font(codepoint, idx, ratio)
        !! Index of the font that draws `codepoint`: 0 for the primary font,
        !! k for fallback k. Codepoints no font knows stay with the primary
        !! (its .notdef box). `ratio` converts primary scales to that font.
        integer, intent(in) :: codepoint
        integer, intent(out) :: idx
        real(wp), intent(out) :: ratio
        integer :: k

        idx = 0
        ratio = 1.0_wp
        if (codepoint < 128) return
        if (global_font%find_glyph_index(codepoint) /= 0) return
        do k = 1, N_FALLBACK
            if (fallback_state(k) == 0) call load_fallback(k)
            if (fallback_state(k) /= 1) cycle
            if (fallback_fonts(k)%find_glyph_index(codepoint) /= 0) then
                idx = k
                ! Same em size as the primary font, which may have changed;
                ! hhea heights differ widely between fonts of wide coverage.
                ratio = fallback_fonts(k)%scale_for_em_to_pixels(100.0_wp)/ &
                        global_font%scale_for_em_to_pixels(100.0_wp)
                return
            end if
        end do
    end subroutine resolve_glyph_font

    subroutine load_fallback(k)
        integer, intent(in) :: k

        fallback_state(k) = -1
        if (.not. file_exists(trim(FALLBACK_PATHS(k)))) return
        if (.not. fallback_fonts(k)%init(trim(FALLBACK_PATHS(k)))) return
        if (fallback_fonts(k)%scale_for_em_to_pixels(100.0_wp) <= 0.0_wp) return
        fallback_state(k) = 1
    end subroutine load_fallback

    subroutine glyph_bitmap(scale, codepoint, bitmap, width, height, xoff, yoff)
        !! Rasterise `codepoint` at the primary-font `scale`, drawing it from a
        !! fallback font when the primary font has no such glyph.
        real(wp), intent(in) :: scale
        integer, intent(in) :: codepoint
        integer(int8), allocatable, intent(out) :: bitmap(:)
        integer, intent(out) :: width, height, xoff, yoff
        integer :: idx
        real(wp) :: ratio

        call resolve_glyph_font(codepoint, idx, ratio)
        if (idx == 0) then
            call global_font%get_codepoint_bitmap(scale, scale, codepoint, bitmap, &
                                                  width, height, xoff, yoff)
        else
            call fallback_fonts(idx)%get_codepoint_bitmap(scale*ratio, scale*ratio, &
                                                          codepoint, bitmap, width, &
                                                          height, xoff, yoff)
        end if
    end subroutine glyph_bitmap

    subroutine glyph_hmetrics(codepoint, advance_width, left_side_bearing)
        !! Horizontal metrics in primary-font units, from the font that draws
        !! the glyph.
        integer, intent(in) :: codepoint
        integer, intent(out) :: advance_width, left_side_bearing
        integer :: idx
        real(wp) :: ratio

        call resolve_glyph_font(codepoint, idx, ratio)
        if (idx == 0) then
            call global_font%get_hmetrics(codepoint, advance_width, left_side_bearing)
        else
            call fallback_fonts(idx)%get_hmetrics(codepoint, advance_width, &
                                                  left_side_bearing)
            advance_width = nint(real(advance_width, wp)*ratio)
            left_side_bearing = nint(real(left_side_bearing, wp)*ratio)
        end if
    end subroutine glyph_hmetrics

    subroutine glyph_bitmap_box(codepoint, scale, ix0, iy0, ix1, iy1)
        !! Bitmap box at the primary-font `scale`, from the drawing font.
        integer, intent(in) :: codepoint
        real(wp), intent(in) :: scale
        integer, intent(out) :: ix0, iy0, ix1, iy1
        integer :: idx
        real(wp) :: ratio

        call resolve_glyph_font(codepoint, idx, ratio)
        if (idx == 0) then
            call global_font%get_bitmap_box(codepoint, scale, scale, ix0, iy0, ix1, iy1)
        else
            call fallback_fonts(idx)%get_bitmap_box(codepoint, scale*ratio, &
                                                    scale*ratio, ix0, iy0, ix1, iy1)
        end if
    end subroutine glyph_bitmap_box

end module fortplot_text_fonts
