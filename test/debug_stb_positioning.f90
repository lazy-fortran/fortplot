program debug_stb_positioning
    !! Debug STB TrueType positioning to understand coordinate system
    use fortplot_stb_truetype
    use iso_c_binding
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(stb_fontinfo_t) :: font_info
    type(c_ptr) :: bitmap_ptr
    integer :: width, height, xoff, yoff
    integer :: ix0, iy0, ix1, iy1
    integer :: ascent, descent, line_gap
    integer :: advance_width, left_side_bearing
    real(wp) :: scale
    character(len=1) :: test_char = 'A'
    integer :: char_code
    
    ! Initialize font
    if (.not. stb_init_font(font_info, "/usr/share/fonts/TTF/DejaVuSans.ttf")) then
        print *, "ERROR: Could not load font"
        stop 1
    end if
    
    char_code = iachar(test_char)
    scale = stb_scale_for_pixel_height(font_info, 16.0_wp)
    
    print *, "=== STB TrueType Positioning Debug ==="
    print *, "Character: '", test_char, "' (code ", char_code, ")"
    print *, "Scale for 16px: ", scale
    print *, ""
    
    ! Get font metrics
    call stb_get_font_vmetrics(font_info, ascent, descent, line_gap)
    print *, "Font metrics (unscaled):"
    print *, "  Ascent: ", ascent
    print *, "  Descent: ", descent 
    print *, "  Line gap: ", line_gap
    print *, ""
    print *, "Font metrics (scaled):"
    print *, "  Ascent: ", int(ascent * scale)
    print *, "  Descent: ", int(descent * scale)
    print *, ""
    
    ! Get character metrics
    call stb_get_codepoint_hmetrics(font_info, char_code, advance_width, left_side_bearing)
    print *, "Character metrics (unscaled):"
    print *, "  Advance width: ", advance_width
    print *, "  Left side bearing: ", left_side_bearing
    print *, ""
    print *, "Character metrics (scaled):"
    print *, "  Advance width: ", int(advance_width * scale)
    print *, "  Left side bearing: ", int(left_side_bearing * scale)
    print *, ""
    
    ! Get bitmap bounding box
    call stb_get_codepoint_bitmap_box(font_info, char_code, scale, scale, ix0, iy0, ix1, iy1)
    print *, "Bitmap box:"
    print *, "  X: ", ix0, " to ", ix1, " (width: ", ix1-ix0, ")"
    print *, "  Y: ", iy0, " to ", iy1, " (height: ", iy1-iy0, ")"
    print *, ""
    
    ! Render bitmap
    bitmap_ptr = stb_get_codepoint_bitmap(font_info, scale, scale, char_code, &
                                         width, height, xoff, yoff)
    
    if (c_associated(bitmap_ptr)) then
        print *, "Rendered bitmap:"
        print *, "  Size: ", width, "x", height
        print *, "  Offset: (", xoff, ", ", yoff, ")"
        print *, ""
        print *, "COORDINATE SYSTEM ANALYSIS:"
        print *, "If baseline is at Y=0:"
        print *, "  Bitmap box Y range: ", iy0, " to ", iy1
        print *, "  Rendered offset Y: ", yoff
        print *, "  Should render from Y=", yoff, " to Y=", yoff + height
        print *, ""
        print *, "For a 20px high canvas with text at Y=16 (4px from bottom):"
        print *, "  STB bitmap would be at Y=", 16 + yoff, " to Y=", 16 + yoff + height
        print *, "  (negative yoff moves UP from baseline)"
        
        call stb_free_bitmap(bitmap_ptr)
    else
        print *, "ERROR: Could not render bitmap"
    end if
    
    call stb_cleanup_font(font_info)
    
end program debug_stb_positioning