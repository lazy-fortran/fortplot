program test_stb_truetype
    !! Unit test for the pure Fortran TrueType integration.
    use fortplot_truetype
    use, intrinsic :: iso_fortran_env, only: wp => real64, int8
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, "=== TrueType Integration Tests ==="
    print *, ""

    if (.not. test_font_initialization()) all_tests_passed = .false.
    if (.not. test_font_metrics()) all_tests_passed = .false.
    if (.not. test_character_rendering()) all_tests_passed = .false.

    if (all_tests_passed) then
        print *, "All TrueType tests PASSED"
        stop 0
    else
        print *, "Some TrueType tests FAILED"
        stop 1
    end if

contains

    function test_font_initialization() result(passed)
        use fortplot_text, only: find_any_available_font
        logical :: passed
        type(truetype_font_t) :: test_font
        character(len=256) :: font_path

        passed = .false.

        print *, "Test 1: Font Initialization"
        print *, "---------------------------"

        if (.not. find_any_available_font(font_path)) then
            print *, "FAIL: Could not find any supported font"
            return
        end if

        if (test_font%init(trim(font_path))) then
            print *, "PASS: Successfully loaded font:", trim(font_path)
            passed = .true.
            call test_font%cleanup()
        else
            print *, "FAIL: Failed to load font:", trim(font_path)
        end if
    end function test_font_initialization

    function test_font_metrics() result(passed)
        use fortplot_text, only: find_any_available_font
        logical :: passed
        type(truetype_font_t) :: test_font
        integer :: ascent, descent, line_gap
        integer :: advance_width, left_side_bearing
        real(wp) :: scale
        character(len=256) :: font_path

        passed = .false.

        print *, ""
        print *, "Test 2: Font Metrics"
        print *, "--------------------"

        if (.not. find_any_available_font(font_path)) then
            print *, "FAIL: Cannot find any supported font for metrics test"
            return
        end if

        if (.not. test_font%init(trim(font_path))) then
            print *, "FAIL: Cannot load font for metrics test"
            return
        end if

        scale = test_font%scale_for_pixel_height(12.0_wp)
        if (scale > 0.0_wp) then
            print *, "PASS: Scale for 12px height:", scale
        else
            print *, "FAIL: Invalid scale factor:", scale
            call test_font%cleanup()
            return
        end if

        call test_font%get_vmetrics(ascent, descent, line_gap)
        print *, "PASS: Vertical metrics: ascent=", ascent, " descent=", descent, " line_gap=", line_gap

        if (ascent <= 0 .or. descent >= 0) then
            print *, "FAIL: Suspicious vertical metrics values"
            call test_font%cleanup()
            return
        end if

        call test_font%get_hmetrics(iachar('A'), advance_width, left_side_bearing)
        print *, "PASS: 'A' metrics: advance=", advance_width, " bearing=", left_side_bearing

        if (advance_width <= 0) then
            print *, "FAIL: Invalid advance width for 'A'"
            call test_font%cleanup()
            return
        end if

        passed = .true.
        call test_font%cleanup()
    end function test_font_metrics

    function test_character_rendering() result(passed)
        use fortplot_text, only: find_any_available_font
        logical :: passed
        type(truetype_font_t) :: test_font
        integer(int8), allocatable :: bitmap(:)
        integer :: width, height, xoff, yoff
        integer :: ix0, iy0, ix1, iy1
        integer :: glyph_index
        real(wp) :: scale
        character(len=256) :: font_path

        passed = .false.

        print *, ""
        print *, "Test 3: Character Rendering"
        print *, "---------------------------"

        if (.not. find_any_available_font(font_path)) then
            print *, "FAIL: Cannot find any supported font for rendering test"
            return
        end if

        if (.not. test_font%init(trim(font_path))) then
            print *, "FAIL: Cannot load font for rendering test"
            return
        end if

        glyph_index = test_font%find_glyph_index(iachar('A'))
        if (glyph_index == 0) then
            print *, "FAIL: Could not find glyph for 'A'"
            call test_font%cleanup()
            return
        end if
        print *, "PASS: Found glyph index for 'A':", glyph_index

        scale = test_font%scale_for_pixel_height(16.0_wp)

        call test_font%get_bitmap_box(iachar('A'), scale, scale, ix0, iy0, ix1, iy1)
        print *, "PASS: 'A' bitmap box: (", ix0, ",", iy0, ") to (", ix1, ",", iy1, ")"

        if (ix1 <= ix0 .or. iy1 <= iy0) then
            print *, "FAIL: Invalid bitmap bounding box"
            call test_font%cleanup()
            return
        end if

        call test_font%get_codepoint_bitmap(scale, scale, iachar('A'), bitmap, &
            width, height, xoff, yoff)

        if (.not. allocated(bitmap)) then
            print *, "FAIL: Failed to render 'A' bitmap"
            call test_font%cleanup()
            return
        end if

        print *, "PASS: Rendered 'A' bitmap: ", width, "x", height, " offset=(", xoff, ",", yoff, ")"

        if (width <= 0 .or. height <= 0) then
            print *, "FAIL: Invalid bitmap dimensions"
            call test_font%cleanup()
            return
        end if

        if (.not. test_bitmap_content(bitmap, width, height)) then
            print *, "FAIL: Bitmap content validation failed"
            call test_font%cleanup()
            return
        end if

        print *, "PASS: Bitmap content validation passed"
        passed = .true.
        call test_font%cleanup()
    end function test_character_rendering

    function test_bitmap_content(bitmap, width, height) result(valid)
        integer(int8), intent(in) :: bitmap(:)
        integer, intent(in) :: width, height
        logical :: valid
        integer :: i, non_zero_pixels, total_pixels

        valid = .false.
        total_pixels = width * height
        non_zero_pixels = 0

        do i = 1, total_pixels
            if (iand(int(bitmap(i)), 255) > 0) then
                non_zero_pixels = non_zero_pixels + 1
            end if
        end do

        print *, "   Bitmap analysis:", non_zero_pixels, "/", total_pixels, " pixels have ink"

        if (non_zero_pixels == 0) then
            print *, "   ERROR: No ink pixels found"
            return
        end if

        if (non_zero_pixels == total_pixels) then
            print *, "   ERROR: All pixels are ink"
            return
        end if

        if (non_zero_pixels < total_pixels / 100 .or. non_zero_pixels > total_pixels * 4 / 5) then
            print *, "   WARNING: Unusual ink ratio"
        end if

        valid = .true.
    end function test_bitmap_content

end program test_stb_truetype
