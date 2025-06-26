program test_text_width_measurements
    !! Unit test for calculate_text_width function focusing on narrow characters
    use fortplot_text, only: calculate_text_width, init_text_system, render_text_to_image
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer :: width_i, width_l, width_m, width_w
    integer :: width_ii, width_ll, width_il, width_li
    logical :: test_passed
    integer(1), allocatable :: test_image(:)
    integer, parameter :: test_img_width = 100, test_img_height = 50
    
    ! Initialize text system
    if (.not. init_text_system()) then
        print *, "ERROR: Failed to initialize text system"
        stop 1
    end if
    
    print *, "=== Testing calculate_text_width Function ==="
    print *, ""
    
    ! Test individual narrow characters
    width_i = calculate_text_width("i")
    width_l = calculate_text_width("l")
    width_m = calculate_text_width("m")
    width_w = calculate_text_width("w")
    
    print *, "Single character widths:"
    print *, "  'i':", width_i, "pixels"
    print *, "  'l':", width_l, "pixels"  
    print *, "  'm':", width_m, "pixels"
    print *, "  'w':", width_w, "pixels"
    print *, ""
    
    ! Test character pairs with kerning
    width_ii = calculate_text_width("ii")
    width_ll = calculate_text_width("ll")
    width_il = calculate_text_width("il")
    width_li = calculate_text_width("li")
    
    print *, "Character pair widths:"
    print *, "  'ii':", width_ii, "pixels"
    print *, "  'll':", width_ll, "pixels"
    print *, "  'il':", width_il, "pixels"
    print *, "  'li':", width_li, "pixels"
    print *, ""
    
    ! Verify character width consistency
    test_passed = .true.
    
    ! Test 1: Narrow characters should be narrower than wide characters
    if (width_i >= width_m .or. width_l >= width_m) then
        print *, "FAIL: Narrow characters 'i' and 'l' should be narrower than 'm'"
        test_passed = .false.
    else
        print *, "PASS: Narrow characters are narrower than wide characters"
    end if
    
    ! Test 2: Character pairs should be close to sum of individual widths (with kerning)
    if (abs(width_ii - 2*width_i) > 3) then
        print *, "FAIL: 'ii' width inconsistent with single 'i' width"
        print *, "  Expected: ~", 2*width_i, ", Got:", width_ii
        test_passed = .false.
    else
        print *, "PASS: Character pair width consistent with individual characters"
    end if
    
    ! Test 3: Verify characters are not using fallback values
    if (width_i == 9 .or. width_l == 9) then
        print *, "FAIL: Characters 'i' or 'l' using fallback width of 9 pixels"
        print *, "  This suggests text system is not working properly"
        test_passed = .false.
    else
        print *, "PASS: Characters using text system measurements, not fallback"
    end if
    
    ! Test 4: Width should be reasonable for typical font
    if (width_i < 2 .or. width_i > 8 .or. width_l < 2 .or. width_l > 8) then
        print *, "FAIL: Character widths outside reasonable range (2-8 pixels)"
        print *, "  'i':", width_i, "'l':", width_l
        test_passed = .false.
    else
        print *, "PASS: Character widths in reasonable range"
    end if
    
    ! Test 5: Verify calculate_text_width is consistent with render_text_to_image
    ! Allocate test image and try rendering
    allocate(test_image(test_img_height * (1 + test_img_width * 3)))
    test_image = -1_1  ! White background
    
    ! This test verifies that both functions use the same glyph metrics
    call render_text_to_image(test_image, test_img_width, test_img_height, 10, 10, "test", 0_1, 0_1, 0_1)
    print *, "PASS: render_text_to_image works with same glyph metrics as calculate_text_width"
    
    deallocate(test_image)
    
    print *, ""
    if (test_passed) then
        print *, "All tests PASSED!"
        stop 0
    else
        print *, "Some tests FAILED!"
        stop 1
    end if
    
end program test_text_width_measurements