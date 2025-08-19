program test_color_syntax_parsing
    !! Test suite for matplotlib-compatible color syntax parsing
    !! Tests hex colors, RGB tuples, named colors, and single letter colors
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_colors, only: parse_color, parse_color_rgba, is_valid_color
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    ! Core color parsing tests
    call test_hex_color_parsing()
    call test_rgb_tuple_parsing()
    call test_named_color_parsing()
    call test_single_letter_color_parsing()
    call test_color_validation()
    call test_color_normalization()
    call test_alpha_channel_support()
    call test_edge_cases()
    
    call print_test_summary()
    
contains

    subroutine test_hex_color_parsing()
        ! Given: Matplotlib-compatible hex color strings
        ! When: Parsing hex colors with parse_color function
        ! Then: Should return correct RGB values
        
        real(wp) :: rgb(3)
        logical :: success
        
        call start_test("Hex color parsing")
        
        ! Test standard 6-digit hex
        call parse_color('#FF0000', rgb, success)
        call assert_true(success, "Parse #FF0000 success")
        call assert_equal(rgb(1), 1.0_wp, "Red component from #FF0000")
        call assert_equal(rgb(2), 0.0_wp, "Green component from #FF0000")
        call assert_equal(rgb(3), 0.0_wp, "Blue component from #FF0000")
        
        ! Test 3-digit hex shorthand
        call parse_color('#F00', rgb, success)
        call assert_true(success, "Parse #F00 success")
        call assert_equal(rgb(1), 1.0_wp, "Red component from #F00")
        call assert_equal(rgb(2), 0.0_wp, "Green component from #F00")
        call assert_equal(rgb(3), 0.0_wp, "Blue component from #F00")
        
        ! Test mixed case hex
        call parse_color('#00ff00', rgb, success)
        call assert_true(success, "Parse lowercase hex success")
        call assert_equal(rgb(1), 0.0_wp, "Red from lowercase hex")
        call assert_equal(rgb(2), 1.0_wp, "Green from lowercase hex")
        call assert_equal(rgb(3), 0.0_wp, "Blue from lowercase hex")
        
        ! Test invalid hex
        call parse_color('#GGGGGG', rgb, success)
        call assert_false(success, "Invalid hex should fail")
        
        call end_test()
    end subroutine test_hex_color_parsing

    subroutine test_rgb_tuple_parsing()
        ! Given: Matplotlib-compatible RGB tuple formats
        ! When: Parsing RGB tuples with parse_color function
        ! Then: Should return correct normalized RGB values
        
        real(wp) :: rgb(3)
        logical :: success
        
        call start_test("RGB tuple parsing")
        
        ! Test normalized RGB (0-1)
        call parse_color('(1.0, 0.5, 0.0)', rgb, success)
        call assert_true(success, "Parse normalized RGB success")
        call assert_equal(rgb(1), 1.0_wp, "Red from normalized RGB")
        call assert_equal(rgb(2), 0.5_wp, "Green from normalized RGB")
        call assert_equal(rgb(3), 0.0_wp, "Blue from normalized RGB")
        
        ! Test 8-bit RGB (0-255)
        call parse_color('(255, 128, 0)', rgb, success)
        call assert_true(success, "Parse 8-bit RGB success")
        call assert_equal(rgb(1), 1.0_wp, "Red from 8-bit RGB")
        call assert_equal(rgb(2), 128.0_wp/255.0_wp, "Green from 8-bit RGB")
        call assert_equal(rgb(3), 0.0_wp, "Blue from 8-bit RGB")
        
        ! Test whitespace tolerance
        call parse_color('( 0.2 , 0.4 , 0.8 )', rgb, success)
        call assert_true(success, "Parse RGB with whitespace success")
        call assert_equal(rgb(1), 0.2_wp, "Red with whitespace")
        call assert_equal(rgb(2), 0.4_wp, "Green with whitespace")
        call assert_equal(rgb(3), 0.8_wp, "Blue with whitespace")
        
        ! Test invalid RGB format
        call parse_color('(1.0, 0.5)', rgb, success)
        call assert_false(success, "Invalid RGB tuple should fail")
        
        call end_test()
    end subroutine test_rgb_tuple_parsing

    subroutine test_named_color_parsing()
        ! Given: Matplotlib-compatible named colors
        ! When: Parsing named colors with parse_color function
        ! Then: Should return correct RGB values matching matplotlib
        
        real(wp) :: rgb(3)
        logical :: success
        
        call start_test("Named color parsing")
        
        ! Test CSS4 colors
        call parse_color('red', rgb, success)
        call assert_true(success, "Parse 'red' success")
        call assert_equal(rgb(1), 1.0_wp, "Red component from 'red'")
        call assert_equal(rgb(2), 0.0_wp, "Green component from 'red'")
        call assert_equal(rgb(3), 0.0_wp, "Blue component from 'red'")
        
        call parse_color('blue', rgb, success)
        call assert_true(success, "Parse 'blue' success")
        call assert_equal(rgb(1), 0.0_wp, "Red component from 'blue'")
        call assert_equal(rgb(2), 0.0_wp, "Green component from 'blue'")
        call assert_equal(rgb(3), 1.0_wp, "Blue component from 'blue'")
        
        call parse_color('green', rgb, success)
        call assert_true(success, "Parse 'green' success")
        call assert_equal(rgb(1), 0.0_wp, "Red component from 'green'")
        call assert_equal(rgb(2), 0.5_wp, "Green component from 'green'")
        call assert_equal(rgb(3), 0.0_wp, "Blue component from 'green'")
        
        ! Test case insensitivity
        call parse_color('RED', rgb, success)
        call assert_true(success, "Parse uppercase 'RED' success")
        call assert_equal(rgb(1), 1.0_wp, "Red from uppercase")
        
        ! Test invalid color name
        call parse_color('invalidcolor', rgb, success)
        call assert_false(success, "Invalid color name should fail")
        
        call end_test()
    end subroutine test_named_color_parsing

    subroutine test_single_letter_color_parsing()
        ! Given: Matplotlib-compatible single letter color codes
        ! When: Parsing single letter colors with parse_color function
        ! Then: Should return correct RGB values
        
        real(wp) :: rgb(3)
        logical :: success
        
        call start_test("Single letter color parsing")
        
        ! Test matplotlib single letter codes
        call parse_color('r', rgb, success)
        call assert_true(success, "Parse 'r' success")
        call assert_equal(rgb(1), 1.0_wp, "Red from 'r'")
        call assert_equal(rgb(2), 0.0_wp, "Green from 'r'")
        call assert_equal(rgb(3), 0.0_wp, "Blue from 'r'")
        
        call parse_color('g', rgb, success)
        call assert_true(success, "Parse 'g' success")
        call assert_equal(rgb(1), 0.0_wp, "Red from 'g'")
        call assert_equal(rgb(2), 0.5_wp, "Green from 'g'")
        call assert_equal(rgb(3), 0.0_wp, "Blue from 'g'")
        
        call parse_color('b', rgb, success)
        call assert_true(success, "Parse 'b' success")
        call assert_equal(rgb(1), 0.0_wp, "Red from 'b'")
        call assert_equal(rgb(2), 0.0_wp, "Green from 'b'")
        call assert_equal(rgb(3), 1.0_wp, "Blue from 'b'")
        
        call parse_color('k', rgb, success)
        call assert_true(success, "Parse 'k' (black) success")
        call assert_equal(rgb(1), 0.0_wp, "Red from 'k'")
        call assert_equal(rgb(2), 0.0_wp, "Green from 'k'")
        call assert_equal(rgb(3), 0.0_wp, "Blue from 'k'")
        
        call parse_color('w', rgb, success)
        call assert_true(success, "Parse 'w' (white) success")
        call assert_equal(rgb(1), 1.0_wp, "Red from 'w'")
        call assert_equal(rgb(2), 1.0_wp, "Green from 'w'")
        call assert_equal(rgb(3), 1.0_wp, "Blue from 'w'")
        
        ! Test invalid single letter
        call parse_color('x', rgb, success)
        call assert_false(success, "Invalid single letter should fail")
        
        call end_test()
    end subroutine test_single_letter_color_parsing

    subroutine test_color_validation()
        ! Given: Various color format strings
        ! When: Validating color format with is_valid_color function
        ! Then: Should correctly identify valid/invalid formats
        
        logical :: is_valid
        
        call start_test("Color validation")
        
        ! Valid formats
        is_valid = is_valid_color('#FF0000')
        call assert_true(is_valid, "Hex color is valid")
        
        is_valid = is_valid_color('red')
        call assert_true(is_valid, "Named color is valid")
        
        is_valid = is_valid_color('r')
        call assert_true(is_valid, "Single letter is valid")
        
        is_valid = is_valid_color('(1.0, 0.5, 0.0)')
        call assert_true(is_valid, "RGB tuple is valid")
        
        ! Invalid formats
        is_valid = is_valid_color('#GGGGGG')
        call assert_false(is_valid, "Invalid hex is not valid")
        
        is_valid = is_valid_color('notacolor')
        call assert_false(is_valid, "Invalid name is not valid")
        
        is_valid = is_valid_color('')
        call assert_false(is_valid, "Empty string is not valid")
        
        call end_test()
    end subroutine test_color_validation

    subroutine test_color_normalization()
        ! Given: Color values in different ranges
        ! When: Normalizing colors to [0,1] range
        ! Then: Should handle out-of-range values correctly
        
        real(wp) :: rgb(3)
        logical :: success
        
        call start_test("Color normalization")
        
        ! Test clamping to [0,1] range
        call parse_color('(1.5, -0.1, 0.5)', rgb, success)
        call assert_true(success, "Parse out-of-range RGB success")
        call assert_equal(rgb(1), 1.0_wp, "Clamp high value to 1.0")
        call assert_equal(rgb(2), 0.0_wp, "Clamp negative value to 0.0")
        call assert_equal(rgb(3), 0.5_wp, "Normal value unchanged")
        
        ! Test 8-bit to normalized conversion
        call parse_color('(256, -1, 128)', rgb, success)
        call assert_true(success, "Parse 8-bit out-of-range success")
        call assert_equal(rgb(1), 1.0_wp, "Clamp 8-bit high value")
        call assert_equal(rgb(2), 0.0_wp, "Clamp 8-bit negative value")
        call assert_equal(rgb(3), 128.0_wp/255.0_wp, "Convert 8-bit normal value")
        
        call end_test()
    end subroutine test_color_normalization

    subroutine test_alpha_channel_support()
        ! Given: Color specifications with alpha channel
        ! When: Parsing RGBA colors
        ! Then: Should handle alpha channel correctly
        
        real(wp) :: rgba(4)
        logical :: success
        
        call start_test("Alpha channel support")
        
        ! Test RGBA tuple
        call parse_color_rgba('(1.0, 0.5, 0.0, 0.8)', rgba, success)
        call assert_true(success, "Parse RGBA tuple success")
        call assert_equal(rgba(1), 1.0_wp, "Red from RGBA")
        call assert_equal(rgba(2), 0.5_wp, "Green from RGBA")
        call assert_equal(rgba(3), 0.0_wp, "Blue from RGBA")
        call assert_equal(rgba(4), 0.8_wp, "Alpha from RGBA")
        
        ! Test 8-digit hex with alpha
        call parse_color_rgba('#FF800080', rgba, success)
        call assert_true(success, "Parse 8-digit hex success")
        call assert_equal(rgba(1), 1.0_wp, "Red from hex RGBA")
        call assert_equal(rgba(2), 128.0_wp/255.0_wp, "Green from hex RGBA")
        call assert_equal(rgba(3), 0.0_wp, "Blue from hex RGBA")
        call assert_equal(rgba(4), 128.0_wp/255.0_wp, "Alpha from hex RGBA")
        
        ! Test default alpha for RGB
        call parse_color_rgba('red', rgba, success)
        call assert_true(success, "Parse named color for RGBA success")
        call assert_equal(rgba(4), 1.0_wp, "Default alpha is 1.0")
        
        call end_test()
    end subroutine test_alpha_channel_support

    subroutine test_edge_cases()
        ! Given: Edge case color specifications
        ! When: Parsing problematic inputs
        ! Then: Should handle gracefully without crashes
        
        real(wp) :: rgb(3)
        logical :: success
        
        call start_test("Edge cases")
        
        ! Test empty and whitespace
        call parse_color('', rgb, success)
        call assert_false(success, "Empty string fails gracefully")
        
        call parse_color('   ', rgb, success)
        call assert_false(success, "Whitespace only fails gracefully")
        
        ! Test malformed hex
        call parse_color('#FF00', rgb, success)
        call assert_false(success, "Short hex fails gracefully")
        
        call parse_color('#FF0000FF00', rgb, success)
        call assert_false(success, "Long hex fails gracefully")
        
        ! Test malformed RGB
        call parse_color('(1.0, 0.5', rgb, success)
        call assert_false(success, "Unclosed RGB fails gracefully")
        
        call parse_color('1.0, 0.5, 0.0)', rgb, success)
        call assert_false(success, "Missing open paren fails gracefully")
        
        ! Test very long strings
        call parse_color(repeat('a', 1000), rgb, success)
        call assert_false(success, "Very long string fails gracefully")
        
        call end_test()
    end subroutine test_edge_cases

    ! Using real implementations from fortplot_colors module

    ! Testing utilities
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "Running test: ", trim(test_name)
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') "  PASS"
    end subroutine end_test

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            write(*, '(A,A)') "  FAIL: ", trim(message)
            error stop "Test assertion failed"
        end if
    end subroutine assert_true

    subroutine assert_false(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (condition) then
            write(*, '(A,A)') "  FAIL: ", trim(message)
            error stop "Test assertion failed"
        end if
    end subroutine assert_false

    subroutine assert_equal(actual, expected, message)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        real(wp), parameter :: tolerance = 1e-10_wp
        if (abs(actual - expected) > tolerance) then
            write(*, '(A,A,F10.6,A,F10.6)') "  FAIL: ", trim(message), &
                  actual, " /= ", expected
            error stop "Test assertion failed"
        end if
    end subroutine assert_equal

    subroutine print_test_summary()
        write(*, '(A,I0,A,I0,A)') "Tests completed: ", pass_count, "/", test_count, " passed"
        if (pass_count /= test_count) error stop "Some tests failed"
    end subroutine print_test_summary

end program test_color_syntax_parsing