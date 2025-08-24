program test_y_label_orientation
    !! Comprehensive test to verify Y-axis label character ordering and spacing
    !! 
    !! This test creates multiple plots with different Y-labels and visually verifies:
    !! 1. Character ordering (should read bottom-to-top for 90° CCW rotation)
    !! 2. Character spacing (should be consistent, especially around 'l')
    !! 3. Overall readability and matplotlib compatibility
    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    print *, "=== Y-axis Label Orientation Test ==="
    
    ! Test 1: Original problematic case
    call test_y_values_label()
    
    ! Test 2: Different character combinations
    call test_various_labels()
    
    ! Test 3: Character spacing analysis
    call test_character_spacing()
    
    ! Test 4: Comparison with simple labels
    call test_simple_labels()
    
    print *, "All Y-label orientation tests completed!"
    print *, "MANUAL VERIFICATION REQUIRED:"
    print *, "  1. Check test_y_values.png - 'Y values' should read bottom-to-top"
    print *, "  2. Check test_various_labels.png - all labels should be readable"
    print *, "  3. Check test_spacing.png - verify consistent character spacing"
    print *, "  4. Check test_simple.png - verify simple cases work correctly"

contains

    subroutine test_y_values_label()
        !! Test the original problematic "Y values" label
        real(wp), dimension(5) :: x, y
        integer :: i
        
        print *, "Test 1: Y values label orientation"
        
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i, wp) ** 2
        end do
        
        call figure(figsize=[400.0_wp, 300.0_wp])
        call plot(x, y)
        call title('Y Values Label Test')
        call xlabel('X axis')
        call ylabel('Y values')  ! The problematic label
        call savefig(get_test_output_path('build/test/test_y_values.png'))
        
        print *, "  Created: test_y_values.png"
        print *, "  VERIFY: 'Y values' should read from bottom 'Y' to top 's'"
    end subroutine test_y_values_label

    subroutine test_various_labels()
        !! Test different Y-labels with various character combinations
        real(wp), dimension(3) :: x, y1, y2, y3, y4
        integer :: i
        
        print *, "Test 2: Various Y-label combinations"
        
        do i = 1, 3
            x(i) = real(i, wp)
            y1(i) = real(i, wp)
            y2(i) = real(i, wp) + 5.0_wp
            y3(i) = real(i, wp) + 10.0_wp
            y4(i) = real(i, wp) + 15.0_wp
        end do
        
        ! Test 1: Problematic 'l' characters
        call figure(figsize=[200.0_wp, 300.0_wp])
        call plot(x, y1)
        call title('L Test')
        call xlabel('X')
        call ylabel('lllll')  ! Multiple 'l' chars to test spacing
        call savefig(get_test_output_path('build/test/test_label_lllll.png'))
        
        ! Test 2: Mixed case
        call figure(figsize=[200.0_wp, 300.0_wp])
        call plot(x, y2)
        call title('Mixed Case')
        call xlabel('X')
        call ylabel('Values')
        call savefig(get_test_output_path('build/test/test_label_values.png'))
        
        ! Test 3: All caps
        call figure(figsize=[200.0_wp, 300.0_wp])
        call plot(x, y3)
        call title('All Caps')
        call xlabel('X')
        call ylabel('VOLTAGE')
        call savefig(get_test_output_path('build/test/test_label_voltage.png'))
        
        ! Test 4: Numbers and letters
        call figure(figsize=[200.0_wp, 300.0_wp])
        call plot(x, y4)
        call title('Mixed')
        call xlabel('X')
        call ylabel('Data123')
        call savefig(get_test_output_path('build/test/test_label_data123.png'))
        
        print *, "  Created: test_label_*.png files"
        print *, "  VERIFY: All labels should read bottom-to-top correctly"
    end subroutine test_various_labels

    subroutine test_character_spacing()
        !! Test character spacing consistency
        real(wp), dimension(2) :: x, y
        
        print *, "Test 3: Character spacing analysis"
        
        x = [1.0_wp, 2.0_wp]
        y = [1.0_wp, 4.0_wp]
        
        ! Test problematic character combinations
        call figure(figsize=[200.0_wp, 300.0_wp])
        call plot(x, y)
        call title('Spacing Test')
        call xlabel('X')
        call ylabel('Will Wall')  ! 'l' next to 'l' and 'W'
        call savefig(get_test_output_path('build/test/test_spacing_will_wall.png'))
        
        call figure(figsize=[200.0_wp, 300.0_wp])
        call plot(x, y)
        call title('Spacing Test 2')
        call xlabel('X')
        call ylabel('TALL WALL')  ! All caps with 'L's
        call savefig(get_test_output_path('build/test/test_spacing_tall_wall.png'))
        
        print *, "  Created: test_spacing_*.png files"
        print *, "  VERIFY: Character spacing should be consistent, no weird gaps"
    end subroutine test_character_spacing

    subroutine test_simple_labels()
        !! Test simple cases that should definitely work
        real(wp), dimension(2) :: x, y
        
        print *, "Test 4: Simple label verification"
        
        x = [0.0_wp, 1.0_wp]
        y = [0.0_wp, 1.0_wp]
        
        ! Single character
        call figure(figsize=[150.0_wp, 200.0_wp])
        call plot(x, y)
        call title('Single Char')
        call xlabel('X')
        call ylabel('Y')
        call savefig(get_test_output_path('build/test/test_simple_y.png'))
        
        ! Two characters
        call figure(figsize=[150.0_wp, 200.0_wp])
        call plot(x, y)
        call title('Two Chars')
        call xlabel('X')
        call ylabel('AB')
        call savefig(get_test_output_path('build/test/test_simple_ab.png'))
        
        ! Three characters
        call figure(figsize=[150.0_wp, 200.0_wp])
        call plot(x, y)
        call title('Three Chars')
        call xlabel('X')
        call ylabel('ABC')
        call savefig(get_test_output_path('build/test/test_simple_abc.png'))
        
        print *, "  Created: test_simple_*.png files"
        print *, "  VERIFY: Simple cases should be perfect"
    end subroutine test_simple_labels

end program test_y_label_orientation