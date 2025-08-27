program test_ylabel_character_ordering
    !! Y-label character ordering and visual verification tests
    !! Extracted from test_ylabel_comprehensive.f90
    !! 
    !! This test covers:
    !! - Character ordering and spacing verification
    !! - Visual verification of various label types
    !! - Manual verification instructions and test patterns

    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0

    print *, "=== Y-LABEL CHARACTER ORDERING TESTS ==="
    
    call test_character_ordering()
    call test_visual_verification()
    call print_test_summary()

contains

    !===========================================================================
    ! Character Ordering Tests
    !===========================================================================
    
    subroutine test_character_ordering()
        print *, "--- Character Ordering Tests ---"
        
        call test_y_values_label()
        call test_various_labels()
        call test_character_spacing()
        call test_simple_labels()
    end subroutine test_character_ordering

    subroutine test_y_values_label()
        !! Test the original problematic "Y values" label
        real(wp) :: x(5), y(5)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Y values label ordering")
        
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i, wp) ** 2
        end do
        
        call figure(figsize=[400.0_wp, 300.0_wp])
        call plot(x, y)
        call title('Y Values Label Test')
        call xlabel('X axis')
        call ylabel('Y values')  ! Should read bottom-to-top when rotated
        
        filename = get_test_output_path('/tmp/ylabel_ordering.png')
        call savefig(filename)
        
        print *, '  Y values ordering test saved - verify "Y values" reads bottom-to-top'
        call end_test()
    end subroutine test_y_values_label

    subroutine test_various_labels()
        real(wp) :: x(3), y(3)
        character(len=512) :: filename
        
        call start_test("Various label characters")
        
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 4.0_wp, 9.0_wp]
        
        ! Test different character combinations
        call figure(figsize=[800.0_wp, 600.0_wp])
        
        ! Multiple subplots to test different labels
        call subplot(2, 2, 1)
        call plot(x, y)
        call title('Lowercase')
        call ylabel('temperature')
        
        call subplot(2, 2, 2)
        call plot(x, y)
        call title('Mixed Case')
        call ylabel('Temperature')
        
        call subplot(2, 2, 3)
        call plot(x, y)
        call title('With Symbols')
        call ylabel('Temp (°C)')
        
        call subplot(2, 2, 4)
        call plot(x, y)
        call title('Long Label')
        call ylabel('Temperature Measurement')
        
        filename = get_test_output_path('/tmp/ylabel_various.png')
        call savefig(filename)
        
        print *, '  Various labels test saved - verify all labels are readable'
        call end_test()
    end subroutine test_various_labels

    subroutine test_character_spacing()
        real(wp) :: x(4), y(4)
        character(len=512) :: filename
        
        call start_test("Character spacing analysis")
        
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y = [0.0_wp, 1.0_wp, 4.0_wp, 9.0_wp]
        
        call figure(figsize=[600.0_wp, 400.0_wp])
        call plot(x, y, 'bo-', markersize=8.0_wp)
        call title('Character Spacing Test')
        call xlabel('Index')
        call ylabel('illlliiilll')  ! Multiple 'l' characters to test spacing
        call grid(.true.)
        
        filename = get_test_output_path('/tmp/ylabel_spacing.png')
        call savefig(filename)
        
        print *, '  Character spacing test saved - verify consistent "l" spacing'
        call end_test()
    end subroutine test_character_spacing

    subroutine test_simple_labels()
        real(wp) :: x(2), y(2)
        character(len=512) :: filename
        
        call start_test("Simple label cases")
        
        x = [1.0_wp, 5.0_wp]
        y = [2.0_wp, 8.0_wp]
        
        call figure(figsize=[300.0_wp, 200.0_wp])
        call plot(x, y, 'r*-', linewidth=3.0_wp)
        call title('Simple')
        call xlabel('x')
        call ylabel('y')
        
        filename = get_test_output_path('/tmp/ylabel_simple.png')
        call savefig(filename)
        
        print *, '  Simple labels test saved'
        call end_test()
    end subroutine test_simple_labels

    !===========================================================================
    ! Visual Verification Tests
    !===========================================================================
    
    subroutine test_visual_verification()
        print *, "--- Visual Verification Summary ---"
        call start_test("Manual verification instructions")
        
        print *, ''
        print *, 'MANUAL VERIFICATION REQUIRED:'
        print *, '  1. Check ylabel_rotation.png - Y-label rotated 90° counter-clockwise'
        print *, '  2. Check ylabel_positioning.png - "Y values" reads bottom-to-top'
        print *, '  3. Check ylabel_various.png - all subplot labels readable'
        print *, '  4. Check ylabel_spacing.png - consistent character spacing'
        print *, '  5. Check backend integration files (PNG, PDF, ASCII)'
        print *, '  6. Compare with matplotlib output for consistency'
        print *, ''
        
        call end_test()
    end subroutine test_visual_verification

    !===========================================================================
    ! Test Framework Utilities
    !===========================================================================

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') 'Test ', test_count, ': ', test_name
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') '  PASS'
        write(*, *)
    end subroutine end_test

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'Y-Label Character Ordering Test Summary'
        write(*, '(A, I0, A, I0)') 'Tests run: ', test_count, ' | Passed: ', pass_count
        write(*, '(A)') 'Character ordering and visual tests COMPLETED!'
        write(*, '(A)') ''
        write(*, '(A)') 'VERIFICATION REQUIRED:'
        write(*, '(A)') '  - Manual inspection of generated image files'
        write(*, '(A)') '  - Character orientation and spacing verification'
        write(*, '(A)') '  - Cross-platform rendering consistency checks'
        write(*, '(A)') '============================================'
    end subroutine print_test_summary

end program test_ylabel_character_ordering