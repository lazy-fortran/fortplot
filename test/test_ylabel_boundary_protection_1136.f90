program test_ylabel_boundary_protection_1136
    !! CRITICAL TEST for Issue #1136 - Visual verification of ylabel boundary protection
    !! Creates test images that would historically show ylabel cutoff problems
    !! This test MUST pass to prove Issue #1136 is resolved
    
    use fortplot
    use fortplot_raster_axes, only: compute_ylabel_x_pos
    use fortplot_layout, only: plot_area_t, plot_margins_t, calculate_plot_area
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(100), y_large(50)
    integer :: i
    logical :: all_tests_passed
    integer :: test_count, pass_count
    
    ! Test positioning calculations
    type(plot_area_t) :: plot_area
    type(plot_margins_t) :: margins
    integer :: x_pos_long, x_pos_extreme
    
    all_tests_passed = .true.
    test_count = 0
    pass_count = 0
    
    print *, '=== CRITICAL TEST: Y-label Boundary Protection (Issue #1136) ==='
    print *, 'This test MUST generate visible images without ylabel cutoff'
    print *, ''
    
    ! Generate challenging test data
    do i = 1, 100
        x(i) = real(i-1, wp) * 0.05_wp
        if (i <= 50) then
            y_large(i) = sin(x(i)) * 123456.789_wp  ! Large numbers = wide tick labels
        end if
    end do
    
    ! VISUAL TEST 1: Extreme ylabel length (should NOT be cut off)
    test_count = test_count + 1
    print *, 'VISUAL TEST 1: Generating extremely long ylabel test...'
    call figure(figsize=[8.0_8, 6.0_8])
    call plot(x(1:30), x(1:30)**2, 'b-')
    call title('Issue #1136 Critical Test - Long Y-label')
    call xlabel('X values')
    call ylabel('EXTREMELY LONG Y-AXIS LABEL THAT MUST REMAIN COMPLETELY VISIBLE AND NOT BE CUT OFF BY WHITE BLOCKS OR CANVAS BOUNDARIES')
    call savefig('test_ylabel_extreme_length_1136.png')
    print *, '  Generated: test_ylabel_extreme_length_1136.png'
    print *, '  REQUIREMENT: Entire ylabel must be visible, no cutoff'
    
    ! VISUAL TEST 2: Wide tick labels pushing ylabel (challenging scenario)
    test_count = test_count + 1
    print *, 'VISUAL TEST 2: Generating wide tick labels scenario...'
    call figure(figsize=[6.0_8, 8.0_8])
    call plot(x(1:50), y_large, 'r--')
    call title('Wide Tick Labels Test')
    call xlabel('X axis')
    call ylabel('Y-label Must Stay Visible With Wide Ticks')
    call savefig('test_ylabel_wide_ticks_1136.png')
    print *, '  Generated: test_ylabel_wide_ticks_1136.png'
    print *, '  REQUIREMENT: Y-label visible despite wide tick labels'
    
    ! VISUAL TEST 3: Small canvas stress test (tight margins)
    test_count = test_count + 1
    print *, 'VISUAL TEST 3: Generating small canvas stress test...'
    call figure(figsize=[4.0_8, 3.0_8])
    call plot(x(1:20), x(1:20)*100.0_wp, 'g:')
    call title('Small Canvas Y-label Test')
    call xlabel('X')
    call ylabel('Long Y-label In Cramped Space Should Still Show Completely')
    call savefig('test_ylabel_small_canvas_1136.png')
    print *, '  Generated: test_ylabel_small_canvas_1136.png'
    print *, '  REQUIREMENT: Y-label fits within small canvas bounds'
    
    ! BOUNDARY CALCULATION TEST: Verify positioning math
    test_count = test_count + 1
    print *, 'BOUNDARY TEST 4: Testing positioning calculations...'
    call calculate_plot_area(800, 600, margins, plot_area)
    
    ! Test with extremely long ylabel (120 pixels wide when rotated)
    x_pos_long = compute_ylabel_x_pos(plot_area, 120, 50)  ! 120px ylabel, 50px tick labels
    if (x_pos_long >= 15 .and. x_pos_long + 120 <= 800) then
        print *, '  ✓ PASS: Long ylabel positioned correctly (x=', x_pos_long, ')'
        pass_count = pass_count + 1
    else
        print *, '  ✗ FAIL: Long ylabel boundary violation (x=', x_pos_long, ')'
        all_tests_passed = .false.
    end if
    
    ! Test with extremely wide ylabel (200 pixels - should use adaptive margin)
    test_count = test_count + 1
    x_pos_extreme = compute_ylabel_x_pos(plot_area, 200, 80)  ! 200px ylabel, 80px tick labels
    if (x_pos_extreme >= 50 .and. x_pos_extreme + 200 <= 800) then  ! Should use adaptive margin max(15, 200/4)=50
        print *, '  ✓ PASS: Extreme ylabel uses adaptive margin (x=', x_pos_extreme, ')'
        pass_count = pass_count + 1
    else
        print *, '  ✗ FAIL: Extreme ylabel margin inadequate (x=', x_pos_extreme, ')'
        all_tests_passed = .false.
    end if
    
    print *, ''
    print *, '=== CRITICAL VERIFICATION INSTRUCTIONS ==='
    print *, 'Manually inspect the generated PNG files:'
    print *, ''
    print *, '1. test_ylabel_extreme_length_1136.png'
    print *, '   ✓ MUST SHOW: Complete long ylabel text visible'
    print *, '   ✗ FAILURE: Any ylabel text cut off or covered by white blocks'
    print *, ''
    print *, '2. test_ylabel_wide_ticks_1136.png'
    print *, '   ✓ MUST SHOW: Y-label visible with proper spacing from wide tick labels'
    print *, '   ✗ FAILURE: Y-label overlapped or cut off due to wide ticks'
    print *, ''
    print *, '3. test_ylabel_small_canvas_1136.png'
    print *, '   ✓ MUST SHOW: Y-label fits within canvas boundaries'
    print *, '   ✗ FAILURE: Y-label extends beyond canvas or is clipped'
    print *, ''
    
    ! Final result
    print *, '=== ISSUE #1136 STATUS VERIFICATION ==='
    print *, 'Generated', test_count, 'critical test images for manual inspection'
    print *, 'Boundary calculation tests:', pass_count, '/', test_count - 3, 'passed'
    
    if (pass_count == test_count - 3) then  ! Subtract visual tests from calculation tests
        print *, ''
        print *, '✓ BOUNDARY CALCULATIONS PASSED'
        print *, '  - Adaptive margin system working'
        print *, '  - Canvas boundary protection active'
        print *, '  - Position calculations within valid ranges'
        print *, ''
        print *, '⚠  MANUAL VERIFICATION REQUIRED:'
        print *, '   Inspect the 3 generated PNG files to confirm ylabel visibility'
        print *, '   If all ylabels are completely visible, Issue #1136 is RESOLVED'
        stop 0
    else
        print *, ''
        print *, '✗ BOUNDARY CALCULATION FAILURES DETECTED'
        print *, '  Issue #1136 fix is INCOMPLETE - boundary math errors'
        print *, '  Generated images may still show ylabel cutoff problems'
        stop 1
    end if
    
end program test_ylabel_boundary_protection_1136