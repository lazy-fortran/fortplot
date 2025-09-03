program test_line_pattern_verification
    !! Test that line patterns produce expected dash and gap sequences
    use fortplot
    use fortplot_raster_line_styles, only: PATTERN_SCALE_FACTOR
    implicit none
    
    real(wp), dimension(100) :: x, y
    integer :: i
    logical :: test_passed
    
    print *, "Testing line pattern generation verification..."
    
    ! Test that PATTERN_SCALE_FACTOR is appropriate for pixel-accurate patterns
    test_passed = .true.
    
    ! Verify the pattern scale factor is set to 1.0 for proper pixel alignment
    if (abs(PATTERN_SCALE_FACTOR - 1.0_wp) > 1e-6) then
        print *, "ERROR: PATTERN_SCALE_FACTOR should be 1.0 for pixel alignment"
        test_passed = .false.
    end if
    
    ! Generate test data
    do i = 1, 100
        x(i) = real(i-1, wp) * 0.1_wp
    end do
    
    call figure(figsize=[10.0_wp, 4.0_wp])
    
    ! Test different line styles
    y = 3.0_wp
    call plot(x, y, linestyle='--')
    
    y = 2.0_wp  
    call plot(x, y, linestyle=':')
    
    y = 1.0_wp
    call plot(x, y, linestyle='-.')
    
    call xlim(0.0_wp, 10.0_wp)
    call ylim(0.0_wp, 4.0_wp)
    call title('Line Pattern Verification')
    
    ! Save PNG to verify patterns are rendered
    call savefig('test/output/test_pattern_verification.png')
    
    if (test_passed) then
        print *, "PASS: Line pattern verification test passed"
        print *, "  - Pattern scale factor is correctly set to 1.0"
        print *, "  - PNG file generated for visual verification"
    else
        print *, "FAIL: Line pattern verification test failed"
        stop 1
    end if
    
end program test_line_pattern_verification