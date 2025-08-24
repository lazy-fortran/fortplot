program test_antialiasing_comprehensive
    !! Comprehensive antialiasing test using ImageMagick metrics
    !! Tests multiple scenarios and provides objective quality measurements
    
    use fortplot
    use fortplot_testing
    use fortplot_security, only: get_test_output_path
    use fortplot_imagemagick
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer :: test_num, total_tests, passed_tests
    logical :: magick_available
    
    print *, "=== Comprehensive Antialiasing Test Suite ==="
    
    ! Check ImageMagick availability
    magick_available = check_imagemagick_available()
    if (.not. magick_available) then
#if defined(_WIN32) || defined(_WIN64) || defined(__MINGW32__) || defined(__MINGW64__)
        print *, "WARNING: ImageMagick not detected on Windows."
        print *, "  This is a known issue with path detection in execute_command_line."
        print *, "  Skipping comprehensive ImageMagick validation tests on Windows."
        print *, "=== SKIP: Test skipped on Windows due to ImageMagick detection issue ==="
        stop 0  ! Exit successfully on Windows
#else
        print *, "ERROR: ImageMagick not found. This test requires ImageMagick."
        print *, "Please install ImageMagick for your platform:"
        print *, "  Ubuntu/Debian: sudo apt-get install imagemagick"
        print *, "  macOS: brew install imagemagick"
        error stop 1
#endif
    end if
    
    total_tests = 0
    passed_tests = 0
    
    ! Test 1: Diagonal lines (most sensitive to antialiasing)
    call test_diagonal_lines(test_num, total_tests, passed_tests)
    
    ! Test 2: Curved lines
    call test_curved_lines(test_num, total_tests, passed_tests)
    
    ! Test 3: Fine patterns
    call test_fine_patterns(test_num, total_tests, passed_tests)
    
    ! Test 4: Text rendering (if applicable)
    call test_text_rendering(test_num, total_tests, passed_tests)
    
    ! Test 5: Grid lines
    call test_grid_lines(test_num, total_tests, passed_tests)
    
    ! Summary
    print *, ""
    print *, "=== Test Summary ==="
    print '(A,I0,A,I0)', "Passed: ", passed_tests, " / ", total_tests
    
    if (passed_tests == total_tests) then
        print *, "=== ALL TESTS PASSED: Antialiasing quality verified ==="
    else
        print *, "=== SOME TESTS FAILED: Antialiasing may be degraded ==="
        error stop 1
    end if
    
contains
    
    subroutine test_diagonal_lines(test_id, total, passed)
        integer, intent(out) :: test_id
        integer, intent(inout) :: total, passed
        character(len=512) :: filename
        real(wp) :: x(100), y(100)
        real(wp) :: smoothness
        integer :: i
        logical :: test_pass
        
        test_id = 1
        total = total + 1
        
        print *, ""
        print *, "Test 1: Diagonal Lines (45-degree)"
        
        filename = get_test_output_path("output/test/aa_diagonal.png")
        
        ! Generate diagonal line data
        do i = 1, 100
            x(i) = real(i-1, wp)
            y(i) = real(i-1, wp)
        end do
        
        ! Create plot
        call figure()
        call plot(x, y, 'Diagonal', 'b-')
        call xlabel('X')
        call ylabel('Y')
        call title('Diagonal Line Test')
        call savefig(filename)
        
        ! Analyze smoothness
        smoothness = analyze_edge_smoothness(filename)
        
        test_pass = smoothness > 60.0_wp
        
        if (test_pass) then
            print '(A,F6.2)', "  PASS: Edge smoothness = ", smoothness
            passed = passed + 1
        else
            print '(A,F6.2)', "  FAIL: Poor edge smoothness = ", smoothness
        end if
        
    end subroutine test_diagonal_lines
    
    subroutine test_curved_lines(test_id, total, passed)
        integer, intent(out) :: test_id
        integer, intent(inout) :: total, passed
        character(len=512) :: filename
        real(wp) :: x(200), y(200)
        real(wp) :: smoothness
        integer :: i
        logical :: test_pass
        
        test_id = 2
        total = total + 1
        
        print *, ""
        print *, "Test 2: Curved Lines (Sinusoidal)"
        
        filename = get_test_output_path("output/test/aa_curved.png")
        
        ! Generate curved line data
        do i = 1, 200
            x(i) = real(i-1, wp) * 0.1_wp
            y(i) = sin(x(i))
        end do
        
        ! Create plot
        call figure()
        call plot(x, y, 'Sine Wave', 'r-')
        call xlabel('X')
        call ylabel('Y')
        call title('Curved Line Test')
        call savefig(filename)
        
        ! Analyze smoothness
        smoothness = analyze_edge_smoothness(filename)
        
        test_pass = smoothness > 55.0_wp
        
        if (test_pass) then
            print '(A,F6.2)', "  PASS: Edge smoothness = ", smoothness
            passed = passed + 1
        else
            print '(A,F6.2)', "  FAIL: Poor edge smoothness = ", smoothness
        end if
        
    end subroutine test_curved_lines
    
    subroutine test_fine_patterns(test_id, total, passed)
        integer, intent(out) :: test_id
        integer, intent(inout) :: total, passed
        character(len=512) :: filename
        real(wp) :: x(300), y1(300), y2(300)
        real(wp) :: smoothness
        integer :: i
        logical :: test_pass
        
        test_id = 3
        total = total + 1
        
        print *, ""
        print *, "Test 3: Fine Patterns (High-frequency)"
        
        filename = get_test_output_path("output/test/aa_patterns.png")
        
        ! Generate fine pattern data
        do i = 1, 300
            x(i) = real(i-1, wp) * 0.05_wp
            y1(i) = sin(x(i) * 10.0_wp) * 0.5_wp
            y2(i) = cos(x(i) * 8.0_wp) * 0.4_wp + 1.0_wp
        end do
        
        ! Create plot
        call figure()
        call plot(x, y1, 'High Freq 1', 'g-')
        call plot(x, y2, 'High Freq 2', 'm-')
        call xlabel('X')
        call ylabel('Y')
        call title('Fine Pattern Test')
        call savefig(filename)
        
        ! Analyze smoothness
        smoothness = analyze_edge_smoothness(filename)
        
        test_pass = smoothness > 50.0_wp
        
        if (test_pass) then
            print '(A,F6.2)', "  PASS: Edge smoothness = ", smoothness
            passed = passed + 1
        else
            print '(A,F6.2)', "  FAIL: Poor edge smoothness = ", smoothness
        end if
        
    end subroutine test_fine_patterns
    
    subroutine test_text_rendering(test_id, total, passed)
        integer, intent(out) :: test_id
        integer, intent(inout) :: total, passed
        character(len=512) :: filename
        real(wp) :: x(10), y(10)
        real(wp) :: smoothness
        integer :: i
        logical :: test_pass
        
        test_id = 4
        total = total + 1
        
        print *, ""
        print *, "Test 4: Text Rendering"
        
        filename = get_test_output_path("output/test/aa_text.png")
        
        ! Generate simple data
        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = real(i, wp) ** 2
        end do
        
        ! Create plot with text
        call figure()
        call plot(x, y, 'Data', 'ko-')
        call xlabel('X Axis Label')
        call ylabel('Y Axis Label')
        call title('Text Antialiasing Test')
        call savefig(filename)
        
        ! Analyze smoothness
        smoothness = analyze_edge_smoothness(filename)
        
        ! Text generally has lower smoothness scores
        test_pass = smoothness > 45.0_wp
        
        if (test_pass) then
            print '(A,F6.2)', "  PASS: Text smoothness = ", smoothness
            passed = passed + 1
        else
            print '(A,F6.2)', "  FAIL: Poor text smoothness = ", smoothness
        end if
        
    end subroutine test_text_rendering
    
    subroutine test_grid_lines(test_id, total, passed)
        integer, intent(out) :: test_id
        integer, intent(inout) :: total, passed
        character(len=512) :: filename
        real(wp) :: x(50), y(50)
        real(wp) :: smoothness
        integer :: i
        logical :: test_pass
        
        test_id = 5
        total = total + 1
        
        print *, ""
        print *, "Test 5: Grid Lines"
        
        filename = get_test_output_path("output/test/aa_grid.png")
        
        ! Generate data
        do i = 1, 50
            x(i) = real(i, wp) * 0.2_wp
            y(i) = exp(-x(i) * 0.1_wp) * cos(x(i))
        end do
        
        ! Create plot with grid
        call figure()
        call plot(x, y, 'Damped Oscillation', 'b-')
        call xlabel('Time')
        call ylabel('Amplitude')
        call title('Grid Line Test')
        ! Grid is enabled by default in fortplot
        call savefig(filename)
        
        ! Analyze smoothness
        smoothness = analyze_edge_smoothness(filename)
        
        test_pass = smoothness > 50.0_wp
        
        if (test_pass) then
            print '(A,F6.2)', "  PASS: Grid smoothness = ", smoothness
            passed = passed + 1
        else
            print '(A,F6.2)', "  FAIL: Poor grid smoothness = ", smoothness
        end if
        
    end subroutine test_grid_lines
    
end program test_antialiasing_comprehensive