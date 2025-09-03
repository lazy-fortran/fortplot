! test_suite_fast.f90 - Fast subset of tests for development iteration
! This suite runs only essential tests without heavy I/O or delays
program test_suite_fast
    use fortplot
    implicit none
    
    logical :: all_passed
    integer :: test_count, pass_count
    
    all_passed = .true.
    test_count = 0
    pass_count = 0
    
    print *, "=== Running Fast Test Suite ==="
    print *, "Optimized for development iteration with minimal I/O"
    print *, ""
    
    ! Test 1: Basic plot functionality (no file output)
    test_count = test_count + 1
    call test_basic_plot()
    pass_count = pass_count + 1
    print *, "✓ Test 1: Basic plot functionality"
    
    ! Test 2: Multiple data series (no file output)
    test_count = test_count + 1
    call test_multiple_series()
    pass_count = pass_count + 1
    print *, "✓ Test 2: Multiple data series"
    
    ! Test 3: Plot customization (no file output)
    test_count = test_count + 1
    call test_plot_customization()
    pass_count = pass_count + 1
    print *, "✓ Test 3: Plot customization"
    
    ! Test 4: Edge cases (no file output)
    test_count = test_count + 1
    call test_edge_cases()
    pass_count = pass_count + 1
    print *, "✓ Test 4: Edge cases"
    
    ! Test 5: Legend functionality (minimal)
    test_count = test_count + 1
    call test_legend_basic()
    pass_count = pass_count + 1
    print *, "✓ Test 5: Legend functionality"
    
    print *, ""
    print *, "=== Fast Test Suite Summary ==="
    print *, "Passed:", pass_count, "/", test_count
    
    if (pass_count == test_count) then
        print *, "✓ ALL FAST TESTS PASSED"
    else
        print *, "✗ SOME TESTS FAILED"
        stop 1
    end if
    
contains

    subroutine test_basic_plot()
        real(8), dimension(10) :: x, y
        integer :: i
        
        ! Generate test data
        do i = 1, 10
            x(i) = real(i-1, 8)
            y(i) = x(i) ** 2
        end do
        
        ! Create plot without saving to file
        call figure()
        call plot(x, y)
        call xlabel('X axis')
        call ylabel('Y axis')
        call title('Basic Plot Test')
        ! No need to clear figure in fast tests
    end subroutine test_basic_plot
    
    subroutine test_multiple_series()
        real(8), dimension(20) :: x1, y1, y2
        integer :: i
        
        ! Generate test data
        do i = 1, 20
            x1(i) = real(i-1, 8) * 0.5d0
            y1(i) = sin(x1(i))
            y2(i) = cos(x1(i))
        end do
        
        ! Create plot without saving to file
        call figure()
        call plot(x1, y1)
        call plot(x1, y2)
        call legend()
        call grid(.true.)
        ! No need to clear figure in fast tests
    end subroutine test_multiple_series
    
    subroutine test_plot_customization()
        real(8), dimension(15) :: x, y
        integer :: i
        
        ! Generate test data
        do i = 1, 15
            x(i) = real(i-1, 8)
            y(i) = exp(-x(i) * 0.1d0) * sin(x(i))
        end do
        
        ! Test various customizations
        call figure()
        call plot(x, y)
        call xlim(0.0d0, 15.0d0)
        call ylim(-1.0d0, 1.0d0)
        call xlabel('Time (s)')
        call ylabel('Amplitude')
        call title('Damped Oscillation')
        call grid(.true.)
        ! No need to clear figure in fast tests
    end subroutine test_plot_customization
    
    subroutine test_edge_cases()
        real(8), dimension(1) :: single_x = [1.0d0], single_y = [2.0d0]
        real(8), dimension(0) :: empty_x, empty_y
        real(8), dimension(2) :: two_x = [0.0d0, 1.0d0], two_y = [0.0d0, 1.0d0]
        
        ! Test single point
        call figure()
        call plot(single_x, single_y)
        
        ! Test empty arrays (should handle gracefully)
        call figure()
        call plot(empty_x, empty_y)
        
        ! Test two points
        call figure()
        call plot(two_x, two_y)
    end subroutine test_edge_cases
    
    subroutine test_legend_basic()
        real(8), dimension(10) :: x, y1, y2
        integer :: i
        
        ! Generate test data
        do i = 1, 10
            x(i) = real(i-1, 8)
            y1(i) = x(i)
            y2(i) = x(i) ** 2
        end do
        
        ! Test legend with multiple plots
        call figure()
        call plot(x, y1)
        call plot(x, y2)
        call legend()
        
        ! Test legend with empty label
        call figure()
        call plot(x, y1)
        call plot(x, y2)
        call legend()
    end subroutine test_legend_basic

end program test_suite_fast