program test_scatter_performance
    !! Test that scatter plot creates a single plot object for efficiency
    !! Validates the performance fix for the O(n) issue identified in PR #483
    
    use fortplot
    use fortplot_plot_data, only: PLOT_TYPE_SCATTER
    implicit none
    
    type(figure_t) :: fig
    real(wp), allocatable :: x(:), y(:), sizes(:), colors(:)
    integer :: i, n, plot_count_before, plot_count_after
    character(len=256) :: msg
    logical :: test_passed
    
    print *, "=== Scatter Plot Performance Test ==="
    
    ! Test 1: Basic scatter with many points
    n = 1000
    allocate(x(n), y(n))
    do i = 1, n
        x(i) = real(i, wp) / real(n, wp)
        y(i) = sin(10.0_wp * x(i))
    end do
    
    call fig%initialize(640, 480, 'png')
    plot_count_before = fig%get_plot_count()
    
    ! Add scatter plot
    call fig%scatter(x, y, label='Sine wave scatter')
    
    plot_count_after = fig%get_plot_count()
    
    ! Check that only ONE plot object was created, not n
    test_passed = (plot_count_after - plot_count_before == 1)
    
    write(msg, '(A,I0,A,I0,A)') &
        'Test 1: ', n, ' points created ', &
        (plot_count_after - plot_count_before), ' plot object(s)'
    
    if (test_passed) then
        print *, '  ✓ PASS: ', trim(msg), ' - Efficient single object'
    else
        print *, '  ✗ FAIL: ', trim(msg), ' - Should be 1 object!'
        stop 1
    end if
    
    ! Test 2: Scatter with variable sizes and colors
    allocate(sizes(n), colors(n))
    do i = 1, n
        sizes(i) = 5.0_wp + 15.0_wp * abs(sin(x(i) * 5.0_wp))
        colors(i) = real(i, wp) / real(n, wp)
    end do
    
    ! Reinitialize figure for next test
    call fig%initialize(640, 480, 'png')
    plot_count_before = fig%get_plot_count()
    
    call fig%scatter(x, y, s=sizes, c=colors, colormap='viridis', &
                    label='Variable size/color', show_colorbar=.true.)
    
    plot_count_after = fig%get_plot_count()
    test_passed = (plot_count_after - plot_count_before == 1)
    
    write(msg, '(A,I0,A,I0,A)') &
        'Test 2: ', n, ' points with colors created ', &
        (plot_count_after - plot_count_before), ' plot object(s)'
    
    if (test_passed) then
        print *, '  ✓ PASS: ', trim(msg), ' - Efficient single object'
    else
        print *, '  ✗ FAIL: ', trim(msg), ' - Should be 1 object!'
        stop 1
    end if
    
    ! Test 3: Verify plot type is set correctly
    if (allocated(fig%plots)) then
        if (fig%plots(1)%plot_type == PLOT_TYPE_SCATTER) then
            print *, '  ✓ PASS: Plot type correctly set to SCATTER'
        else
            print *, '  ✗ FAIL: Plot type not set to SCATTER'
            stop 1
        end if
        
        ! Verify data is stored properly
        if (allocated(fig%plots(1)%x) .and. allocated(fig%plots(1)%y)) then
            if (size(fig%plots(1)%x) == n .and. size(fig%plots(1)%y) == n) then
                print *, '  ✓ PASS: All data points stored in single plot object'
            else
                print *, '  ✗ FAIL: Data not stored properly'
                stop 1
            end if
        else
            print *, '  ✗ FAIL: Plot data arrays not allocated'
            stop 1
        end if
    else
        print *, '  ✗ FAIL: Plots array not allocated'
        stop 1
    end if
    
    ! Test 4: Performance test with very large dataset
    n = 10000
    deallocate(x, y, sizes, colors)
    allocate(x(n), y(n))
    
    do i = 1, n
        x(i) = real(i, wp) / real(n, wp)
        y(i) = cos(20.0_wp * x(i)) * exp(-x(i))
    end do
    
    ! Reinitialize for large dataset test
    call fig%initialize(640, 480, 'png')
    plot_count_before = fig%get_plot_count()
    
    call fig%scatter(x, y, markersize=2.0_wp, label='Large dataset')
    
    plot_count_after = fig%get_plot_count()
    test_passed = (plot_count_after - plot_count_before == 1)
    
    write(msg, '(A,I0,A,I0,A)') &
        'Test 4: ', n, ' points created ', &
        (plot_count_after - plot_count_before), ' plot object(s)'
    
    if (test_passed) then
        print *, '  ✓ PASS: ', trim(msg), ' - Scales to large datasets'
    else
        print *, '  ✗ FAIL: ', trim(msg), ' - Performance issue!'
        stop 1
    end if
    
    ! Save output for visual verification
    call fig%savefig("test/output/test_scatter_performance.png")
    
    print *, ''
    print *, '=== ALL SCATTER PERFORMANCE TESTS PASSED ==='
    print *, 'Scatter plot implementation is efficient (single plot object)'
    print *, 'Successfully handles thousands of points without O(n) overhead'
    
end program test_scatter_performance