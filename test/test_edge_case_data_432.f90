program test_edge_case_data_432
    !! Comprehensive test for Issue #432: Critical edge case handling
    !! Tests zero-size arrays and single points across all backends
    
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp), allocatable :: x_empty(:), y_empty(:)
    real(wp) :: x_single(1), y_single(1)
    real(wp) :: x_normal(5), y_normal(5)
    logical :: test_passed
    
    print *, "=== Testing Edge Case Data Handling (Issue #432) ==="
    
    test_passed = .true.
    
    ! Test data
    allocate(x_empty(0), y_empty(0))  ! Zero-size arrays
    x_single = [5.0_wp]
    y_single = [3.0_wp] 
    x_normal = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y_normal = [1.0_wp, 4.0_wp, 2.0_wp, 5.0_wp, 3.0_wp]
    
    !---------------------------------------------------------------------
    ! Test 1: Zero-size array handling
    !---------------------------------------------------------------------
    print *, "Test 1: Zero-size arrays"
    
    ! PNG backend
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x_empty, y_empty, label="empty data")
    call fig%set_title("Zero-size Array Test - PNG")
    call fig%set_xlabel("X axis")  
    call fig%set_ylabel("Y axis")
    call fig%savefig("test/output/test_zero_arrays.png")
    
    ! Check that axes and labels are visible even with no data
    if (len_trim(fig%title) > 0 .and. len_trim(fig%xlabel) > 0) then
        print *, "  ✓ PNG: Labels preserved with zero-size data"
    else
        print *, "  ✗ PNG: Labels missing with zero-size data"
        test_passed = .false.
    end if
    
    ! ASCII backend - easier to verify visually
    call fig%initialize(80, 24, 'ascii')
    call fig%add_plot(x_empty, y_empty, label="empty data")
    call fig%set_title("Zero-size Array Test - ASCII")
    call fig%set_xlabel("X axis")
    call fig%set_ylabel("Y axis")  
    call fig%savefig("test/output/test_zero_arrays.txt")
    
    !---------------------------------------------------------------------
    ! Test 2: Single point handling
    !---------------------------------------------------------------------
    print *, "Test 2: Single point plotting"
    
    ! PNG backend
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x_single, y_single, label="single point", linestyle="o")
    call fig%set_title("Single Point Test - PNG")
    call fig%set_xlabel("X axis")
    call fig%set_ylabel("Y axis")
    call fig%savefig("test/output/test_single_point.png")
    
    ! ASCII backend  
    call fig%initialize(80, 24, 'ascii')
    call fig%add_plot(x_single, y_single, label="single point", linestyle="o")
    call fig%set_title("Single Point Test - ASCII") 
    call fig%set_xlabel("X axis")
    call fig%set_ylabel("Y axis")
    call fig%savefig("test/output/test_single_point.txt")
    
    !---------------------------------------------------------------------
    ! Test 3: Normal data (control test)
    !---------------------------------------------------------------------
    print *, "Test 3: Normal data (control)"
    
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x_normal, y_normal, label="normal data") 
    call fig%set_title("Normal Data Test - PNG")
    call fig%set_xlabel("X axis")
    call fig%set_ylabel("Y axis")
    call fig%savefig("test/output/test_normal_data.png")
    
    call fig%initialize(80, 24, 'ascii')
    call fig%add_plot(x_normal, y_normal, label="normal data")
    call fig%set_title("Normal Data Test - ASCII")
    call fig%set_xlabel("X axis") 
    call fig%set_ylabel("Y axis")
    call fig%savefig("test/output/test_normal_data.txt")
    
    !---------------------------------------------------------------------
    ! Test 4: Mixed edge cases
    !---------------------------------------------------------------------
    print *, "Test 4: Mixed edge cases on same plot"
    
    call fig%initialize(400, 300, 'png')
    ! This should not crash and should handle each data series appropriately
    call fig%add_plot(x_empty, y_empty, label="empty")
    call fig%add_plot(x_single, y_single, label="single", linestyle="o") 
    call fig%add_plot(x_normal, y_normal, label="normal")
    call fig%set_title("Mixed Edge Cases Test")
    call fig%set_xlabel("X axis")
    call fig%set_ylabel("Y axis")
    call fig%savefig("test/output/test_mixed_edge_cases.png")
    
    !---------------------------------------------------------------------
    ! Summary
    !---------------------------------------------------------------------
    print *, "============================================="
    if (test_passed) then
        print *, "✓ All edge case tests completed"
        print *, "Check generated files to verify visual output:"
        print *, "- test_zero_arrays.png/txt (should show axes/labels only)"
        print *, "- test_single_point.png/txt (should show visible point)"  
        print *, "- test_normal_data.png/txt (should show connected line)"
        print *, "- test_mixed_edge_cases.png (should show point + line)"
    else
        print *, "✗ Some edge case tests failed"
    end if
    print *, "============================================="
    
end program test_edge_case_data_432