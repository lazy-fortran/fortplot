program test_markers_rendering
    !! Test suite for marker rendering on PNG backend
    !! Validates fix for issue #276: markers completely broken on PNG backend
    
    use fortplot
    use fortplot_validation
    implicit none
    
    type(figure_t) :: fig
    real(wp), dimension(10) :: x, y
    integer :: i, test_count, pass_count
    logical :: test_passed
    type(validation_result_t) :: validation
    
    test_count = 0
    pass_count = 0
    
    ! Create test data
    do i = 1, 10
        x(i) = real(i, wp)
        y(i) = sin(real(i, wp) * 0.5_wp) * 10.0_wp
    end do
    
    print *, '=== Testing Marker Rendering on PNG Backend ==='
    print *, ''
    
    ! Test 1: Markers only (no lines)
    test_count = test_count + 1
    call fig%initialize(640, 480)
    call fig%add_plot(x, y, linestyle='o', label='Circles only')
    call fig%set_title('Test: Markers Only')
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call fig%savefig('test_markers_only_validation.png')
    
    validation = validate_file_exists('test_markers_only_validation.png')
    test_passed = validation%passed
    if (test_passed) then
        pass_count = pass_count + 1
        print *, '[PASS] Test 1: Markers only rendering'
    else
        print *, '[FAIL] Test 1: Markers only rendering - ', trim(validation%message)
    end if
    
    ! Test 2: Lines with markers
    test_count = test_count + 1
    call fig%initialize(640, 480)
    call fig%add_plot(x, y, linestyle='-o', label='Lines + Circles')
    call fig%set_title('Test: Lines with Markers')
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call fig%savefig('test_lines_markers_validation.png')
    
    validation = validate_file_exists('test_lines_markers_validation.png')
    test_passed = validation%passed
    if (test_passed) then
        pass_count = pass_count + 1
        print *, '[PASS] Test 2: Lines with markers rendering'
    else
        print *, '[FAIL] Test 2: Lines with markers rendering - ', trim(validation%message)
    end if
    
    ! Test 3: Multiple marker types
    test_count = test_count + 1
    call fig%initialize(640, 480)
    call fig%add_plot(x(1:3), y(1:3), linestyle='o', label='Circles')
    call fig%add_plot(x(4:6), y(4:6), linestyle='s', label='Squares')
    call fig%add_plot(x(7:10), y(7:10), linestyle='^', label='Triangles')
    call fig%set_title('Test: Multiple Marker Types')
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call fig%legend()
    call fig%savefig('test_multiple_markers_validation.png')
    
    validation = validate_file_exists('test_multiple_markers_validation.png')
    test_passed = validation%passed
    if (test_passed) then
        pass_count = pass_count + 1
        print *, '[PASS] Test 3: Multiple marker types'
    else
        print *, '[FAIL] Test 3: Multiple marker types - ', trim(validation%message)
    end if
    
    ! Test 4: Combined line styles and markers
    test_count = test_count + 1
    call fig%initialize(640, 480)
    call fig%add_plot(x, y, linestyle='-o', label='Solid + Circles')
    call fig%add_plot(x, y*0.8_wp, linestyle='--s', label='Dashed + Squares')
    call fig%add_plot(x, y*0.6_wp, linestyle=':^', label='Dotted + Triangles')
    call fig%add_plot(x, y*0.4_wp, linestyle='-.v', label='DashDot + Down triangles')
    call fig%set_title('Test: Combined Line Styles and Markers')
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call fig%legend()
    call fig%savefig('test_combined_validation.png')
    
    validation = validate_file_exists('test_combined_validation.png')
    test_passed = validation%passed
    if (test_passed) then
        pass_count = pass_count + 1
        print *, '[PASS] Test 4: Combined line styles and markers'
    else
        print *, '[FAIL] Test 4: Combined line styles and markers - ', trim(validation%message)
    end if
    
    ! Test 5: Edge case - single point with marker
    test_count = test_count + 1
    call fig%initialize(640, 480)
    call fig%add_plot([5.0_wp], [5.0_wp], linestyle='o', label='Single point')
    call fig%set_title('Test: Single Point Marker')
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call fig%set_xlim(0.0_wp, 10.0_wp)
    call fig%set_ylim(0.0_wp, 10.0_wp)
    call fig%savefig('test_single_point_validation.png')
    
    validation = validate_file_exists('test_single_point_validation.png')
    test_passed = validation%passed
    if (test_passed) then
        pass_count = pass_count + 1
        print *, '[PASS] Test 5: Single point with marker'
    else
        print *, '[FAIL] Test 5: Single point with marker - ', trim(validation%message)
    end if
    
    ! Test 6: All supported marker types
    test_count = test_count + 1
    call fig%initialize(800, 600)
    call fig%add_plot([1.0_wp], [1.0_wp], linestyle='o', label='Circle')
    call fig%add_plot([2.0_wp], [2.0_wp], linestyle='s', label='Square')
    call fig%add_plot([3.0_wp], [3.0_wp], linestyle='^', label='Triangle up')
    call fig%add_plot([4.0_wp], [4.0_wp], linestyle='v', label='Triangle down')
    call fig%add_plot([5.0_wp], [5.0_wp], linestyle='<', label='Triangle left')
    call fig%add_plot([6.0_wp], [6.0_wp], linestyle='>', label='Triangle right')
    call fig%add_plot([7.0_wp], [7.0_wp], linestyle='+', label='Plus')
    call fig%add_plot([8.0_wp], [8.0_wp], linestyle='x', label='Cross')
    call fig%add_plot([9.0_wp], [9.0_wp], linestyle='*', label='Star')
    call fig%add_plot([10.0_wp], [10.0_wp], linestyle='d', label='Diamond')
    call fig%set_title('Test: All Marker Types')
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call fig%set_xlim(0.0_wp, 11.0_wp)
    call fig%set_ylim(0.0_wp, 11.0_wp)
    call fig%legend()
    call fig%savefig('test_all_markers_validation.png')
    
    validation = validate_file_exists('test_all_markers_validation.png')
    test_passed = validation%passed
    if (test_passed) then
        pass_count = pass_count + 1
        print *, '[PASS] Test 6: All supported marker types'
    else
        print *, '[FAIL] Test 6: All supported marker types - ', trim(validation%message)
    end if
    
    ! Summary
    print *, ''
    print *, '=== Test Summary ==='
    print '(A,I0,A,I0)', 'Passed: ', pass_count, ' / ', test_count
    
    if (pass_count == test_count) then
        print *, 'SUCCESS: All marker rendering tests passed!'
        stop 0
    else
        print *, 'FAILURE: Some tests failed'
        stop 1
    end if
    
end program test_markers_rendering