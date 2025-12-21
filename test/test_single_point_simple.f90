program test_single_point_simple
    !! Simplified test for Issue #436 - single point plotting failures
    !! This test demonstrates the core issue without complex validation

    use fortplot
    use iso_fortran_env, only: wp => real64
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    type(figure_t) :: fig
    real(wp) :: x_single(1), y_single(1)
    real(wp) :: x_multi(3), y_multi(3)
    logical :: file_exists
    character(len=:), allocatable :: output_dir

    call ensure_test_output_dir('single_point_simple', output_dir)
    
    print *, "Testing single point plotting (Issue #436)..."
    print *, "============================================="
    
    ! Test data
    x_single = [1.0_wp]
    y_single = [2.0_wp]
    
    x_multi = [1.0_wp, 2.0_wp, 3.0_wp]
    y_multi = [1.0_wp, 2.0_wp, 1.5_wp]
    
    ! Test 1: Single point PNG
    print *, "Test 1: Single point PNG..."
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x_single, y_single, label="single point")
    call fig%savefig(trim(output_dir)//'single_point_test.png')

    inquire(file=trim(output_dir)//'single_point_test.png', exist=file_exists)
    if (file_exists) then
        print *, "  PASS: PNG file created (but may be empty due to bug)"
    else
        print *, "  FAIL: PNG file not created - CRITICAL FAILURE"
    end if
    
    ! Test 2: Single point PDF
    print *, "Test 2: Single point PDF..."
    call fig%initialize(400, 300, 'pdf')
    call fig%add_plot(x_single, y_single, label="single point")
    call fig%savefig(trim(output_dir)//'single_point_test.pdf')

    inquire(file=trim(output_dir)//'single_point_test.pdf', exist=file_exists)
    if (file_exists) then
        print *, "  PASS: PDF file created (but may be empty due to bug)"
    else
        print *, "  FAIL: PDF file not created - CRITICAL FAILURE"
    end if
    
    ! Test 3: Single point ASCII
    print *, "Test 3: Single point ASCII..."
    call fig%initialize(80, 24, 'ascii')
    call fig%add_plot(x_single, y_single, label="single point")
    call fig%savefig(trim(output_dir)//'single_point_test.txt')

    inquire(file=trim(output_dir)//'single_point_test.txt', exist=file_exists)
    if (file_exists) then
        print *, "  PASS: ASCII file created (but likely shows no point due to bug)"
    else
        print *, "  FAIL: ASCII file not created - CRITICAL FAILURE"
    end if
    
    ! Test 4: Comparison with multi-point (this should work)
    print *, "Test 4: Multi-point comparison..."
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x_multi, y_multi, label="multi points")
    call fig%savefig(trim(output_dir)//'multi_point_test.png')

    inquire(file=trim(output_dir)//'multi_point_test.png', exist=file_exists)
    if (file_exists) then
        print *, "  PASS: Multi-point PNG created (this should work fine)"
    else
        print *, "  FAIL: Multi-point PNG failed - UNEXPECTED"
    end if
    
    print *, "============================================="
    print *, "Test completed. Issue #436 confirmed if single point plots are empty."
    print *, "Generated test files in temporary directory:"
    print *, "- single_point_test.(png|pdf|txt) stored under ", trim(output_dir)
    print *, "  Expected: should show one point but likely empty"
    print *, "- multi_point_test.png stored under ", trim(output_dir)
    print *, "  Expected: shows connected line and works correctly"
    
    ! No cleanup: leave artifacts for inspection

end program test_single_point_simple
