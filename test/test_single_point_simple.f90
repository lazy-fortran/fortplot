program test_single_point_simple
    !! Simplified test for Issue #436 - single point plotting failures
    !! This test demonstrates the core issue without complex validation
    
    use fortplot
    use fortplot_test_helpers, only: test_initialize_figure, test_savefig, test_get_temp_path, test_cleanup_all
    use iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x_single(1), y_single(1)
    real(wp) :: x_multi(3), y_multi(3)
    logical :: file_exists
    
    print *, "Testing single point plotting (Issue #436)..."
    print *, "============================================="
    
    ! Test data
    x_single = [1.0_wp]
    y_single = [2.0_wp]
    
    x_multi = [1.0_wp, 2.0_wp, 3.0_wp]
    y_multi = [1.0_wp, 2.0_wp, 1.5_wp]
    
    ! Test 1: Single point PNG
    print *, "Test 1: Single point PNG..."
    call test_initialize_figure(fig, 400, 300, 'png')
    call fig%add_plot(x_single, y_single, label="single point")
    call test_savefig(fig, 'single_point_test.png')
    
    inquire(file=test_get_temp_path('single_point_test.png'), exist=file_exists)
    if (file_exists) then
        print *, "  ✓ PNG file created (but may be empty due to bug)"
    else
        print *, "  ✗ PNG file not created - CRITICAL FAILURE"
    end if
    
    ! Test 2: Single point PDF
    print *, "Test 2: Single point PDF..."
    call test_initialize_figure(fig, 400, 300, 'pdf')
    call fig%add_plot(x_single, y_single, label="single point")
    call test_savefig(fig, 'single_point_test.pdf')
    
    inquire(file=test_get_temp_path('single_point_test.pdf'), exist=file_exists)
    if (file_exists) then
        print *, "  ✓ PDF file created (but may be empty due to bug)"
    else
        print *, "  ✗ PDF file not created - CRITICAL FAILURE"
    end if
    
    ! Test 3: Single point ASCII
    print *, "Test 3: Single point ASCII..."
    call test_initialize_figure(fig, 80, 24, 'ascii')
    call fig%add_plot(x_single, y_single, label="single point")
    call test_savefig(fig, 'single_point_test.txt')
    
    inquire(file=test_get_temp_path('single_point_test.txt'), exist=file_exists)
    if (file_exists) then
        print *, "  ✓ ASCII file created (but likely shows no point due to bug)"
    else
        print *, "  ✗ ASCII file not created - CRITICAL FAILURE"
    end if
    
    ! Test 4: Comparison with multi-point (this should work)
    print *, "Test 4: Multi-point comparison..."
    call test_initialize_figure(fig, 400, 300, 'png')
    call fig%add_plot(x_multi, y_multi, label="multi points")
    call test_savefig(fig, 'multi_point_test.png')
    
    inquire(file=test_get_temp_path('multi_point_test.png'), exist=file_exists)
    if (file_exists) then
        print *, "  ✓ Multi-point PNG created (this should work fine)"
    else
        print *, "  ✗ Multi-point PNG failed - UNEXPECTED"
    end if
    
    print *, "============================================="
    print *, "Test completed. Issue #436 confirmed if single point plots are empty."
    print *, "Generated test files in temporary directory:"
    print *, "- single_point_test.png/pdf/txt (should show one point but likely empty)"
    print *, "- multi_point_test.png (should show connected line, works fine)"
    
    ! Clean up test files
    call test_cleanup_all()

end program test_single_point_simple