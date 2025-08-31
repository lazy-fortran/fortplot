program test_png_overflow
    !! Test PNG dimension overflow handling with automatic validation
    use fortplot_context, only: plot_context
    use fortplot_matplotlib, only: figure, savefig, plot
    use fortplot_utils, only: initialize_backend
    use fortplot_png_validation, only: validate_png_file
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(100), y(100)
    integer :: i
    logical :: file_exists
    
    print *, "Testing PNG dimension overflow handling..."
    
    ! Generate test data
    do i = 1, 100
        x(i) = real(i - 1, wp) * 0.1_wp
        y(i) = sin(x(i))
    end do
    
    ! Test 1: Normal dimensions (8x6 inches * 100 dpi = 800x600 pixels)
    print *, "Test 1: Normal dimensions (8x6)"
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot(x, y)
    call savefig("test/output/test_normal.png")
    call validate_png_file("test/output/test_normal.png", "PNG overflow test - normal dimensions")
    print *, "  Normal PNG saved and validated successfully"
    
    ! Test 2: Large pixel values mistakenly used as inches
    ! This would create 64000x48000 pixels and should trigger overflow
    print *, "Test 2: Large dimensions (640x480 - likely meant as pixels)"
    call figure(figsize=[640.0_wp, 480.0_wp])
    call plot(x, y)
    call savefig("test/output/test_overflow.png")
    call validate_png_file("test/output/test_overflow.png", "PNG overflow test - large dimensions")
    print *, "  Large dimension file saved and validated (may be PDF fallback)"
    
    ! Test 3: Edge case at validation boundary (50x50 inches * 100 = 5000x5000 pixels)
    print *, "Test 3: Edge case dimensions (50x50)"
    call figure(figsize=[50.0_wp, 50.0_wp])
    call plot(x, y)
    call savefig("test/output/test_edge.png")
    call validate_png_file("test/output/test_edge.png", "PNG overflow test - edge case dimensions")
    print *, "  Edge case file saved and validated (may be PDF fallback)"
    
    ! Test 4: Just over validation boundary (51x51 inches * 100 = 5100x5100 pixels)
    print *, "Test 4: Over boundary dimensions (51x51)"
    call figure(figsize=[51.0_wp, 51.0_wp])
    call plot(x, y)
    call savefig("test/output/test_over.png")
    call validate_png_file("test/output/test_over.png", "PNG overflow test - over boundary dimensions")
    print *, "  Over boundary file handled and validated (may be PDF fallback)"
    
    print *, "All tests completed!"
    
end program test_png_overflow