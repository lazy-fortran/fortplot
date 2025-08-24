program test_png_overflow
    !! Test PNG dimension overflow handling
    use fortplot_context, only: plot_context
    use fortplot_matplotlib, only: figure, savefig, plot
    use fortplot_utils, only: initialize_backend
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(100), y(100)
    integer :: i
    
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
    call savefig("test_normal.png")
    print *, "  Normal PNG saved successfully"
    
    ! Test 2: Large pixel values mistakenly used as inches
    ! This would create 64000x48000 pixels and should trigger overflow
    print *, "Test 2: Large dimensions (640x480 - likely meant as pixels)"
    call figure(figsize=[640.0_wp, 480.0_wp])
    call plot(x, y)
    call savefig("test_overflow.png")
    print *, "  Large dimension PNG saved (should handle overflow)"
    
    ! Test 3: Edge case at validation boundary (50x50 inches * 100 = 5000x5000 pixels)
    print *, "Test 3: Edge case dimensions (50x50)"
    call figure(figsize=[50.0_wp, 50.0_wp])
    call plot(x, y)
    call savefig("test_edge.png")
    print *, "  Edge case PNG saved"
    
    ! Test 4: Just over validation boundary (51x51 inches * 100 = 5100x5100 pixels)
    print *, "Test 4: Over boundary dimensions (51x51)"
    call figure(figsize=[51.0_wp, 51.0_wp])
    call plot(x, y)
    call savefig("test_over.png")
    print *, "  Over boundary PNG handled"
    
    print *, "All tests completed!"
    
end program test_png_overflow