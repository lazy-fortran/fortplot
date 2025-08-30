program test_backend_switching_debug
    !! Simplified test to isolate backend switching segfault
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(3), y(3)
    integer :: i
    
    print *, "Initializing test data..."
    do i = 1, 3
        x(i) = real(i-1, wp)
        y(i) = x(i)
    end do
    
    print *, "Test 1: Initialize with PDF backend"
    call fig%initialize(80, 24, backend='pdf')
    print *, "  PDF backend initialized successfully"
    
    print *, "Test 2: Add simple plot data"
    call fig%add_plot(x, y)
    print *, "  Plot data added successfully"
    
    print *, "Test 3: Save as PNG (should trigger backend switch)"
    call fig%savefig("test_debug_switch.png")
    print *, "  Backend switching completed successfully"
    
    print *, "SUCCESS: No segmentation fault detected"
end program test_backend_switching_debug