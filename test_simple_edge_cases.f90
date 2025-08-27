program test_simple_edge_cases
    !! Simple test using matplotlib interface for Issue #432
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp), allocatable :: x_empty(:), y_empty(:)
    real(wp) :: x_single(1), y_single(1)
    
    print *, "Testing simple edge cases..."
    
    ! Test 1: Zero-size arrays
    allocate(x_empty(0), y_empty(0))
    print *, "Zero-size array test"
    call figure()
    call plot(x_empty, y_empty)
    call savefig('test_zero_matplotlib.png')
    
    ! Test 2: Single point
    x_single = [5.0_wp]
    y_single = [3.0_wp]
    print *, "Single point test"
    call figure()
    call plot(x_single, y_single)
    call savefig('test_single_matplotlib.png')
    
    print *, "Tests completed - check output files"
    
end program test_simple_edge_cases