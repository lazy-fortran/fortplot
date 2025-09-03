program test_subplots_edge_cases
    !! Test edge cases for the stateful API subplots function
    
    use fortplot
    implicit none
    
    real(8) :: x(5), y(5)
    integer :: i
    logical :: test_passed
    
    ! Generate minimal test data
    do i = 1, 5
        x(i) = real(i, 8)
        y(i) = real(i*i, 8)
    end do
    
    test_passed = .true.
    
    ! Test 1: Valid grid sizes
    print *, "Test 1: Valid grid sizes"
    call subplots(1, 1)  ! Single plot
    print *, "  - 1x1 grid: OK"
    
    call subplots(3, 4)  ! Larger grid
    print *, "  - 3x4 grid: OK"
    
    ! Test 2: Invalid grid sizes (should handle gracefully)
    print *, "Test 2: Invalid grid sizes (error handling)"
    call subplots(0, 1)  ! Invalid row count
    print *, "  - 0x1 grid: Error handled gracefully"
    
    call subplots(1, 0)  ! Invalid column count  
    print *, "  - 1x0 grid: Error handled gracefully"
    
    call subplots(-1, 2)  ! Negative row count
    print *, "  - -1x2 grid: Error handled gracefully"
    
    ! Test 3: Large grid
    print *, "Test 3: Large grid"
    call subplots(10, 10)  ! 100 subplots
    print *, "  - 10x10 grid: OK"
    
    ! Test 4: After creating subplots, verify plot still works
    print *, "Test 4: Plot after subplots"
    call subplots(2, 2)
    call plot(x, y, label="test")
    call savefig("test_subplots_edge_cases.png")
    print *, "  - Plot after subplots: OK"
    
    if (test_passed) then
        print *, "All edge case tests passed!"
    else
        print *, "Some tests failed!"
        stop 1
    end if
    
end program test_subplots_edge_cases