program test_issue_430_debug
    !! Debug test for Issue #430 case to understand expected behavior
    
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x_coords(6), y_coords(5), c_data(5,4)
    integer :: i, j
    logical :: file_exists
    
    print *, "=== DEBUGGING ISSUE #430 CASE ==="
    print *, "Testing x(6), y(5), c(5,4)"
    print *, ""
    
    ! Initialize data exactly like Issue #430 test
    do i = 1, 6
        x_coords(i) = real(i-1, wp)
    end do
    do i = 1, 5  
        y_coords(i) = real(i-1, wp)
    end do
    do i = 1, 5
        do j = 1, 4
            c_data(i, j) = real(i * j, wp)
        end do
    end do
    
    print *, "Array setup:"
    print *, "  x_coords: ", size(x_coords), " elements"
    print *, "  y_coords: ", size(y_coords), " elements"  
    print *, "  c_data: ", size(c_data,1), "x", size(c_data,2)
    print *, ""
    
    print *, "Dimension analysis:"
    print *, "  If c_data(5,4) is Fortran z(ny,nx): ny=5, nx=4"
    print *, "    Expected: x(nx+1=5), y(ny+1=6)"
    print *, "    Actual: x(6), y(5) → MISMATCH"
    print *, ""
    print *, "  If c_data(5,4) is C-style z(nx,ny): nx=5, ny=4"  
    print *, "    Expected: x(nx+1=6), y(ny+1=5)"
    print *, "    Actual: x(6), y(5) → MATCH"
    print *, ""
    
    print *, "Testing with pcolormesh():"
    call figure()
    call pcolormesh(x_coords, y_coords, c_data)
    call savefig('test_issue_430_debug.png')
    
    inquire(file='test_issue_430_debug.png', exist=file_exists)
    
    if (file_exists) then
        print *, "✓ RESULT: PNG created successfully"
        print *, "✓ CONCLUSION: This case should work (C-style valid)"
        print *, "✓ Issue #430 test expectation may be wrong"
    else
        print *, "✗ RESULT: PNG not created"
        print *, "✗ CONCLUSION: True dimension error"
    end if
    
    print *, ""
    print *, "=== ISSUE #430 DEBUG COMPLETE ==="
    
end program test_issue_430_debug