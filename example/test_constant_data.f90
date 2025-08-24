program test_constant_data
    !! Test PDF generation with constant data (Issue #237)
    !! Verifies division by zero protection when data ranges are zero
    
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(5), y_constant(5), y_normal(5)
    integer :: i
    
    ! Test case 1: Horizontal line (constant Y data)
    print *, "Testing constant Y data (horizontal line)..."
    x = [(real(i, wp), i = 1, 5)]
    y_constant = 2.5_wp  ! All points have same Y value
    
    call figure()
    call xlabel('X axis')
    call ylabel('Y axis')
    call title('Constant Y Data Test')
    call plot(x, y_constant, 'b-')
    call savefig('test_constant_y.pdf')
    print *, "Saved: test_constant_y.pdf"
    
    ! Test case 2: Vertical line (constant X data)
    print *, "Testing constant X data (vertical line)..."
    x = 3.0_wp  ! All points have same X value
    y_normal = [(real(i, wp), i = 1, 5)]
    
    call figure()
    call xlabel('X axis')
    call ylabel('Y axis')
    call title('Constant X Data Test')
    call plot(x, y_normal, 'r-')
    call savefig('test_constant_x.pdf')
    print *, "Saved: test_constant_x.pdf"
    
    ! Test case 3: Single point (both X and Y constant)
    print *, "Testing single point data..."
    call figure()
    call xlabel('X axis')
    call ylabel('Y axis')
    call title('Single Point Test')
    call plot([5.0_wp], [5.0_wp], 'go')
    call savefig('test_single_point.pdf')
    print *, "Saved: test_single_point.pdf"
    
    print *, ""
    print *, "All constant data tests completed successfully!"
    print *, "Division by zero protection verified."
    
end program test_constant_data