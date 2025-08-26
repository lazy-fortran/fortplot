program test_ylabel_rotation_fix
    use fortplot
    implicit none
    
    real(8) :: x(100), y(100)
    integer :: i
    character(len=256) :: output_file
    
    ! Create test data
    do i = 1, 100
        x(i) = real(i-1) * 0.1
        y(i) = sin(x(i))
    end do
    
    ! Test PNG with ylabel rotation
    output_file = "test_ylabel_rotation.png"
    call figure()
    call plot(x, y)
    call title("Y-Label Rotation Test")
    call xlabel("X Axis")
    call ylabel("Y Axis (Should be rotated 90 degrees)")
    call savefig(output_file)
    
    print *, "Test PNG saved to: ", trim(output_file)
    print *, "Y-label should be rotated 90 degrees counter-clockwise"
    
end program test_ylabel_rotation_fix