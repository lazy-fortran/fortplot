program test_png_ylabel_integration
    !! Integration test for PNG y-axis label rotation
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(10), y(10)
    integer :: i
    logical :: test_passed
    
    ! Generate test data
    do i = 1, 10
        x(i) = real(i, wp)
        y(i) = real(i, wp)**2
    end do
    
    ! Create figure with y-axis label using functional API
    call figure()
    call plot(x, y, 'b-')
    call ylabel('Temperature (°C)')
    call xlabel('Time (s)')
    call title('Y-axis Label Rotation Test')
    
    ! Save to PNG file
    call savefig('test_ylabel_rotation.png')
    
    ! Check that file was created
    inquire(file='test_ylabel_rotation.png', exist=test_passed)
    
    if (test_passed) then
        print *, "PASS: PNG with rotated y-label created successfully"
        print *, "      Check test_ylabel_rotation.png to verify visual quality"
        stop 0
    else
        print *, "FAIL: Failed to create PNG with rotated y-label"
        stop 1
    end if
    
end program test_png_ylabel_integration