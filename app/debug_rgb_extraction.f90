program debug_rgb_extraction
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    integer(1) :: rgb_data(3)
    real :: r, g, b
    
    ! Test 1: Gray value 127
    rgb_data = int(127, 1)
    
    ! Extract using our method
    r = real(iand(int(rgb_data(1)), 255))
    g = real(iand(int(rgb_data(2)), 255))
    b = real(iand(int(rgb_data(3)), 255))
    
    print *, "Test 1: int8 value 127"
    print *, "Extracted RGB:", r, g, b
    
    ! Test 2: White (255)
    rgb_data = int(-1, 1)  ! -1 in int8 = 255 unsigned
    
    r = real(iand(int(rgb_data(1)), 255))
    g = real(iand(int(rgb_data(2)), 255))
    b = real(iand(int(rgb_data(3)), 255))
    
    print *, ""
    print *, "Test 2: int8 value -1 (255)"
    print *, "Extracted RGB:", r, g, b
    
    ! Test 3: What if we just use int() without iand?
    rgb_data = int(127, 1)
    r = real(int(rgb_data(1)))
    g = real(int(rgb_data(2)))
    b = real(int(rgb_data(3)))
    
    print *, ""
    print *, "Test 3: Direct int() of 127"
    print *, "Without iand:", r, g, b
    
    ! The issue might be if rgb_data contains negative values
    ! when it should contain positive values
    
end program debug_rgb_extraction