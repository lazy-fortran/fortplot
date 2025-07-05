program debug_scan_content
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    integer(1) :: gray_data(8*8*3)
    integer :: i
    
    ! Create different test patterns
    
    ! Test 1: Solid gray
    do i = 1, 8*8*3, 3
        gray_data(i) = int(127, 1)      ! R
        gray_data(i+1) = int(127, 1)    ! G
        gray_data(i+2) = int(127, 1)    ! B
    end do
    call write_jpeg_file("test_solid_gray.jpg", 8, 8, gray_data, 90)
    
    ! Test 2: Gradient
    do i = 0, 63
        gray_data(i*3+1) = int(i*4, 1)    ! R gradient
        gray_data(i*3+2) = int(i*4, 1)    ! G gradient
        gray_data(i*3+3) = int(i*4, 1)    ! B gradient
    end do
    call write_jpeg_file("test_gradient.jpg", 8, 8, gray_data, 90)
    
    ! Test 3: Checkerboard
    do i = 0, 63
        if (mod(i/8 + mod(i,8), 2) == 0) then
            gray_data(i*3+1) = 0_int8      ! Black
            gray_data(i*3+2) = 0_int8
            gray_data(i*3+3) = 0_int8
        else
            gray_data(i*3+1) = -1_int8     ! White (255)
            gray_data(i*3+2) = -1_int8
            gray_data(i*3+3) = -1_int8
        end if
    end do
    call write_jpeg_file("test_checkerboard.jpg", 8, 8, gray_data, 90)
    
    print *, "Created test patterns:"
    print *, "- test_solid_gray.jpg (should be uniform gray)"
    print *, "- test_gradient.jpg (should show gradient)"
    print *, "- test_checkerboard.jpg (should show checkerboard)"
    
end program debug_scan_content