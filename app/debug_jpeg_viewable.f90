program debug_jpeg_viewable
    use fortplot_jpeg
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    integer(1), allocatable :: pixels(:)
    integer :: x, y, idx
    integer :: width = 256, height = 256
    
    allocate(pixels(3 * width * height))
    
    ! Create a recognizable pattern - color bars
    do y = 1, height
        do x = 1, width
            idx = ((y-1) * width + (x-1)) * 3 + 1
            
            if (x <= width/8) then
                ! White
                pixels(idx:idx+2) = -1_int8  ! 255
            else if (x <= 2*width/8) then
                ! Yellow (R+G)
                pixels(idx) = -1_int8     ! R=255
                pixels(idx+1) = -1_int8   ! G=255
                pixels(idx+2) = 0_int8    ! B=0
            else if (x <= 3*width/8) then
                ! Cyan (G+B)
                pixels(idx) = 0_int8      ! R=0
                pixels(idx+1) = -1_int8   ! G=255
                pixels(idx+2) = -1_int8   ! B=255
            else if (x <= 4*width/8) then
                ! Green
                pixels(idx) = 0_int8      ! R=0
                pixels(idx+1) = -1_int8   ! G=255
                pixels(idx+2) = 0_int8    ! B=0
            else if (x <= 5*width/8) then
                ! Magenta (R+B)
                pixels(idx) = -1_int8     ! R=255
                pixels(idx+1) = 0_int8    ! G=0
                pixels(idx+2) = -1_int8   ! B=255
            else if (x <= 6*width/8) then
                ! Red
                pixels(idx) = -1_int8     ! R=255
                pixels(idx+1) = 0_int8    ! G=0
                pixels(idx+2) = 0_int8    ! B=0
            else if (x <= 7*width/8) then
                ! Blue
                pixels(idx) = 0_int8      ! R=0
                pixels(idx+1) = 0_int8    ! G=0
                pixels(idx+2) = -1_int8   ! B=255
            else
                ! Black
                pixels(idx:idx+2) = 0_int8
            end if
        end do
    end do
    
    ! Write JPEG with various qualities
    print *, "Creating color bar test images..."
    call write_jpeg_file("color_bars_q50.jpg", width, height, pixels, 50)
    call write_jpeg_file("color_bars_q80.jpg", width, height, pixels, 80)
    call write_jpeg_file("color_bars_q95.jpg", width, height, pixels, 95)
    
    ! Create a simple gradient
    do y = 1, height
        do x = 1, width
            idx = ((y-1) * width + (x-1)) * 3 + 1
            pixels(idx) = int(iand(x, 255), int8)       ! R gradient
            pixels(idx+1) = int(iand(y, 255), int8)     ! G gradient
            pixels(idx+2) = int(iand(x+y, 255), int8)   ! B diagonal
        end do
    end do
    
    call write_jpeg_file("gradient_test.jpg", width, height, pixels, 85)
    
    print *, "Test images created!"
    print *, "Please view the generated JPEG files to check if they're readable"
    
end program debug_jpeg_viewable