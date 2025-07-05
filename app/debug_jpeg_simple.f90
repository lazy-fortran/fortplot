program debug_jpeg_simple
    use fortplot_jpeg
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    integer(1) :: pixels(3, 64, 64)  ! RGB image
    integer :: x, y, i
    integer(1), parameter :: white = -1_int8  ! 255 as signed int8
    integer(1), parameter :: black = 0_int8
    
    ! Create a simple gradient pattern
    do y = 1, 64
        do x = 1, 64
            ! Red channel - horizontal gradient
            pixels(1, x, y) = int(min(127, x * 2), int8)
            ! Green channel - vertical gradient  
            pixels(2, x, y) = int(min(127, y * 2), int8)
            ! Blue channel - diagonal gradient
            pixels(3, x, y) = int(min(127, x + y), int8)
        end do
    end do
    
    ! Write with different qualities
    call write_jpeg_file("debug_gradient_q50.jpg", 64, 64, reshape(pixels, [3*64*64]), 50)
    call write_jpeg_file("debug_gradient_q80.jpg", 64, 64, reshape(pixels, [3*64*64]), 80)
    call write_jpeg_file("debug_gradient_q95.jpg", 64, 64, reshape(pixels, [3*64*64]), 95)
    
    ! Create a simple checkerboard pattern
    do y = 1, 64
        do x = 1, 64
            if (mod(x/8 + y/8, 2) == 0) then
                pixels(:, x, y) = white  ! White (255)
            else
                pixels(:, x, y) = black   ! Black (0)
            end if
        end do
    end do
    
    call write_jpeg_file("debug_checkerboard.jpg", 64, 64, reshape(pixels, [3*64*64]), 90)
    
    print *, "Debug JPEG files created!"
    
end program debug_jpeg_simple