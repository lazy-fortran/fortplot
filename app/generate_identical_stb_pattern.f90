program generate_identical_stb_pattern
    use stb_image_write_wrapper, only: stb_write_jpeg_wrapper
    use, intrinsic :: iso_fortran_env, only: int8
    use iso_c_binding
    implicit none
    
    integer(1), target :: pixels(3, 16, 16)  ! RGB image data
    integer :: x, y
    integer(1), parameter :: white = -1_int8  ! 255 as signed int8
    integer(1), parameter :: black = 0_int8
    integer(1), parameter :: gray = 127_int8
    
    ! Create identical test pattern to our implementation
    do y = 1, 16
        do x = 1, 16
            if (y <= 8) then
                if (x <= 8) then
                    ! Top-left: white
                    pixels(:, x, y) = white
                else
                    ! Top-right: black
                    pixels(:, x, y) = black
                end if
            else
                if (x <= 8) then
                    ! Bottom-left: red
                    pixels(1, x, y) = white
                    pixels(2:3, x, y) = black
                else
                    ! Bottom-right: gray
                    pixels(:, x, y) = gray
                end if
            end if
        end do
    end do
    
    ! Write with STB at quality=95 to match our settings
    print *, "Writing with STB at quality=95..."
    if (stb_write_jpeg_wrapper("stb_pattern_q95.jpg"//c_null_char, 16, 16, 3, c_loc(pixels), 95) == 1) then
        print *, "STB JPEG file 'stb_pattern_q95.jpg' created successfully!"
    else
        print *, "Failed to create STB JPEG file"
    end if
    
end program