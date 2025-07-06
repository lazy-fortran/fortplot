program debug_encode_issue
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    integer, parameter :: width = 32, height = 32  ! Smaller test
    integer(1) :: image_data(width * height * 3)
    integer :: x, y, idx
    
    ! Create a simple checkerboard pattern
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            if (mod(x/8 + y/8, 2) == 0) then
                ! White square
                image_data(idx:idx+2) = -1_int8  ! 255
            else
                ! Black square
                image_data(idx:idx+2) = 0_int8
            end if
        end do
    end do
    
    print *, "Creating 32x32 checkerboard pattern"
    print *, "With quality 85, this should have:"
    print *, "  MCUs: 2x2 = 4"
    print *, "  Blocks: 4 * 6 = 24"
    
    call write_jpeg_file("debug_checkerboard_32x32.jpg", width, height, image_data, 85)
    print *, "File written: debug_checkerboard_32x32.jpg"
    
    ! Try a tiny 16x16 image (single MCU)
    print *, ""
    print *, "Creating 16x16 single MCU test"
    call write_jpeg_file("debug_single_mcu.jpg", 16, 16, image_data(1:16*16*3), 85)
    print *, "File written: debug_single_mcu.jpg"
    
end program debug_encode_issue