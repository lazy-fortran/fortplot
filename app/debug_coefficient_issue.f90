program debug_coefficient_issue
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    integer, parameter :: width = 16, height = 16
    integer(1) :: image_data(width * height * 3)
    integer :: x, y, idx
    
    ! Create a simple pattern - each 8x8 block has a different color
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            
            ! Top-left block: red
            if (x < 8 .and. y < 8) then
                image_data(idx) = -1_int8     ! 255
                image_data(idx + 1) = 0_int8
                image_data(idx + 2) = 0_int8
            ! Top-right block: green
            else if (x >= 8 .and. y < 8) then
                image_data(idx) = 0_int8
                image_data(idx + 1) = -1_int8  ! 255
                image_data(idx + 2) = 0_int8
            ! Bottom-left block: blue
            else if (x < 8 .and. y >= 8) then
                image_data(idx) = 0_int8
                image_data(idx + 1) = 0_int8
                image_data(idx + 2) = -1_int8  ! 255
            ! Bottom-right block: yellow
            else
                image_data(idx) = -1_int8      ! 255
                image_data(idx + 1) = -1_int8  ! 255
                image_data(idx + 2) = 0_int8
            end if
        end do
    end do
    
    print *, "Creating 16x16 image with 4 colored blocks"
    print *, "Top-left: Red, Top-right: Green"
    print *, "Bottom-left: Blue, Bottom-right: Yellow"
    
    call write_jpeg_file("debug_16x16_colors.jpg", width, height, image_data, 85)
    print *, "File written: debug_16x16_colors.jpg"
    print *, ""
    print *, "With quality 85, this uses 4:2:0 subsampling"
    print *, "Should have 1 MCU with 4Y + 1U + 1V = 6 blocks"
    
end program debug_coefficient_issue