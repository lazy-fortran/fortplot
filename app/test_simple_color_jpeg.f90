program test_simple_color_jpeg
    use fortplot_jpeg, only: write_jpeg_file
    implicit none
    
    integer, parameter :: width = 32, height = 32
    integer(1) :: image_data(width * height * 3)
    integer :: x, y, idx
    
    ! Create a simple color pattern
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            ! Red gradient horizontally
            image_data(idx) = int(255 * x / (width - 1), 1)
            ! Green gradient vertically
            image_data(idx + 1) = int(255 * y / (height - 1), 1)
            ! Blue fixed at half intensity
            image_data(idx + 2) = int(-128, 1)
        end do
    end do
    
    print *, "Creating test_color.jpg with", width, "x", height, "color gradient"
    call write_jpeg_file("test_color.jpg", width, height, image_data, 85)
    
    print *, "Test completed"
end program