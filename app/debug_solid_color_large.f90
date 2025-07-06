program debug_solid_color_large
    use fortplot_jpeg
    implicit none
    
    integer, parameter :: width = 64, height = 64
    integer(1) :: image_data(width * height * 3)
    integer :: i
    
    ! Fill with solid red
    do i = 1, width * height * 3, 3
        image_data(i) = int(-1, 1)     ! R = 255
        image_data(i + 1) = int(0, 1)  ! G = 0
        image_data(i + 2) = int(0, 1)  ! B = 0
    end do
    
    print *, "Creating 64x64 solid red image with quality 85..."
    call write_jpeg_file("debug_solid_red.jpg", width, height, image_data, 85)
    
end program