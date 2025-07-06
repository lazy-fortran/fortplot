program debug_jpeg_visual
    use iso_c_binding
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    interface
        function stbi_write_jpg(filename, w, h, comp, data, quality) &
                bind(C, name="stbi_write_jpg")
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int), value :: w, h, comp, quality
            type(c_ptr), value :: data
            integer(c_int) :: stbi_write_jpg
        end function
    end interface
    
    integer, parameter :: width = 16, height = 16, quality = 85
    integer(1), target :: image_data(width * height * 3)
    integer(1), allocatable :: our_jpeg(:)
    real :: ycbcr_data(width, height, 3)
    integer :: x, y, idx, result
    real :: r, g, b
    character(len=100) :: filename
    
    print *, "Debug JPEG Visual Test - 16x16 gradient"
    print *, "======================================="
    
    ! Create a simple gradient pattern
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            
            ! Simple red-green gradient
            r = real(x) * 255.0 / real(width - 1)
            g = real(y) * 255.0 / real(height - 1)  
            b = 0.0
            
            image_data(idx) = int(r, 1)
            image_data(idx+1) = int(g, 1)
            image_data(idx+2) = int(b, 1)
        end do
    end do
    
    ! Write STB reference
    filename = "debug_stb_gradient.jpg" // c_null_char
    result = stbi_write_jpg(filename, width, height, 3, c_loc(image_data), quality)
    print *, "STB JPEG written"
    
    ! Convert to YCbCr and trace
    print *, ""
    print *, "Converting to YCbCr..."
    call convert_rgb_to_ycbcr(image_data, width, height, ycbcr_data)
    
    ! Check first few pixels
    print *, "First pixel RGB:", image_data(1:3)
    print *, "First pixel YCbCr:", ycbcr_data(1,1,:)
    print *, ""
    print *, "Last pixel RGB:", image_data(width*height*3-2:width*height*3)
    print *, "Last pixel YCbCr:", ycbcr_data(width,height,:)
    
    ! Create our JPEG
    call get_jpeg_data(width, height, image_data, quality, our_jpeg)
    
    ! Write our output
    open(unit=10, file="debug_our_gradient.jpg", access='stream', form='unformatted')
    write(10) our_jpeg
    close(10)
    
    print *, ""
    print *, "Our JPEG size:", size(our_jpeg), "bytes"
    print *, "Files written: debug_stb_gradient.jpg and debug_our_gradient.jpg"
    
contains

    subroutine convert_rgb_to_ycbcr(rgb_data, width, height, ycbcr_data)
        integer(1), intent(in) :: rgb_data(:)
        integer, intent(in) :: width, height
        real, intent(out) :: ycbcr_data(:,:,:)
        
        integer :: x, y, idx
        real :: r, g, b
        
        do y = 1, height
            do x = 1, width
                idx = ((y-1) * width + (x-1)) * 3 + 1
                
                ! Get RGB values (0-255)
                r = real(iand(int(rgb_data(idx)), 255))
                g = real(iand(int(rgb_data(idx+1)), 255))
                b = real(iand(int(rgb_data(idx+2)), 255))
                
                ! STB YCbCr conversion
                ycbcr_data(x, y, 1) = 0.29900 * r + 0.58700 * g + 0.11400 * b - 128.0
                ycbcr_data(x, y, 2) = -0.16874 * r - 0.33126 * g + 0.50000 * b
                ycbcr_data(x, y, 3) = 0.50000 * r - 0.41869 * g - 0.08131 * b
            end do
        end do
    end subroutine

end program