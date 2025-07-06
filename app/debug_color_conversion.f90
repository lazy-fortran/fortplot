program debug_color_conversion
    use fortplot_jpeg
    implicit none
    
    integer :: i
    real :: r, g, b, y, cb, cr
    
    print *, "Testing RGB to YCbCr conversion"
    print *, "STB formula: Y = 0.299*R + 0.587*G + 0.114*B - 128"
    print *, ""
    
    ! Test some specific values
    do i = 1, 5
        select case(i)
        case(1)
            r = 255; g = 0; b = 0  ! Red
        case(2)
            r = 0; g = 255; b = 0  ! Green
        case(3)
            r = 0; g = 0; b = 255  ! Blue
        case(4)
            r = 128; g = 128; b = 128  ! Gray
        case(5)
            r = 255; g = 255; b = 255  ! White
        end select
        
        ! STB conversion
        y = 0.299 * r + 0.587 * g + 0.114 * b - 128
        cb = -0.16874 * r - 0.33126 * g + 0.50000 * b
        cr = 0.50000 * r - 0.41869 * g - 0.08131 * b
        
        print '("RGB(", F5.0, ",", F5.0, ",", F5.0, ") -> Y=", F7.2, " Cb=", F7.2, " Cr=", F7.2)', &
              r, g, b, y, cb, cr
    end do
    
    print *, ""
    print *, "Check: For gray (128,128,128), Y should be 0.0"
    print *, "Check: For white (255,255,255), Y should be 127.0"
    
end program