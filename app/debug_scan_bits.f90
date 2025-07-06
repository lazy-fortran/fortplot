program debug_scan_bits
    use fortplot_jpeg, only: write_jpeg_file
    use stb_image_write_wrapper, only: stb_write_jpeg_wrapper
    use iso_c_binding, only: c_loc
    implicit none
    
    integer, parameter :: width = 8, height = 8
    integer(1), target :: gray_data(width * height * 3)
    integer :: i, j, pixel_idx
    
    ! Create simple uniform gray image (127,127,127)
    do i = 1, width * height
        pixel_idx = (i-1) * 3 + 1
        gray_data(pixel_idx) = 127_1      ! R
        gray_data(pixel_idx + 1) = 127_1  ! G  
        gray_data(pixel_idx + 2) = 127_1  ! B
    end do
    
    print *, "=== Creating 8x8 uniform gray test image ==="
    print *, "All pixels: (127,127,127)"
    print *, ""
    
    ! Generate STB reference
    if (stb_write_jpeg_wrapper("stb_debug.jpg"//char(0), width, height, 3, c_loc(gray_data), 90) == 1) then
        print *, "STB reference written: stb_debug.jpg"
    else
        print *, "ERROR: Failed to write STB reference"
        stop 1
    end if
    
    ! Generate our output
    call write_jpeg_file("our_debug.jpg", width, height, gray_data, 90)
    print *, "Our output written: our_debug.jpg"
    print *, ""
    
    ! Compare the scan data sections
    call system("hexdump -C stb_debug.jpg | grep -A3 'ff da' > stb_scan.hex")
    call system("hexdump -C our_debug.jpg | grep -A3 'ff da' > our_scan.hex")
    
    print *, "=== Scan data comparison ==="
    print *, "STB scan data (after FF DA):"
    call system("cat stb_scan.hex")
    print *, ""
    print *, "Our scan data (after FF DA):"
    call system("cat our_scan.hex")
    print *, ""
    
    ! Show first difference in scan data bytes
    call system("diff stb_scan.hex our_scan.hex | head -5")
    
end program debug_scan_bits