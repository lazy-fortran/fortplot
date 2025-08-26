program test_new_modules_integration
    !! Test integration of all new modules (bitmap, PNG encoder, Unicode)
    use fortplot_bitmap, only: initialize_white_background, composite_image
    use fortplot_png_encoder, only: bitmap_to_png_buffer  
    use fortplot_unicode, only: escape_unicode_for_raster, unicode_codepoint_to_ascii
    implicit none
    
    integer(1), allocatable :: image_buffer(:), png_buffer(:)
    integer :: width, height, buffer_size
    character(len=50) :: unicode_text, escaped_text, ascii_result
    
    ! Test basic functionality of each new module
    print *, "Testing new module integration..."
    
    ! Test 1: Bitmap module
    width = 10
    height = 10  
    buffer_size = width * height * 3
    allocate(image_buffer(buffer_size))
    
    call initialize_white_background(image_buffer, width, height)
    
    ! Verify white background was created
    if (image_buffer(1) == -1_1 .and. image_buffer(2) == -1_1 .and. image_buffer(3) == -1_1) then
        print *, "PASS: Bitmap module - white background initialization"
    else
        print *, "FAIL: Bitmap module - white background initialization"
        stop 1
    end if
    
    ! Test 2: PNG Encoder module
    allocate(png_buffer(height * (1 + width * 3)))
    
    ! Create a simple 3D bitmap for testing
    block
        integer(1) :: test_bitmap(width, height, 3)
        test_bitmap = -1_1  ! White
        call bitmap_to_png_buffer(test_bitmap, width, height, png_buffer)
        
        ! Check PNG format - first byte of each row should be 0 (filter byte)
        if (png_buffer(1) == 0_1) then
            print *, "PASS: PNG Encoder module - buffer format"
        else
            print *, "FAIL: PNG Encoder module - buffer format"
            stop 1
        end if
    end block
    
    ! Test 3: Unicode module
    unicode_text = "Test string"
    call escape_unicode_for_raster(unicode_text, escaped_text)
    
    if (trim(escaped_text) == trim(unicode_text)) then
        print *, "PASS: Unicode module - text escape"
    else
        print *, "FAIL: Unicode module - text escape"
        stop 1
    end if
    
    ! Test Unicode to ASCII conversion
    call unicode_codepoint_to_ascii(945, ascii_result)  ! Greek alpha
    if (trim(ascii_result) == "alpha") then
        print *, "PASS: Unicode module - codepoint conversion"
    else
        print *, "FAIL: Unicode module - codepoint conversion, got: ", trim(ascii_result)
        stop 1
    end if
    
    print *, "SUCCESS: All new modules integrated correctly"
    
    deallocate(image_buffer, png_buffer)
    
end program test_new_modules_integration