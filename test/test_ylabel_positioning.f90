program test_ylabel_positioning
    !! Debug Y-axis label rendering step by step
    use fortplot_text, only: init_text_system, render_text_to_image, calculate_text_width, calculate_text_height
    use fortplot_bitmap, only: initialize_white_background
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer, parameter :: test_width = 100, test_height = 20
    integer(1), allocatable :: test_buffer(:)
    integer :: text_width, text_height, padding, buf_width, buf_height
    integer :: i, non_white_pixels
    character(len=*), parameter :: test_text = "Y Label"
    
    ! Initialize text system
    if (.not. init_text_system()) then
        print *, "ERROR: Failed to initialize text system"
        stop 1
    end if
    
    print *, "=== Debug Y-Axis Label Step by Step ==="
    
    ! Step 1: Test basic text dimensions
    text_width = calculate_text_width(test_text)
    text_height = calculate_text_height(test_text)
    print *, "Text '", test_text, "' dimensions: ", text_width, "x", text_height
    
    ! Step 2: Create buffer and render text
    padding = 4
    buf_width = text_width + 2 * padding
    buf_height = text_height + 2 * padding
    print *, "Buffer dimensions: ", buf_width, "x", buf_height
    print *, "Buffer size needed: ", buf_height * (1 + buf_width * 3), "bytes"
    
    allocate(test_buffer(buf_height * (1 + buf_width * 3)))
    ! Use the same initialization as the PNG module
    call initialize_white_background(test_buffer, buf_width, buf_height)
    
    ! Step 3: Render text to buffer
    print *, "Rendering text to buffer at position (", padding, ",", padding, ")"
    call render_text_to_image(test_buffer, buf_width, buf_height, &
                             padding, padding, test_text, &
                             0_1, 0_1, 0_1)  ! Black text
    
    ! Step 4: Count non-white pixels in buffer
    non_white_pixels = 0
    do i = 2, buf_height * (1 + buf_width * 3) - 2, 3
        ! Skip row byte, check RGB values (white is -1_1 which is 255 unsigned)
        if (test_buffer(i) /= -1_1 .or. test_buffer(i+1) /= -1_1 .or. test_buffer(i+2) /= -1_1) then
            non_white_pixels = non_white_pixels + 1
        end if
    end do
    
    print *, "Non-white pixels in text buffer: ", non_white_pixels
    
    if (non_white_pixels == 0) then
        print *, "FAIL: No text was rendered to the buffer!"
        print *, "This indicates render_text_to_image is not working properly"
        stop 1
    else
        print *, "PASS: Text was rendered to buffer successfully"
    end if
    
    ! Step 5: Test buffer format
    print *, "Buffer format check:"
    print *, "  Expected white pixel: -1, -1, -1"
    print *, "  First few pixels:"
    do i = 2, min(30, buf_height * (1 + buf_width * 3) - 2), 3
        if (i <= 5) then
            print *, "    Pixel", (i-2)/3 + 1, ":", int(test_buffer(i)), int(test_buffer(i+1)), int(test_buffer(i+2))
        end if
    end do
    
    deallocate(test_buffer)
    print *, ""
    print *, "Basic text rendering test completed"
    
end program test_ylabel_positioning