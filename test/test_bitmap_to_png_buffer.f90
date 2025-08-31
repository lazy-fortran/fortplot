program test_bitmap_to_png_buffer
    use fortplot_text, only: init_text_system, cleanup_text_system, calculate_text_width, calculate_text_height
    use fortplot_bitmap, only: render_text_to_bitmap, rotate_bitmap_90_cw
    use fortplot_png_encoder, only: bitmap_to_png_buffer
    use fortplot_png, only: write_png_file
    use fortplot_png_validation, only: validate_png_file
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer(1), allocatable :: bitmap(:,:,:), rotated_bitmap(:,:,:)
    integer(1), allocatable :: png_buffer(:), rotated_png_buffer(:)
    integer :: width, height, text_width, text_height, padding
    character(len=*), parameter :: test_text = "PNG Test"
    
    ! Initialize text system
    if (.not. init_text_system()) then
        print *, "ERROR: Failed to initialize text system"
        stop 1
    end if
    
    ! Calculate text dimensions and create bitmap
    text_width = calculate_text_width(test_text)
    text_height = calculate_text_height(test_text)
    padding = 5
    
    width = text_width + 2 * padding
    height = text_height + 2 * padding
    
    print *, "Creating bitmaps for '", test_text, "'"
    print *, "  Original size: ", width, "x", height
    
    ! Create and render original bitmap
    allocate(bitmap(width, height, 3))
    bitmap = -1_1  ! White background
    call render_text_to_bitmap(bitmap, width, height, padding, padding + text_height, test_text)
    
    ! Create rotated bitmap
    allocate(rotated_bitmap(height, width, 3))
    call rotate_bitmap_90_cw(bitmap, rotated_bitmap, width, height)
    
    print *, "  Rotated size:  ", height, "x", width
    
    ! Convert original bitmap to PNG buffer
    allocate(png_buffer(height * (1 + width * 3)))
    call bitmap_to_png_buffer(bitmap, width, height, png_buffer)
    
    ! Convert rotated bitmap to PNG buffer  
    allocate(rotated_png_buffer(width * (1 + height * 3)))
    call bitmap_to_png_buffer(rotated_bitmap, height, width, rotated_png_buffer)
    
    ! Validate PNG buffers
    call validate_png_buffer(png_buffer, width, height, "original")
    call validate_png_buffer(rotated_png_buffer, height, width, "rotated")
    
    ! Test the PNG buffer format compatibility
    call test_buffer_roundtrip(bitmap, png_buffer, width, height, "original")
    call test_buffer_roundtrip(rotated_bitmap, rotated_png_buffer, height, width, "rotated")
    
    ! Write actual PNG files using the real PNG writer
    call write_png_file("test_bitmap_original.png", width, height, png_buffer)
    call validate_png_file("test_bitmap_original.png", "Bitmap PNG test - original")
    call write_png_file("test_bitmap_rotated.png", height, width, rotated_png_buffer)
    call validate_png_file("test_bitmap_rotated.png", "Bitmap PNG test - rotated")
    
    print *, "SUCCESS: PNG buffer conversion tests passed"
    print *, "  bitmap_to_png_buffer format validated"
    print *, "  Created test_bitmap_original.png and test_bitmap_rotated.png"
    print *, "  Rotation and conversion pipeline working correctly"
    
    deallocate(bitmap, rotated_bitmap, png_buffer, rotated_png_buffer)
    call cleanup_text_system()
    
contains

    subroutine validate_png_buffer(buffer, width, height, label)
        !! Validate PNG buffer format and content
        integer(1), intent(in) :: buffer(:)
        integer, intent(in) :: width, height
        character(len=*), intent(in) :: label
        integer :: expected_size, actual_size, i, j, idx
        integer :: text_pixels, row_start
        logical :: has_filter_bytes, has_text_data
        
        ! Test 1: Buffer size
        expected_size = height * (1 + width * 3)
        actual_size = size(buffer)
        
        if (actual_size /= expected_size) then
            print *, "ERROR: ", label, " buffer size mismatch - Expected:", expected_size, " Actual:", actual_size
            stop 1
        end if
        print *, "PASS: ", label, " buffer size correct (", actual_size, " bytes)"
        
        ! Test 2: PNG row format (filter byte + RGB data)
        has_filter_bytes = .true.
        do j = 1, height
            row_start = (j - 1) * (1 + width * 3) + 1
            ! PNG filter byte should be 0 for no filter (as set by initialize_white_background)
            if (buffer(row_start) /= 0_1) then
                has_filter_bytes = .false.
                exit
            end if
        end do
        
        if (.not. has_filter_bytes) then
            print *, "ERROR: ", label, " missing proper PNG filter bytes"
            stop 1
        end if
        print *, "PASS: ", label, " PNG filter bytes correct"
        
        ! Test 3: Content validation (check for non-white pixels indicating text)
        text_pixels = 0
        has_text_data = .false.
        
        do j = 1, height
            do i = 1, width
                idx = (j - 1) * (1 + width * 3) + 1 + (i - 1) * 3 + 1
                ! Check if RGB pixel is not white (-1,-1,-1)
                if (buffer(idx) /= -1_1 .or. buffer(idx+1) /= -1_1 .or. buffer(idx+2) /= -1_1) then
                    text_pixels = text_pixels + 1
                    has_text_data = .true.
                end if
            end do
        end do
        
        if (.not. has_text_data) then
            print *, "ERROR: ", label, " buffer contains no text data"
            stop 1
        end if
        print *, "PASS: ", label, " contains text data (", text_pixels, " text pixels)"
        
    end subroutine validate_png_buffer

    subroutine test_buffer_roundtrip(original_bitmap, png_buffer, width, height, label)
        !! Test that PNG buffer accurately represents the original bitmap
        integer(1), intent(in) :: original_bitmap(:,:,:), png_buffer(:)
        integer, intent(in) :: width, height
        character(len=*), intent(in) :: label
        integer :: i, j, buf_idx
        integer(1) :: orig_pixel(3), buffer_pixel(3)
        logical :: pixels_match
        integer :: mismatched_pixels
        
        mismatched_pixels = 0
        pixels_match = .true.
        
        ! Compare each pixel between bitmap and PNG buffer
        do j = 1, height
            do i = 1, width
                ! Get pixel from original bitmap
                orig_pixel = original_bitmap(i, j, :)
                
                ! Get corresponding pixel from PNG buffer
                buf_idx = (j - 1) * (1 + width * 3) + 1 + (i - 1) * 3 + 1
                buffer_pixel(1) = png_buffer(buf_idx)     ! R
                buffer_pixel(2) = png_buffer(buf_idx + 1) ! G
                buffer_pixel(3) = png_buffer(buf_idx + 2) ! B
                
                ! Check if pixels match
                if (.not. (orig_pixel(1) == buffer_pixel(1) .and. &
                          orig_pixel(2) == buffer_pixel(2) .and. &
                          orig_pixel(3) == buffer_pixel(3))) then
                    mismatched_pixels = mismatched_pixels + 1
                    pixels_match = .false.
                end if
            end do
        end do
        
        if (.not. pixels_match) then
            print *, "ERROR: ", label, " roundtrip failed - ", mismatched_pixels, " pixels don't match"
            stop 1
        end if
        
        print *, "PASS: ", label, " bitmap->PNG buffer roundtrip perfect match"
    end subroutine test_buffer_roundtrip
    
end program test_bitmap_to_png_buffer