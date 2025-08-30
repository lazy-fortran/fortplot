program test_bitmap_rotation_both
    use fortplot_text, only: init_text_system, cleanup_text_system, calculate_text_width, calculate_text_height
    use fortplot_bitmap, only: render_text_to_bitmap, rotate_bitmap_90_cw, rotate_bitmap_90_ccw
    use fortplot_png_encoder, only: bitmap_to_png_buffer
    use fortplot_png, only: write_png_file
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer(1), allocatable :: bitmap(:,:,:), cw_bitmap(:,:,:), ccw_bitmap(:,:,:), full_rotation_bitmap(:,:,:)
    integer(1), allocatable :: png_buffer(:), cw_png_buffer(:), ccw_png_buffer(:), full_png_buffer(:)
    integer :: width, height, text_width, text_height, padding
    character(len=*), parameter :: test_text = "Rotate Me!"
    
    ! Initialize text system
    if (.not. init_text_system()) then
        print *, "ERROR: Failed to initialize text system"
        stop 1
    end if
    
    ! Calculate text dimensions and create bitmap
    text_width = calculate_text_width(test_text)
    text_height = calculate_text_height(test_text)
    padding = 8
    
    width = text_width + 2 * padding
    height = text_height + 2 * padding
    
    print *, "Testing both rotation directions for '", test_text, "'"
    print *, "  Original size: ", width, "x", height
    
    ! Create and render original bitmap
    allocate(bitmap(width, height, 3))
    bitmap = -1_1  ! White background
    call render_text_to_bitmap(bitmap, width, height, padding, padding + text_height, test_text)
    
    ! Test clockwise rotation
    allocate(cw_bitmap(height, width, 3))
    call rotate_bitmap_90_cw(bitmap, cw_bitmap, width, height)
    print *, "  Clockwise rotated: ", height, "x", width
    
    ! Test counter-clockwise rotation
    allocate(ccw_bitmap(height, width, 3))
    call rotate_bitmap_90_ccw(bitmap, ccw_bitmap, width, height)
    print *, "  Counter-clockwise rotated: ", height, "x", width
    
    ! Test full 180° rotation (CW then CW again, should equal CCW then CCW)
    allocate(full_rotation_bitmap(width, height, 3))
    call rotate_bitmap_90_cw(cw_bitmap, full_rotation_bitmap, height, width)
    print *, "  180° rotation (CW+CW): ", width, "x", height
    
    ! Validate rotation properties
    call validate_rotation_properties(bitmap, cw_bitmap, ccw_bitmap, full_rotation_bitmap, width, height)
    
    ! Convert to PNG buffers
    allocate(png_buffer(height * (1 + width * 3)))
    allocate(cw_png_buffer(width * (1 + height * 3)))
    allocate(ccw_png_buffer(width * (1 + height * 3)))
    allocate(full_png_buffer(height * (1 + width * 3)))
    
    call bitmap_to_png_buffer(bitmap, width, height, png_buffer)
    call bitmap_to_png_buffer(cw_bitmap, height, width, cw_png_buffer)
    call bitmap_to_png_buffer(ccw_bitmap, height, width, ccw_png_buffer)
    call bitmap_to_png_buffer(full_rotation_bitmap, width, height, full_png_buffer)
    
    ! Write PNG files
    call write_png_file("test/output/test_rotation_original.png", width, height, png_buffer)
    call write_png_file("test/output/test_rotation_clockwise.png", height, width, cw_png_buffer)
    call write_png_file("test/output/test_rotation_counter_clockwise.png", height, width, ccw_png_buffer)
    call write_png_file("test/output/test_rotation_180.png", width, height, full_png_buffer)
    
    print *, "SUCCESS: Both rotation directions working correctly"
    print *, "  Created test/output/test_rotation_original.png"
    print *, "  Created test/output/test_rotation_clockwise.png"
    print *, "  Created test/output/test_rotation_counter_clockwise.png"
    print *, "  Created test/output/test_rotation_180.png"
    
    deallocate(bitmap, cw_bitmap, ccw_bitmap, full_rotation_bitmap)
    deallocate(png_buffer, cw_png_buffer, ccw_png_buffer, full_png_buffer)
    call cleanup_text_system()
    
contains

    subroutine validate_rotation_properties(original, cw_rotated, ccw_rotated, rotated_180, orig_width, orig_height)
        !! Validate rotation properties for both directions
        integer(1), intent(in) :: original(:,:,:), cw_rotated(:,:,:), ccw_rotated(:,:,:), rotated_180(:,:,:)
        integer, intent(in) :: orig_width, orig_height
        integer :: orig_pixels, cw_pixels, ccw_pixels, rot180_pixels
        
        ! Test 1: Pixel count preservation in all rotations
        orig_pixels = count_non_white_pixels(original, orig_width, orig_height)
        cw_pixels = count_non_white_pixels(cw_rotated, orig_height, orig_width)
        ccw_pixels = count_non_white_pixels(ccw_rotated, orig_height, orig_width)
        rot180_pixels = count_non_white_pixels(rotated_180, orig_width, orig_height)
        
        if (orig_pixels /= cw_pixels .or. orig_pixels /= ccw_pixels .or. orig_pixels /= rot180_pixels) then
            print *, "ERROR: Pixel count not preserved across rotations"
            print *, "  Original:", orig_pixels, " CW:", cw_pixels, " CCW:", ccw_pixels, " 180°:", rot180_pixels
            stop 1
        end if
        print *, "PASS: Pixel count preserved in all rotations (", orig_pixels, " pixels)"
        
        ! Test 2: Dimension swapping for 90° rotations
        if (size(cw_rotated, 1) /= orig_height .or. size(cw_rotated, 2) /= orig_width) then
            print *, "ERROR: CW rotation dimensions incorrect"
            stop 1
        end if
        
        if (size(ccw_rotated, 1) /= orig_height .or. size(ccw_rotated, 2) /= orig_width) then
            print *, "ERROR: CCW rotation dimensions incorrect"
            stop 1
        end if
        print *, "PASS: 90° rotations swap dimensions correctly"
        
        ! Test 3: 180° rotation should preserve original dimensions
        if (size(rotated_180, 1) /= orig_width .or. size(rotated_180, 2) /= orig_height) then
            print *, "ERROR: 180° rotation dimensions incorrect"
            stop 1
        end if
        print *, "PASS: 180° rotation preserves original dimensions"
        
        ! Test 4: Corner pixel transformation test for CW rotation
        ! (x,y) -> (y, width-x+1) for clockwise
        if (orig_width >= 3 .and. orig_height >= 3) then
            ! Top-left (1,1) -> (1, width) for CW
            if (.not. pixels_equal(original(1, 1, :), cw_rotated(1, orig_width, :))) then
                print *, "ERROR: CW rotation corner transformation failed"
                stop 1
            end if
            
            ! Top-left (1,1) -> (height, 1) for CCW  
            if (.not. pixels_equal(original(1, 1, :), ccw_rotated(orig_height, 1, :))) then
                print *, "ERROR: CCW rotation corner transformation failed"
                stop 1
            end if
        end if
        print *, "PASS: Corner pixel transformations correct for both directions"
        
        ! Test 5: 180° rotation should flip image (bottom-right becomes top-left)
        if (orig_width >= 3 .and. orig_height >= 3) then
            if (.not. pixels_equal(original(orig_width, orig_height, :), rotated_180(1, 1, :))) then
                print *, "ERROR: 180° rotation corner transformation failed"
                stop 1
            end if
        end if
        print *, "PASS: 180° rotation transformation correct"
        
    end subroutine validate_rotation_properties
    
    function count_non_white_pixels(bitmap, width, height) result(count)
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: width, height
        integer :: count, i, j
        
        count = 0
        do j = 1, height
            do i = 1, width
                if (bitmap(i,j,1) /= -1_1 .or. bitmap(i,j,2) /= -1_1 .or. bitmap(i,j,3) /= -1_1) then
                    count = count + 1
                end if
            end do
        end do
    end function count_non_white_pixels
    
    function pixels_equal(pixel1, pixel2) result(equal)
        integer(1), intent(in) :: pixel1(3), pixel2(3)
        logical :: equal
        
        equal = (pixel1(1) == pixel2(1) .and. pixel1(2) == pixel2(2) .and. pixel1(3) == pixel2(3))
    end function pixels_equal
    
end program test_bitmap_rotation_both