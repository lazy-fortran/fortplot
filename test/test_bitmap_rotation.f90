program test_bitmap_rotation
    use fortplot_text, only: init_text_system, cleanup_text_system, calculate_text_width, calculate_text_height
    use fortplot_raster, only: render_text_to_bitmap, rotate_bitmap_90_cw
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer(1), allocatable :: bitmap(:,:,:), rotated_bitmap(:,:,:)
    integer :: width, height, text_width, text_height, padding
    character(len=*), parameter :: test_text = "Test"
    
    ! Initialize text system
    if (.not. init_text_system()) then
        print *, "ERROR: Failed to initialize text system"
        stop 1
    end if
    
    ! Calculate text dimensions and create minimal bitmap
    text_width = calculate_text_width(test_text)
    text_height = calculate_text_height(test_text)
    padding = 3
    
    width = text_width + 2 * padding
    height = text_height + 2 * padding
    
    allocate(bitmap(width, height, 3))
    bitmap = -1_1  ! White background
    
    ! Render text to bitmap
    call render_text_to_bitmap(bitmap, width, height, padding, padding + text_height, test_text)
    
    ! Test rotation
    allocate(rotated_bitmap(height, width, 3))
    call rotate_bitmap_90_cw(bitmap, rotated_bitmap, width, height)
    
    ! Validate rotation properties
    call test_rotation_properties(bitmap, rotated_bitmap, width, height)
    
    print *, "SUCCESS: Bitmap rotation tests passed"
    
    deallocate(bitmap, rotated_bitmap)
    call cleanup_text_system()
    
contains

    subroutine test_rotation_properties(original, rotated, orig_width, orig_height)
        !! Test that rotation preserves pixels and swaps dimensions correctly
        integer(1), intent(in) :: original(:,:,:), rotated(:,:,:)
        integer, intent(in) :: orig_width, orig_height
        integer :: i, j, orig_pixels, rot_pixels
        integer(1) :: orig_pixel(3), rot_pixel(3)
        logical :: corner_test_passed
        
        ! Test 1: Pixel count preservation
        orig_pixels = count_non_white_pixels(original, orig_width, orig_height)
        rot_pixels = count_non_white_pixels(rotated, orig_height, orig_width)
        
        if (orig_pixels /= rot_pixels) then
            print *, "ERROR: Pixel count mismatch - Original:", orig_pixels, " Rotated:", rot_pixels
            stop 1
        end if
        print *, "PASS: Pixel count preserved (", orig_pixels, " pixels)"
        
        ! Test 2: Dimension swap
        if (size(rotated, 1) /= orig_height .or. size(rotated, 2) /= orig_width) then
            print *, "ERROR: Dimensions not swapped correctly"
            stop 1
        end if
        print *, "PASS: Dimensions swapped correctly (", orig_width, "x", orig_height, " -> ", orig_height, "x", orig_width, ")"
        
        ! Test 3: Corner pixel transformation test
        ! 90° clockwise rotation: (x,y) -> (y, width-x+1)
        corner_test_passed = .true.
        
        ! Test a few specific pixel transformations
        if (orig_width >= 3 .and. orig_height >= 3) then
            ! Top-left corner (1,1) -> (1, width)
            orig_pixel = original(1, 1, :)
            rot_pixel = rotated(1, orig_width, :)
            if (.not. pixels_equal(orig_pixel, rot_pixel)) then
                corner_test_passed = .false.
            end if
            
            ! Top-right corner (width,1) -> (1, 1)  
            orig_pixel = original(orig_width, 1, :)
            rot_pixel = rotated(1, 1, :)
            if (.not. pixels_equal(orig_pixel, rot_pixel)) then
                corner_test_passed = .false.
            end if
            
            ! Bottom-left corner (1,height) -> (height, width)
            orig_pixel = original(1, orig_height, :)
            rot_pixel = rotated(orig_height, orig_width, :)
            if (.not. pixels_equal(orig_pixel, rot_pixel)) then
                corner_test_passed = .false.
            end if
        end if
        
        if (.not. corner_test_passed) then
            print *, "ERROR: Corner pixel transformation failed"
            stop 1
        end if
        print *, "PASS: Corner pixel transformations correct"
        
        ! Test 4: White pixel preservation
        if (has_white_pixels(original, orig_width, orig_height) .neqv. &
            has_white_pixels(rotated, orig_height, orig_width)) then
            print *, "ERROR: White pixel pattern not preserved"
            stop 1
        end if
        print *, "PASS: White pixel pattern preserved"
        
    end subroutine test_rotation_properties
    
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
    
    function has_white_pixels(bitmap, width, height) result(has_white)
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: width, height
        logical :: has_white
        integer :: i, j
        
        has_white = .false.
        do j = 1, height
            do i = 1, width
                if (bitmap(i,j,1) == -1_1 .and. bitmap(i,j,2) == -1_1 .and. bitmap(i,j,3) == -1_1) then
                    has_white = .true.
                    return
                end if
            end do
        end do
    end function has_white_pixels
    
    function pixels_equal(pixel1, pixel2) result(equal)
        integer(1), intent(in) :: pixel1(3), pixel2(3)
        logical :: equal
        
        equal = (pixel1(1) == pixel2(1) .and. pixel1(2) == pixel2(2) .and. pixel1(3) == pixel2(3))
    end function pixels_equal
    
end program test_bitmap_rotation