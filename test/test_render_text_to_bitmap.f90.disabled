program test_render_text_to_bitmap
    use fortplot_text, only: init_text_system, cleanup_text_system, calculate_text_width, calculate_text_height
    use fortplot_raster, only: render_text_to_bitmap, rotate_bitmap_90_cw
    use fortplot_bmp, only: write_bmp_file
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer(1), allocatable :: bitmap(:,:,:), rotated_bitmap(:,:,:)
    integer :: width, height, text_width, text_height, padding
    character(len=*), parameter :: test_text = "Hello BMP!"
    
    ! Initialize text system
    if (.not. init_text_system()) then
        print *, "ERROR: Failed to initialize text system"
        stop 1
    end if
    
    ! Calculate actual text dimensions
    text_width = calculate_text_width(test_text)
    text_height = calculate_text_height(test_text)
    padding = 5
    
    print *, "Text dimensions:"
    print *, "  Width: ", text_width, " pixels"
    print *, "  Height:", text_height, " pixels"
    
    ! Create bitmap sized to fit text with padding
    width = text_width + 2 * padding
    height = text_height + 2 * padding
    
    print *, "Bitmap dimensions:"
    print *, "  Width: ", width, " pixels"
    print *, "  Height:", height, " pixels"
    
    allocate(bitmap(width, height, 3))
    bitmap = -1_1  ! White background
    
    ! Render text to bitmap with padding
    call render_text_to_bitmap(bitmap, width, height, padding, padding + text_height, test_text)
    
    ! Validate the bitmap contains text pixels
    call validate_bitmap_has_text(bitmap, width, height, padding)
    
    ! Write original bitmap as BMP file
    call write_bmp_file("test_text_bitmap.bmp", bitmap, width, height)
    
    ! Test rotation: create rotated bitmap (90 degrees clockwise)
    allocate(rotated_bitmap(height, width, 3))
    call rotate_bitmap_90_cw(bitmap, rotated_bitmap, width, height)
    
    ! Validate the rotated bitmap
    call validate_rotated_bitmap(rotated_bitmap, height, width)
    
    ! Write rotated bitmap as BMP file
    call write_bmp_file("test_text_bitmap_rotated.bmp", rotated_bitmap, height, width)
    
    print *, "SUCCESS: All tests passed"
    print *, "  Created test_text_bitmap.bmp (", width, "x", height, ")"
    print *, "  Created test_text_bitmap_rotated.bmp (", height, "x", width, ")"
    print *, "  Text: '", test_text, "'"
    
    deallocate(bitmap, rotated_bitmap)
    call cleanup_text_system()
    
contains

    subroutine validate_bitmap_has_text(bitmap, width, height, padding)
        !! Validate that the bitmap contains non-white pixels (text)
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: width, height, padding
        integer :: i, j, text_pixels
        logical :: has_text_in_expected_area
        
        ! Count non-white pixels
        text_pixels = 0
        has_text_in_expected_area = .false.
        
        do j = 1, height
            do i = 1, width
                ! Check if pixel is not white (not all components = -1)
                if (bitmap(i,j,1) /= -1_1 .or. bitmap(i,j,2) /= -1_1 .or. bitmap(i,j,3) /= -1_1) then
                    text_pixels = text_pixels + 1
                    
                    ! Check if text appears in center area (exclude border padding)
                    if (i >= padding .and. i <= width - padding .and. j >= padding .and. j <= height - padding) then
                        has_text_in_expected_area = .true.
                    end if
                end if
            end do
        end do
        
        ! Validate results
        if (text_pixels == 0) then
            print *, "ERROR: No text pixels found in bitmap"
            stop 1
        end if
        
        if (.not. has_text_in_expected_area) then
            print *, "ERROR: No text found in expected area"
            stop 1
        end if
        
        if (text_pixels < 20) then
            print *, "ERROR: Too few text pixels (", text_pixels, "), expected at least 20"
            stop 1
        end if
        
        print *, "PASS: Found", text_pixels, "text pixels in expected area"
    end subroutine validate_bitmap_has_text

    subroutine validate_rotated_bitmap(bitmap, width, height)
        !! Validate that the rotated bitmap contains text pixels
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: width, height
        integer :: i, j, text_pixels
        logical :: has_text_in_rotated_area
        
        ! Count non-white pixels in rotated bitmap
        text_pixels = 0
        has_text_in_rotated_area = .false.
        
        do j = 1, height
            do i = 1, width
                ! Check if pixel is not white (not all components = -1)
                if (bitmap(i,j,1) /= -1_1 .or. bitmap(i,j,2) /= -1_1 .or. bitmap(i,j,3) /= -1_1) then
                    text_pixels = text_pixels + 1
                    
                    ! Just check if we have any text pixels (position doesn't matter for this test)
                    has_text_in_rotated_area = .true.
                end if
            end do
        end do
        
        ! Validate results
        if (text_pixels == 0) then
            print *, "ERROR: No text pixels found in rotated bitmap"
            stop 1
        end if
        
        if (.not. has_text_in_rotated_area) then
            print *, "ERROR: No text found in expected rotated area"
            stop 1
        end if
        
        if (text_pixels < 20) then
            print *, "ERROR: Too few text pixels in rotated bitmap (", text_pixels, "), expected at least 20"
            stop 1
        end if
        
        print *, "PASS: Found", text_pixels, "text pixels in rotated bitmap"
    end subroutine validate_rotated_bitmap
    
end program test_render_text_to_bitmap