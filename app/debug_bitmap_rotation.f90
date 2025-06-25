program debug_bitmap_rotation
    use fortplot_text, only: init_text_system, cleanup_text_system
    use fortplot_raster, only: render_text_to_bitmap, rotate_bitmap_90_cw, bitmap_to_png_buffer
    use fortplot_raster, only: initialize_white_background
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer(1), allocatable :: text_bitmap(:,:,:), rotated_bitmap(:,:,:)
    integer(1), allocatable :: text_buffer(:), rotated_buffer(:)
    integer :: width, height, i, j
    character(len=*), parameter :: test_text = "Y Label"
    
    ! Initialize text system
    if (.not. init_text_system()) then
        print *, "Error: Failed to initialize text system"
        stop 1
    end if
    
    ! Create small test bitmap
    width = 100
    height = 30
    
    print *, "=== Testing Bitmap Text Rendering ==="
    
    ! Test 1: Direct bitmap rendering
    allocate(text_bitmap(width, height, 3))
    text_bitmap = -1_1  ! White background
    
    call render_text_to_bitmap(text_bitmap, width, height, 5, 20, test_text)
    
    ! Save as PNG to inspect
    allocate(text_buffer(height * (1 + width * 3)))
    call bitmap_to_png_buffer(text_bitmap, width, height, text_buffer)
    
    ! Write original text bitmap (simulate PNG writing)
    print *, "Original text bitmap created"
    
    ! Test 2: Rotation
    allocate(rotated_bitmap(height, width, 3))
    call rotate_bitmap_90_cw(text_bitmap, rotated_bitmap, width, height)
    
    ! Save rotated version
    allocate(rotated_buffer(width * (1 + height * 3)))
    call bitmap_to_png_buffer(rotated_bitmap, height, width, rotated_buffer)
    
    print *, "Rotated bitmap created"
    
    ! Test 3: Check specific pixels
    print *, "=== Pixel Analysis ==="
    print *, "Original bitmap corner pixels:"
    print *, "  (1,1):", text_bitmap(1,1,:)
    print *, "  (10,10):", text_bitmap(10,10,:)
    
    print *, "Rotated bitmap corner pixels:"
    print *, "  (1,1):", rotated_bitmap(1,1,:)
    print *, "  (10,10):", rotated_bitmap(10,10,:)
    
    ! Test 4: Check for non-white pixels (text pixels)
    print *, "=== Text Detection ==="
    print *, "Original bitmap - counting non-white pixels:"
    call count_text_pixels(text_bitmap, width, height, "original")
    
    print *, "Rotated bitmap - counting non-white pixels:"  
    call count_text_pixels(rotated_bitmap, height, width, "rotated")
    
    deallocate(text_bitmap, rotated_bitmap, text_buffer, rotated_buffer)
    call cleanup_text_system()
    
contains
    
    subroutine count_text_pixels(bitmap, w, h, label)
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: w, h
        character(len=*), intent(in) :: label
        integer :: i, j, count
        
        count = 0
        do j = 1, h
            do i = 1, w
                ! Check if pixel is not white (not all components = -1)
                if (bitmap(i,j,1) /= -1_1 .or. bitmap(i,j,2) /= -1_1 .or. bitmap(i,j,3) /= -1_1) then
                    count = count + 1
                end if
            end do
        end do
        
        print *, "  ", label, " bitmap: ", count, " text pixels out of ", w*h, " total"
    end subroutine count_text_pixels
    
end program debug_bitmap_rotation