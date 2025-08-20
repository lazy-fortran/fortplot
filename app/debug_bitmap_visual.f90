program debug_bitmap_visual
    !! Debug program to visually inspect character bitmaps
    use fortplot_raster, only: render_text_to_bitmap
    use fortplot_text, only: init_text_system, cleanup_text_system, calculate_text_width, calculate_text_height
    implicit none
    
    integer(1), allocatable :: bitmap(:,:,:)
    integer :: width, height, text_width, text_height, padding
    character(len=1) :: test_char = 'A'
    character(len=1) :: char_str
    integer :: i, j
    
    if (.not. init_text_system()) then
        print *, "ERROR: Failed to initialize text system"
        stop 1
    end if
    
    char_str = test_char
    
    ! Calculate required bitmap size
    text_width = calculate_text_width(char_str)
    text_height = calculate_text_height(char_str)
    padding = 5
    
    width = text_width + 2 * padding
    height = text_height + 2 * padding
    
    print *, "=== Visual Bitmap Debug ==="
    print *, "Character: '", test_char, "'"
    print *, "Text dimensions:", text_width, "x", text_height
    print *, "Bitmap size:", width, "x", height
    print *, "Rendering at position:", padding, ",", padding + text_height
    print *, ""
    
    ! Create and render to bitmap
    allocate(bitmap(width, height, 3))
    bitmap = -1_1  ! White background
    
    call render_text_to_bitmap(bitmap, width, height, padding, padding + text_height, char_str)
    
    ! Print bitmap as ASCII art
    print *, "Bitmap visualization (X=ink, .=white):"
    print *, "+"
    do i = 1, width+2
        write(*, '(A)', advance='no') "-"
    end do
    print *, "+"
    
    do j = 1, height
        write(*, '(A)', advance='no') "|"
        do i = 1, width
            if (bitmap(i,j,1) /= -1_1 .or. bitmap(i,j,2) /= -1_1 .or. bitmap(i,j,3) /= -1_1) then
                write(*, '(A)', advance='no') "X"
            else
                write(*, '(A)', advance='no') "."
            end if
        end do
        write(*, '(A)') "|"
    end do
    
    write(*, '(A)', advance='no') "+"
    do i = 1, width+2
        write(*, '(A)', advance='no') "-"
    end do
    print *, "+"
    
    ! Count pixels
    print *, ""
    print *, "Pixel analysis:"
    print *, "  Non-white pixels:", count_non_white_pixels()
    print *, "  Total pixels:", width * height
    
    deallocate(bitmap)
    call cleanup_text_system()
    
contains

    function count_non_white_pixels() result(count)
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

end program debug_bitmap_visual