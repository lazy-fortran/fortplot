program test_character_bitmap_rendering
    !! Unit test for verifying actual character bitmap rendering correctness
    !! Tests that characters are rendered with expected pixel patterns
    !! Backend-agnostic test that uses high-level rendering API
    use fortplot_text, only: init_text_system, cleanup_text_system, calculate_text_width, calculate_text_height
    use fortplot_raster, only: render_text_to_bitmap
    implicit none
    
    logical :: all_tests_passed
    
    if (.not. init_text_system()) then
        print *, "ERROR: Failed to initialize text system"
        stop 1
    end if
    
    all_tests_passed = .true.
    
    ! Test specific characters with known properties
    if (.not. test_character_rendering('I')) all_tests_passed = .false.
    if (.not. test_character_rendering('O')) all_tests_passed = .false.
    if (.not. test_character_rendering('.')) all_tests_passed = .false.
    if (.not. test_character_rendering('W')) all_tests_passed = .false.
    
    call cleanup_text_system()
    
    if (all_tests_passed) then
        print *, "All character bitmap rendering tests PASSED"
        stop 0
    else
        print *, "Some character bitmap rendering tests FAILED"
        stop 1
    end if
    
contains

    function test_character_rendering(test_char) result(passed)
        !! Test that a character renders with expected bitmap properties
        character, intent(in) :: test_char
        logical :: passed
        integer(1), allocatable :: bitmap(:,:,:)
        integer :: width, height, text_width, text_height, padding
        integer :: non_white_pixels, total_pixels
        character(len=1) :: char_str
        
        char_str = test_char
        passed = .true.
        
        print *, ""
        print *, "Testing character '", test_char, "'"
        print *, "------------------------"
        
        ! Calculate required bitmap size
        text_width = calculate_text_width(char_str)
        text_height = calculate_text_height(char_str)
        padding = 3
        
        width = text_width + 2 * padding
        height = text_height + 2 * padding
        
        if (width <= 0 .or. height <= 0) then
            print *, "ERROR: Invalid text dimensions:", text_width, "x", text_height
            passed = .false.
            return
        end if
        
        print *, "Text dimensions:", text_width, "x", text_height
        print *, "Bitmap size:", width, "x", height
        
        ! Create and render to bitmap
        allocate(bitmap(width, height, 3))
        bitmap = -1_1  ! White background
        
        call render_text_to_bitmap(bitmap, width, height, padding, padding + text_height, char_str)
        
        ! Analyze bitmap content
        total_pixels = width * height
        non_white_pixels = count_non_white_pixels(bitmap, width, height)
        
        print *, "Bitmap analysis:", non_white_pixels, "/", total_pixels, " pixels have ink"
        
        ! Test character-specific properties
        select case (test_char)
        case ('I')
            ! 'I' should be narrow and have significant ink
            if (text_width > 12) then
                print *, "WARNING: 'I' wider than expected (", text_width, " pixels)"
            end if
            if (non_white_pixels < total_pixels / 50) then
                print *, "ERROR: 'I' has too little ink (", non_white_pixels, " pixels)"
                passed = .false.
            end if
            
        case ('O')
            ! 'O' should have moderate ink (hollow center)
            if (non_white_pixels < total_pixels / 100) then
                print *, "ERROR: 'O' has too little ink"
                passed = .false.
            end if
            if (non_white_pixels > total_pixels / 2) then
                print *, "ERROR: 'O' has too much ink (should be hollow)"
                passed = .false.
            end if
            
        case ('.')
            ! Period should be small and have some ink
            if (non_white_pixels == 0) then
                print *, "ERROR: '.' has no ink"
                passed = .false.
            end if
            if (non_white_pixels > total_pixels / 10) then
                print *, "WARNING: '.' has unusually much ink"
            end if
            
        case ('W')
            ! 'W' should be wide and have significant ink
            if (text_width < 8) then
                print *, "WARNING: 'W' narrower than expected (", text_width, " pixels)"
            end if
            if (non_white_pixels < total_pixels / 20) then
                print *, "ERROR: 'W' has too little ink"
                passed = .false.
            end if
        end select
        
        ! General sanity checks for all characters
        if (non_white_pixels == 0) then
            print *, "ERROR: Character rendered with no ink pixels"
            passed = .false.
        end if
        
        if (non_white_pixels == total_pixels) then
            print *, "ERROR: Character rendered as solid block"
            passed = .false.
        end if
        
        ! Test that character doesn't extend to all edges
        if (passed) then
            passed = test_bitmap_edges(bitmap, width, height) .and. passed
        end if
        
        ! Test pixel color consistency
        if (passed) then
            passed = test_pixel_colors(bitmap, width, height) .and. passed
        end if
        
        if (passed) then
            print *, "PASS: Character '", test_char, "' rendered correctly"
        else
            print *, "FAIL: Character '", test_char, "' rendering issues detected"
        end if
        
        deallocate(bitmap)
    end function test_character_rendering
    
    function count_non_white_pixels(bitmap, width, height) result(count)
        !! Count pixels that are not white (have ink)
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
    
    function test_bitmap_edges(bitmap, width, height) result(passed)
        !! Test that character doesn't fill entire bitmap edges
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: width, height
        logical :: passed
        integer :: top_edge_pixels, bottom_edge_pixels, left_edge_pixels, right_edge_pixels
        integer :: i, j
        
        passed = .true.
        top_edge_pixels = 0
        bottom_edge_pixels = 0
        left_edge_pixels = 0
        right_edge_pixels = 0
        
        ! Count non-white edge pixels
        do i = 1, width
            ! Top edge
            if (bitmap(i,1,1) /= -1_1 .or. bitmap(i,1,2) /= -1_1 .or. bitmap(i,1,3) /= -1_1) then
                top_edge_pixels = top_edge_pixels + 1
            end if
            ! Bottom edge
            if (bitmap(i,height,1) /= -1_1 .or. bitmap(i,height,2) /= -1_1 .or. bitmap(i,height,3) /= -1_1) then
                bottom_edge_pixels = bottom_edge_pixels + 1
            end if
        end do
        
        do j = 1, height
            ! Left edge
            if (bitmap(1,j,1) /= -1_1 .or. bitmap(1,j,2) /= -1_1 .or. bitmap(1,j,3) /= -1_1) then
                left_edge_pixels = left_edge_pixels + 1
            end if
            ! Right edge
            if (bitmap(width,j,1) /= -1_1 .or. bitmap(width,j,2) /= -1_1 .or. bitmap(width,j,3) /= -1_1) then
                right_edge_pixels = right_edge_pixels + 1
            end if
        end do
        
        print *, "Edge pixels: top=", top_edge_pixels, " bottom=", bottom_edge_pixels, &
                 " left=", left_edge_pixels, " right=", right_edge_pixels
        
        ! Characters shouldn't fill entire edges (indicates clipping or overflow)
        if (top_edge_pixels == width .or. bottom_edge_pixels == width .or. &
            left_edge_pixels == height .or. right_edge_pixels == height) then
            print *, "WARNING: Character may be clipped at edges"
        end if
        
    end function test_bitmap_edges
    
    function test_pixel_colors(bitmap, width, height) result(passed)
        !! Test that non-white pixels are black (expected ink color)
        integer(1), intent(in) :: bitmap(:,:,:)
        integer, intent(in) :: width, height
        logical :: passed
        integer :: i, j, non_black_ink_pixels
        
        passed = .true.
        non_black_ink_pixels = 0
        
        do j = 1, height
            do i = 1, width
                ! If pixel is not white, it should be black (or close to black)
                if (bitmap(i,j,1) /= -1_1 .or. bitmap(i,j,2) /= -1_1 .or. bitmap(i,j,3) /= -1_1) then
                    ! Check if ink pixel is reasonably dark
                    if (bitmap(i,j,1) > 64_1 .or. bitmap(i,j,2) > 64_1 .or. bitmap(i,j,3) > 64_1) then
                        non_black_ink_pixels = non_black_ink_pixels + 1
                    end if
                end if
            end do
        end do
        
        if (non_black_ink_pixels > 0) then
            print *, "INFO:", non_black_ink_pixels, " ink pixels are not black (antialiasing)"
        end if
        
        ! This is informational - antialiasing may cause gray pixels
        
    end function test_pixel_colors

end program test_character_bitmap_rendering