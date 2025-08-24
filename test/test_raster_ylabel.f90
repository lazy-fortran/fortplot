program test_raster_ylabel
    !! Test that raster_render_ylabel properly rotates y-axis labels
    use fortplot_raster, only: raster_context
    use fortplot_layout, only: plot_area_t
    implicit none
    
    type(raster_context) :: ctx
    type(plot_area_t) :: plot_area
    integer :: width, height
    integer :: x, y, idx
    logical :: found_black_pixel
    logical :: test_passed
    
    ! Initialize test
    width = 800
    height = 600
    test_passed = .true.
    
    ! Allocate raster image
    allocate(ctx%raster%image_data(width * height * 3))
    
    ! Initialize with white background
    ctx%raster%image_data = -1_1  ! All white (255 unsigned = -1 signed)
    
    ! Set up plot area (typical values)
    plot_area%left = 100
    plot_area%bottom = 100
    plot_area%width = 600
    plot_area%height = 400
    ctx%plot_area = plot_area
    ctx%raster%width = width
    ctx%raster%height = height
    
    ! Render a y-axis label
    call ctx%render_ylabel("Temperature (°C)")
    
    ! Check that text was rendered (should find non-white pixels)
    found_black_pixel = .false.
    
    ! Check in the expected region for rotated text (left of plot area)
    ! Y-axis label should be around x=40-80, centered vertically
    do y = height/2 - 100, height/2 + 100
        do x = 20, 90
            idx = ((y - 1) * width + (x - 1)) * 3 + 1
            if (idx > 0 .and. idx + 2 <= size(ctx%raster%image_data)) then
                ! Check if pixel is not white (text should be black)
                if (ctx%raster%image_data(idx) /= -1_1 .or. &
                    ctx%raster%image_data(idx + 1) /= -1_1 .or. &
                    ctx%raster%image_data(idx + 2) /= -1_1) then
                    found_black_pixel = .true.
                    exit
                end if
            end if
        end do
        if (found_black_pixel) exit
    end do
    
    if (.not. found_black_pixel) then
        print *, "FAIL: No text pixels found in expected region for rotated y-label"
        test_passed = .false.
    end if
    
    ! Additional test: verify text is vertically oriented
    ! For rotated text, we expect more height than width in the rendered area
    if (found_black_pixel) then
        block
            ! Find bounding box of non-white pixels in the y-label region
            integer :: min_x, max_x, min_y, max_y
            integer :: text_width, text_height
        
            min_x = width
            max_x = 1
            min_y = height
            max_y = 1
        
            do y = 1, height
                do x = 1, 99  ! Only check left region where y-label should be
                    idx = ((y - 1) * width + (x - 1)) * 3 + 1
                    if (idx > 0 .and. idx + 2 <= size(ctx%raster%image_data)) then
                        if (ctx%raster%image_data(idx) /= -1_1 .or. &
                            ctx%raster%image_data(idx + 1) /= -1_1 .or. &
                            ctx%raster%image_data(idx + 2) /= -1_1) then
                            if (x < min_x) min_x = x
                            if (x > max_x) max_x = x
                            if (y < min_y) min_y = y
                            if (y > max_y) max_y = y
                        end if
                    end if
                end do
            end do
            
            ! For rotated text, height should be greater than width
            text_width = max_x - min_x + 1
            text_height = max_y - min_y + 1
        
            if (text_height <= text_width) then
                print *, "FAIL: Y-label text appears horizontal (width=", text_width, &
                         " height=", text_height, ")"
                test_passed = .false.
            else
                print *, "PASS: Y-label text is properly rotated (width=", text_width, &
                         " height=", text_height, ")"
            end if
        end block
    end if
    
    ! Clean up
    deallocate(ctx%raster%image_data)
    
    ! Report result
    if (test_passed) then
        print *, "All raster_render_ylabel tests PASSED"
        stop 0
    else
        print *, "Some raster_render_ylabel tests FAILED"
        stop 1
    end if
    
end program test_raster_ylabel