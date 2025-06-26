program test_antialiased_markers
    use fortplot_png, only: png_context, create_png_canvas
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    call test_should_render_smooth_circle_edges()
    call test_should_support_separate_outline_fill_colors()
    call test_should_support_transparent_fill()
    print *, "All antialiased marker tests passed!"

contains

    subroutine test_should_render_smooth_circle_edges()
        type(png_context) :: ctx
        
        ctx = create_png_canvas(100, 100)
        ctx%x_min = 0.0_wp
        ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp
        ctx%y_max = 10.0_wp
        call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)
        
        ! Draw a circle marker - should be antialiased
        call ctx%draw_marker(5.0_wp, 5.0_wp, 'o')
        
        ! Check that edge pixels have partial alpha (antialiasing)
        ! This will fail with current implementation
        if (.not. has_antialiased_edges(ctx)) then
            error stop "Circle marker should have antialiased edges"
        end if
        
        call ctx%save('test_antialiased_circle.png')
    end subroutine test_should_render_smooth_circle_edges

    subroutine test_should_support_separate_outline_fill_colors()
        type(png_context) :: ctx
        
        ctx = create_png_canvas(100, 100)
        ctx%x_min = 0.0_wp
        ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp
        ctx%y_max = 10.0_wp
        
        ! This API doesn't exist yet - will fail compilation
        call ctx%set_marker_colors(edge_r=0.0_wp, edge_g=0.0_wp, edge_b=1.0_wp, &
                                   face_r=1.0_wp, face_g=0.0_wp, face_b=0.0_wp)
        call ctx%draw_marker(5.0_wp, 5.0_wp, 'o')
        
        ! Check that marker has blue outline and red fill
        if (.not. has_separate_edge_face_colors(ctx)) then
            error stop "Marker should have separate edge and face colors"
        end if
        
        call ctx%save('test_marker_colors.png')
    end subroutine test_should_support_separate_outline_fill_colors

    subroutine test_should_support_transparent_fill()
        type(png_context) :: ctx
        
        ctx = create_png_canvas(100, 100)
        ctx%x_min = 0.0_wp
        ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp
        ctx%y_max = 10.0_wp
        call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)
        
        ! Draw overlapping markers to test transparency
        call ctx%draw_marker(4.0_wp, 5.0_wp, 'o')
        call ctx%draw_marker(6.0_wp, 5.0_wp, 'o')
        
        call ctx%save('test_transparent_markers.png')
    end subroutine test_should_support_transparent_fill

    logical function has_antialiased_edges(ctx)
        type(png_context), intent(inout) :: ctx
        integer :: x, y, k
        integer :: r_val, g_val, b_val
        logical :: found_partial_alpha
        
        found_partial_alpha = .false.
        
        ! Save the image for visual inspection
        call ctx%save('test_antialiased_circle.png')
        
        ! Check pixels around the marker center for partial alpha values
        ! Look for any pixels that are not pure white (255,255,255) or pure red (255,0,0)
        do y = 40, 60
            do x = 40, 60
                k = (y - 1) * (1 + ctx%width * 3) + 1 + (x - 1) * 3 + 1
                if (k > 0 .and. k <= size(ctx%raster%image_data) - 2) then
                    r_val = int(ctx%raster%image_data(k))
                    g_val = int(ctx%raster%image_data(k+1))
                    b_val = int(ctx%raster%image_data(k+2))
                    
                    ! Convert signed byte to unsigned
                    if (r_val < 0) r_val = r_val + 256
                    if (g_val < 0) g_val = g_val + 256
                    if (b_val < 0) b_val = b_val + 256
                    
                    ! Check for any intermediate values indicating blending
                    if ((r_val > 0 .and. r_val < 255) .or. &
                        (g_val > 0 .and. g_val < 255) .or. &
                        (b_val > 0 .and. b_val < 255)) then
                        found_partial_alpha = .true.
                        print *, "Found antialiased pixel at", x, y, "RGB:", r_val, g_val, b_val
                        exit
                    end if
                end if
            end do
            if (found_partial_alpha) exit
        end do
        
        if (.not. found_partial_alpha) then
            print *, "No antialiased pixels found. This suggests aliased rendering."
        end if
        
        has_antialiased_edges = found_partial_alpha
    end function has_antialiased_edges

    logical function has_separate_edge_face_colors(ctx)
        type(png_context), intent(inout) :: ctx
        integer :: x, y, k
        integer :: r_val, g_val, b_val
        logical :: found_blue_pixels, found_red_pixels
        
        ! Save the image for visual inspection
        call ctx%save('test_marker_colors.png')
        
        found_blue_pixels = .false.
        found_red_pixels = .false.
        
        ! Look for blue edge pixels and red face pixels
        do y = 40, 60
            do x = 40, 60
                k = (y - 1) * (1 + ctx%width * 3) + 1 + (x - 1) * 3 + 1
                if (k > 0 .and. k <= size(ctx%raster%image_data) - 2) then
                    r_val = int(ctx%raster%image_data(k))
                    g_val = int(ctx%raster%image_data(k+1))
                    b_val = int(ctx%raster%image_data(k+2))
                    
                    ! Convert signed byte to unsigned
                    if (r_val < 0) r_val = r_val + 256
                    if (g_val < 0) g_val = g_val + 256
                    if (b_val < 0) b_val = b_val + 256
                    
                    ! Check for blue pixels (edge color: 0,0,1 -> 0,0,255)
                    if (r_val < 50 .and. g_val < 50 .and. b_val > 200) then
                        found_blue_pixels = .true.
                        print *, "Found blue edge pixel at", x, y, "RGB:", r_val, g_val, b_val
                    end if
                    
                    ! Check for red pixels (face color: 1,0,0 -> 255,0,0)
                    if (r_val > 200 .and. g_val < 50 .and. b_val < 50) then
                        found_red_pixels = .true.
                        print *, "Found red face pixel at", x, y, "RGB:", r_val, g_val, b_val
                    end if
                end if
            end do
        end do
        
        print *, "Edge detection results: blue =", found_blue_pixels, "red =", found_red_pixels
        has_separate_edge_face_colors = found_blue_pixels .and. found_red_pixels
    end function has_separate_edge_face_colors

end program test_antialiased_markers