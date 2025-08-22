program test_ascii_quad_rendering
    !! Test ASCII quadrilateral rendering for pcolormesh
    !!
    !! Tests the core fill_quad functionality to ensure proper
    !! character-based mesh visualization instead of solid fills.
    !!
    !! Given: ASCII context with quad data and colors
    !! When: Calling fill_quad with varying data values
    !! Then: Should render using density/pattern characters, not solid blocks

    use fortplot_ascii, only: ascii_context, create_ascii_canvas
    use fortplot_pcolormesh, only: pcolormesh_t
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    logical :: all_tests_passed = .true.
    
    ! Test core quad rendering functionality
    call test_fill_quad_character_mapping()
    call test_quad_data_value_rendering()
    call test_overlapping_quads_blending()
    call test_quad_coordinate_transformation()
    call test_quad_boundary_handling()
    
    if (all_tests_passed) then
        print *, "All ASCII quad rendering tests PASSED (but expected to FAIL until Issue #176 fixed)"
    else
        print *, "ASCII quad rendering tests completed - failures expected due to Issue #176"
        call exit(1)
    end if

contains

    subroutine test_fill_quad_character_mapping()
        !! Given: ASCII context and quad vertices
        !! When: Filling quad with normalized data values 0.0 to 1.0
        !! Then: Should map to ASCII_CHARS range, not always use '#'
        
        type(ascii_context) :: ctx
        real(wp) :: x_quad(4), y_quad(4)
        integer :: i, j, filled_positions
        character(len=1) :: found_char
        logical :: uses_varied_chars = .false.
        character(len=10) :: unique_chars = ""
        integer :: unique_count = 0
        
        print *, "Testing fill_quad character mapping..."
        
        ctx = create_ascii_canvas(20, 15)
        call ctx%set_coordinates(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp)
        
        ! Create test quad covering center of canvas
        x_quad = [0.3_wp, 0.7_wp, 0.7_wp, 0.3_wp]
        y_quad = [0.3_wp, 0.3_wp, 0.7_wp, 0.7_wp]
        
        ! Test with low intensity (should use light characters)
        call ctx%color(0.2_wp, 0.2_wp, 0.2_wp)
        call ctx%fill_quad(x_quad, y_quad)
        
        ! Count non-space characters and collect unique ones
        filled_positions = 0
        do i = 1, ctx%plot_height
            do j = 1, ctx%plot_width
                found_char = ctx%canvas(i, j)
                if (found_char /= ' ') then
                    filled_positions = filled_positions + 1
                    ! Track unique characters
                    if (index(unique_chars, found_char) == 0) then
                        unique_count = unique_count + 1
                        if (unique_count <= 10) then
                            unique_chars(unique_count:unique_count) = found_char
                        end if
                    end if
                end if
            end do
        end do
        
        ! Issue #176: Current implementation fills everything with '#'
        ! Proper implementation should use varied characters based on data value
        if (unique_count > 1) then
            uses_varied_chars = .true.
        end if
        
        ! This test should FAIL until Issue #176 is fixed
        if (.not. uses_varied_chars) then
            print *, "EXPECTED FAIL: fill_quad uses solid blocks instead of varied characters"
            print '(A,I0,A)', "  Found ", filled_positions, " positions filled"
            print '(A,I0,A,A)', "  Using ", unique_count, " unique characters: ", trim(unique_chars)
            all_tests_passed = .false.
        else
            print *, "UNEXPECTED PASS: fill_quad uses varied characters"
        end if
    end subroutine test_fill_quad_character_mapping

    subroutine test_quad_data_value_rendering()
        !! Given: Multiple quads with different data values
        !! When: Rendering each with corresponding color intensity
        !! Then: Should produce different character patterns
        
        type(ascii_context) :: ctx
        real(wp) :: x_quad(4), y_quad(4)
        real(wp) :: test_values(5) = [0.0_wp, 0.25_wp, 0.5_wp, 0.75_wp, 1.0_wp]
        character(len=5) :: result_chars = ""
        integer :: i, j, k
        
        print *, "Testing quad data value rendering..."
        
        ctx = create_ascii_canvas(25, 15)
        call ctx%set_coordinates(0.0_wp, 5.0_wp, 0.0_wp, 1.0_wp)
        
        ! Render 5 quads with different data values side by side
        do k = 1, 5
            ! Position quads side by side
            x_quad = [real(k-1, wp), real(k, wp), real(k, wp), real(k-1, wp)]
            y_quad = [0.2_wp, 0.2_wp, 0.8_wp, 0.8_wp]
            
            ! Set color intensity based on data value
            call ctx%color(test_values(k), test_values(k), test_values(k))
            call ctx%fill_quad(x_quad, y_quad)
            
            ! Sample character from center of each quad
            i = ctx%plot_height / 2
            j = 2 + 4 * (k - 1)  ! Approximate center of each quad
            if (j <= ctx%plot_width) then
                result_chars(k:k) = ctx%canvas(i, j)
            end if
        end do
        
        ! Check if all quads render with same character (Issue #176 problem)
        if (result_chars == "#####" .or. result_chars == "     ") then
            print *, "EXPECTED FAIL: All data values render identically"
            print '(A,A)', "  Result characters: '", result_chars, "'"
            all_tests_passed = .false.
        else
            print *, "UNEXPECTED PASS: Different data values render differently"
            print '(A,A)', "  Result characters: '", result_chars, "'"
        end if
    end subroutine test_quad_data_value_rendering

    subroutine test_overlapping_quads_blending()
        !! Given: Two overlapping quads with different data values
        !! When: Rendering both quads
        !! Then: Overlapping region should show blended/combined character
        
        type(ascii_context) :: ctx
        real(wp) :: x_quad1(4), y_quad1(4), x_quad2(4), y_quad2(4)
        character(len=1) :: overlap_char, quad1_char, quad2_char
        integer :: center_i, center_j
        
        print *, "Testing overlapping quad blending..."
        
        ctx = create_ascii_canvas(20, 15)
        call ctx%set_coordinates(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp)
        
        ! First quad (left side)
        x_quad1 = [0.2_wp, 0.6_wp, 0.6_wp, 0.2_wp]
        y_quad1 = [0.3_wp, 0.3_wp, 0.7_wp, 0.7_wp]
        
        ! Second quad (right side, overlapping)
        x_quad2 = [0.4_wp, 0.8_wp, 0.8_wp, 0.4_wp] 
        y_quad2 = [0.3_wp, 0.3_wp, 0.7_wp, 0.7_wp]
        
        ! Render first quad with medium intensity
        call ctx%color(0.4_wp, 0.4_wp, 0.4_wp)
        call ctx%fill_quad(x_quad1, y_quad1)
        
        ! Get character from first quad only region
        center_i = ctx%plot_height / 2
        center_j = ctx%plot_width / 4
        quad1_char = ctx%canvas(center_i, center_j)
        
        ! Render second quad with high intensity
        call ctx%color(0.8_wp, 0.8_wp, 0.8_wp)
        call ctx%fill_quad(x_quad2, y_quad2)
        
        ! Get character from second quad only region  
        center_j = 3 * ctx%plot_width / 4
        quad2_char = ctx%canvas(center_i, center_j)
        
        ! Get character from overlapping region
        center_j = ctx%plot_width / 2
        overlap_char = ctx%canvas(center_i, center_j)
        
        ! Test character blending behavior
        print '(A,A,A,A,A,A)', "  Quad1: '", quad1_char, "', Quad2: '", quad2_char, "', Overlap: '", overlap_char, "'"
        
        ! With current implementation, all will likely be '#'
        if (quad1_char == quad2_char .and. quad2_char == overlap_char .and. overlap_char == '#') then
            print *, "EXPECTED FAIL: No character variation or blending"
            all_tests_passed = .false.
        else
            print *, "UNEXPECTED PASS: Character blending working"
        end if
    end subroutine test_overlapping_quads_blending

    subroutine test_quad_coordinate_transformation()
        !! Given: Quad vertices in data coordinates
        !! When: Converting to canvas coordinates for rendering
        !! Then: Should map correctly to canvas bounds
        
        type(ascii_context) :: ctx
        real(wp) :: x_quad(4), y_quad(4)
        integer :: min_x, max_x, min_y, max_y, i, j
        logical :: quad_rendered = .false.
        
        print *, "Testing quad coordinate transformation..."
        
        ctx = create_ascii_canvas(40, 20)
        call ctx%set_coordinates(-2.0_wp, 2.0_wp, -1.0_wp, 1.0_wp)
        
        ! Create quad in data coordinates
        x_quad = [-0.5_wp, 0.5_wp, 0.5_wp, -0.5_wp]
        y_quad = [-0.25_wp, -0.25_wp, 0.25_wp, 0.25_wp]
        
        call ctx%color(0.7_wp, 0.7_wp, 0.7_wp)
        call ctx%fill_quad(x_quad, y_quad)
        
        ! Find rendered region bounds
        min_x = ctx%plot_width; max_x = 1
        min_y = ctx%plot_height; max_y = 1
        
        do i = 1, ctx%plot_height
            do j = 1, ctx%plot_width
                if (ctx%canvas(i, j) /= ' ') then
                    quad_rendered = .true.
                    min_x = min(min_x, j)
                    max_x = max(max_x, j)
                    min_y = min(min_y, i)
                    max_y = max(max_y, i)
                end if
            end do
        end do
        
        if (quad_rendered) then
            print '(A,I0,A,I0,A,I0,A,I0)', "  Rendered region: x[", min_x, ",", max_x, "] y[", min_y, ",", max_y, "]"
            
            ! Check if rendered in approximately correct location (center of canvas)
            if (min_x > ctx%plot_width/4 .and. max_x < 3*ctx%plot_width/4 .and. &
                min_y > ctx%plot_height/4 .and. max_y < 3*ctx%plot_height/4) then
                print *, "PASS: Coordinate transformation working"
            else
                print *, "FAIL: Coordinate transformation incorrect"
                all_tests_passed = .false.
            end if
        else
            print *, "FAIL: No quad rendered - coordinate transformation issue"
            all_tests_passed = .false.
        end if
    end subroutine test_quad_coordinate_transformation

    subroutine test_quad_boundary_handling()
        !! Given: Quad vertices partially outside canvas bounds
        !! When: Rendering the quad
        !! Then: Should clip properly and not crash
        
        type(ascii_context) :: ctx
        real(wp) :: x_quad(4), y_quad(4)
        integer :: i, j, rendered_count = 0
        
        print *, "Testing quad boundary handling..."
        
        ctx = create_ascii_canvas(20, 15)
        call ctx%set_coordinates(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp)
        
        ! Create quad that extends outside canvas bounds
        x_quad = [-0.2_wp, 0.3_wp, 0.3_wp, -0.2_wp]  ! Left side extends beyond left edge
        y_quad = [0.2_wp, 0.2_wp, 1.2_wp, 1.2_wp]    ! Top extends beyond top edge
        
        call ctx%color(0.6_wp, 0.6_wp, 0.6_wp)
        call ctx%fill_quad(x_quad, y_quad)  ! Should not crash
        
        ! Count rendered characters
        do i = 1, ctx%plot_height
            do j = 1, ctx%plot_width
                if (ctx%canvas(i, j) /= ' ') then
                    rendered_count = rendered_count + 1
                end if
            end do
        end do
        
        if (rendered_count > 0) then
            print '(A,I0)', "  PASS: Boundary clipping working, rendered ", rendered_count, " characters"
        else
            print *, "FAIL: Boundary quad not rendered"
            all_tests_passed = .false.
        end if
    end subroutine test_quad_boundary_handling

end program test_ascii_quad_rendering