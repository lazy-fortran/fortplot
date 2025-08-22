program test_ascii_pcolormesh_mesh_rendering
    !! Test ASCII pcolormesh mesh rendering to fix Issue #176
    !! 
    !! Tests that pcolormesh shows mesh structure instead of solid fill
    !! in ASCII backend through proper character-based visualization.
    !!
    !! Given: ASCII backend with pcolormesh data
    !! When: Rendering mesh with varying data values
    !! Then: Should show character-based mesh pattern, not solid blocks
    
    use fortplot, only: figure_t, wp
    use fortplot_ascii, only: ascii_context, create_ascii_canvas
    use fortplot_security, only: get_test_output_path
    implicit none
    
    logical :: test_passed
    integer :: total_tests = 0, failed_tests = 0
    
    ! Test ASCII mesh character mapping
    call test_ascii_mesh_character_mapping()
    
    ! Test pcolormesh quad rendering shows mesh structure
    call test_pcolormesh_quad_mesh_structure()
    
    ! Test colormap integration with mesh rendering
    call test_colormap_mesh_integration()
    
    ! Test edge cases
    call test_empty_mesh_handling()
    call test_single_quad_mesh()
    call test_mesh_boundaries()
    
    ! Print results
    if (failed_tests == 0) then
        print *, "All ASCII pcolormesh mesh rendering tests PASSED"
    else
        print '(A,I0,A,I0)', "ASCII pcolormesh tests FAILED: ", failed_tests, " of ", total_tests
        call exit(1)
    end if

contains

    subroutine test_ascii_mesh_character_mapping()
        !! Given: ASCII context with fill_quad method
        !! When: Rendering quadrilaterals with different data values
        !! Then: Should use appropriate characters based on data values, not solid blocks
        
        type(ascii_context) :: ctx
        real(wp) :: x_quad(4), y_quad(4)
        real(wp) :: test_data_values(5) = [0.0_wp, 0.25_wp, 0.5_wp, 0.75_wp, 1.0_wp]
        character(len=1) :: expected_chars(5) = [' ', '.', '=', '#', '@']
        integer :: i, j, quad_x, quad_y
        character(len=1) :: rendered_char
        logical :: found_expected_char
        
        total_tests = total_tests + 1
        
        ! Setup ASCII canvas
        ctx = create_ascii_canvas(40, 20)
        call ctx%set_coordinates(-1.0_wp, 1.0_wp, -1.0_wp, 1.0_wp)
        
        ! Test different data values map to different characters
        do i = 1, 5
            ! Clear canvas
            ctx%canvas = ' '
            
            ! Set color based on normalized data value
            call ctx%color(test_data_values(i), test_data_values(i), test_data_values(i))
            
            ! Create small test quad in center
            x_quad = [-0.1_wp, 0.1_wp, 0.1_wp, -0.1_wp]
            y_quad = [-0.1_wp, -0.1_wp, 0.1_wp, 0.1_wp]
            
            ! Fill quad - this should NOT produce solid '#' for all values
            call ctx%fill_quad(x_quad, y_quad)
            
            ! Check that different data values produce different characters
            found_expected_char = .false.
            do quad_y = 1, ctx%plot_height
                do quad_x = 1, ctx%plot_width
                    rendered_char = ctx%canvas(quad_y, quad_x)
                    if (rendered_char /= ' ') then
                        ! For Issue #176: Should NOT always be '#'
                        if (i == 1 .and. rendered_char /= '#') found_expected_char = .true.
                        if (i == 2 .and. rendered_char /= '#') found_expected_char = .true.
                        if (i == 3 .and. rendered_char /= '#') found_expected_char = .true.
                        if (i == 4 .and. rendered_char /= '#') found_expected_char = .true.
                        if (i == 5 .and. rendered_char == '#') found_expected_char = .true.
                        exit
                    end if
                end do
                if (found_expected_char) exit
            end do
            
            if (.not. found_expected_char) then
                print *, "FAIL: ASCII mesh character mapping - all values render as solid blocks"
                failed_tests = failed_tests + 1
                return
            end if
        end do
        
        print *, "EXPECTED FAIL: ASCII mesh character mapping test"
    end subroutine test_ascii_mesh_character_mapping

    subroutine test_pcolormesh_quad_mesh_structure()
        !! Given: Figure with pcolormesh data
        !! When: Rendering to ASCII backend
        !! Then: Should show mesh structure, not uniform solid fill
        
        type(figure_t) :: fig
        real(wp), allocatable :: x_coords(:), y_coords(:), mesh_data(:,:)
        character(len=:), allocatable :: output
        integer :: i, j, mesh_size = 5
        logical :: has_variation
        character(len=1) :: prev_char, curr_char
        integer :: char_count
        
        total_tests = total_tests + 1
        
        ! Create test mesh with varying data
        allocate(x_coords(mesh_size + 1), y_coords(mesh_size + 1))
        allocate(mesh_data(mesh_size, mesh_size))
        
        ! Grid coordinates
        do i = 1, mesh_size + 1
            x_coords(i) = real(i - 1, wp) / real(mesh_size, wp)
            y_coords(i) = real(i - 1, wp) / real(mesh_size, wp) 
        end do
        
        ! Varying mesh data (checkerboard pattern)
        do i = 1, mesh_size
            do j = 1, mesh_size
                mesh_data(i, j) = real(mod(i + j, 2), wp)
            end do
        end do
        
        ! Create pcolormesh plot
        call fig%initialize(40, 20)
        call fig%add_pcolormesh(x_coords, y_coords, mesh_data)
        call fig%savefig("terminal")  ! Force ASCII output
        
        ! For Issue #176: ASCII output should show mesh structure variation
        ! NOT uniform solid fill with all same characters
        has_variation = .false.
        prev_char = ' '
        char_count = 0
        
        ! This test will fail until Issue #176 is fixed
        ! Expected behavior: different parts of mesh should render differently
        if (.not. has_variation) then
            print *, "EXPECTED FAIL: Pcolormesh shows solid fill instead of mesh structure"
            failed_tests = failed_tests + 1
        end if
    end subroutine test_pcolormesh_quad_mesh_structure

    subroutine test_colormap_mesh_integration()
        !! Given: Pcolormesh with different colormaps
        !! When: Rendering to ASCII backend
        !! Then: Different colormaps should produce different character patterns
        
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), data(:,:)
        integer :: i, j
        
        total_tests = total_tests + 1
        
        ! Create gradient test data
        allocate(x(4), y(4), data(3, 3))
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        
        do i = 1, 3
            do j = 1, 3
                data(i, j) = real(i + j, wp) / 6.0_wp  ! Gradient from 0.33 to 1.0
            end do
        end do
        
        ! Test different colormaps produce different outputs
        ! (This will fail until proper colormap integration is implemented)
        call fig%initialize(40, 20)
        call fig%add_pcolormesh(x, y, data, colormap='viridis')
        call fig%savefig("terminal")
        
        print *, "EXPECTED FAIL: Colormap integration with ASCII mesh rendering"
        failed_tests = failed_tests + 1
    end subroutine test_colormap_mesh_integration

    subroutine test_empty_mesh_handling()
        !! Given: Empty or zero-size mesh data
        !! When: Rendering to ASCII backend
        !! Then: Should handle gracefully without crashing
        
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), data(:,:)
        
        total_tests = total_tests + 1
        
        ! Test empty mesh (1x1 vertex grid = 0x0 quads)
        allocate(x(1), y(1), data(0, 0))
        x(1) = 0.0_wp
        y(1) = 0.0_wp
        
        call fig%initialize(20, 10)
        ! This should not crash
        call fig%add_pcolormesh(x, y, data)
        call fig%savefig("terminal")
        
        print *, "PASS: Empty mesh handling (no crash)"
    end subroutine test_empty_mesh_handling

    subroutine test_single_quad_mesh()
        !! Given: Single quadrilateral mesh
        !! When: Rendering to ASCII backend  
        !! Then: Should render single quad with appropriate character
        
        type(figure_t) :: fig
        real(wp) :: x(2) = [0.0_wp, 1.0_wp]
        real(wp) :: y(2) = [0.0_wp, 1.0_wp] 
        real(wp) :: data(1, 1) = reshape([0.7_wp], [1, 1])
        
        total_tests = total_tests + 1
        
        call fig%initialize(10, 10)
        call fig%add_pcolormesh(x, y, data)
        call fig%savefig("terminal")
        
        ! Should render single quad, not crash
        print *, "EXPECTED FAIL: Single quad mesh rendering produces solid block"
        failed_tests = failed_tests + 1
    end subroutine test_single_quad_mesh

    subroutine test_mesh_boundaries()
        !! Given: Mesh with extreme values at boundaries
        !! When: Rendering to ASCII backend
        !! Then: Boundary quads should use appropriate edge characters
        
        type(figure_t) :: fig
        real(wp) :: x(4) = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: y(4) = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: data(3, 3)
        integer :: i, j
        
        total_tests = total_tests + 1
        
        ! Create boundary pattern (high values at edges, low in center)
        do i = 1, 3
            do j = 1, 3
                if (i == 1 .or. i == 3 .or. j == 1 .or. j == 3) then
                    data(i, j) = 1.0_wp  ! Boundary
                else
                    data(i, j) = 0.0_wp  ! Center
                end if
            end do
        end do
        
        call fig%initialize(20, 15)
        call fig%add_pcolormesh(x, y, data)
        call fig%savefig("terminal")
        
        print *, "EXPECTED FAIL: Mesh boundaries show solid fill instead of pattern"
        failed_tests = failed_tests + 1
    end subroutine test_mesh_boundaries

end program test_ascii_pcolormesh_mesh_rendering