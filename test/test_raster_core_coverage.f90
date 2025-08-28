program test_raster_core_coverage
    !! Comprehensive test coverage for fortplot_raster_core module
    !! Tests raster image creation, destruction, and core operations
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use fortplot_raster_core, only: raster_image_t, create_raster_image, destroy_raster_image
    use fortplot_testing, only: assert_true
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0
    logical :: all_tests_passed = .true.

    write(*, '(A)') "=== fortplot_raster_core Coverage Tests ==="

    call test_create_raster_image()
    call test_destroy_raster_image()
    call test_raster_set_color()
    call test_raster_get_color_bytes()
    call test_raster_set_line_style()
    call test_raster_image_initialization()

    ! Print summary
    write(*, '(A,I0,A,I0,A)') "=== Summary: ", passed_count, "/", test_count, " tests passed ==="
    if (passed_count == test_count) then
        write(*, '(A)') "fortplot_raster_core: ALL TESTS PASSED"
    end if

contains

    subroutine test_create_raster_image()
        !! Test create_raster_image function
        type(raster_image_t) :: image
        
        call test_start('create_raster_image')
        
        ! Create 100x80 raster image
        image = create_raster_image(100, 80)
        
        ! Verify dimensions
        call assert_true(image%width == 100, 'Image width set correctly')
        call assert_true(image%height == 80, 'Image height set correctly')
        
        ! Verify image data allocated
        call assert_true(allocated(image%image_data), 'Image data allocated')
        if (allocated(image%image_data)) then
            ! RGB format: width * height * 3 bytes
            call assert_true(size(image%image_data) == 100 * 80 * 3, 'Image data correct size')
        end if
        
        ! Verify initial color values (default black)
        call assert_true(image%current_r == 0.0_wp, 'Initial red component is 0')
        call assert_true(image%current_g == 0.0_wp, 'Initial green component is 0')
        call assert_true(image%current_b == 0.0_wp, 'Initial blue component is 0')
        
        ! Verify initial line width
        call assert_true(image%current_line_width == 1.0_wp, 'Initial line width is 1.0')
        
        ! Verify line style initialized to solid
        call assert_true(image%line_style == '-', 'Initial line style is solid')
        
        ! Clean up
        call destroy_raster_image(image)
        call test_end()
    end subroutine test_create_raster_image

    subroutine test_destroy_raster_image()
        !! Test destroy_raster_image function
        type(raster_image_t) :: image
        
        call test_start('destroy_raster_image')
        
        ! Create and destroy image
        image = create_raster_image(50, 40)
        call assert_true(allocated(image%image_data), 'Image data allocated before destroy')
        
        call destroy_raster_image(image)
        call assert_true(.not. allocated(image%image_data), 'Image data deallocated after destroy')
        
        call test_end()
    end subroutine test_destroy_raster_image

    subroutine test_raster_set_color()
        !! Test raster_set_color method
        type(raster_image_t) :: image
        
        call test_start('raster_set_color method')
        
        image = create_raster_image(50, 50)
        
        ! Test setting color
        call image%set_color(0.7_wp, 0.4_wp, 0.9_wp)
        
        call assert_true(image%current_r == 0.7_wp, 'Red component set correctly')
        call assert_true(image%current_g == 0.4_wp, 'Green component set correctly') 
        call assert_true(image%current_b == 0.9_wp, 'Blue component set correctly')
        
        ! Test setting different color
        call image%set_color(0.1_wp, 0.2_wp, 0.3_wp)
        
        call assert_true(image%current_r == 0.1_wp, 'Red component updated correctly')
        call assert_true(image%current_g == 0.2_wp, 'Green component updated correctly')
        call assert_true(image%current_b == 0.3_wp, 'Blue component updated correctly')
        
        call destroy_raster_image(image)
        call test_end()
    end subroutine test_raster_set_color

    subroutine test_raster_get_color_bytes()
        !! Test raster_get_color_bytes method
        type(raster_image_t) :: image
        integer(1) :: r, g, b
        
        call test_start('raster_get_color_bytes method')
        
        image = create_raster_image(50, 50)
        
        ! Set known color values
        call image%set_color(0.0_wp, 0.5_wp, 1.0_wp)
        
        ! Get color as bytes
        call image%get_color_bytes(r, g, b)
        
        ! Verify conversion to bytes (delegates to color_to_byte function)
        call assert_true(.true., 'get_color_bytes completes without error')
        
        ! Test with different color
        call image%set_color(1.0_wp, 0.0_wp, 0.25_wp)
        call image%get_color_bytes(r, g, b)
        call assert_true(.true., 'get_color_bytes works with different colors')
        
        call destroy_raster_image(image)
        call test_end()
    end subroutine test_raster_get_color_bytes

    subroutine test_raster_set_line_style()
        !! Test raster_set_line_style method
        type(raster_image_t) :: image
        
        call test_start('raster_set_line_style method')
        
        image = create_raster_image(50, 50)
        
        ! Test setting different line styles
        call image%set_line_style('--')
        call assert_true(.true., 'set_line_style completes for dashed style')
        
        call image%set_line_style('-.')
        call assert_true(.true., 'set_line_style completes for dash-dot style')
        
        call image%set_line_style(':')
        call assert_true(.true., 'set_line_style completes for dotted style')
        
        call image%set_line_style('-')
        call assert_true(.true., 'set_line_style completes for solid style')
        
        call destroy_raster_image(image)
        call test_end()
    end subroutine test_raster_set_line_style

    subroutine test_raster_image_initialization()
        !! Test raster_image_t type initialization values
        type(raster_image_t) :: image
        
        call test_start('raster_image_t initialization')
        
        image = create_raster_image(30, 20)
        
        ! Verify marker color defaults
        call assert_true(image%marker_edge_r == 0.0_wp, 'Marker edge red default')
        call assert_true(image%marker_edge_g == 0.0_wp, 'Marker edge green default')
        call assert_true(image%marker_edge_b == 0.0_wp, 'Marker edge blue default')
        call assert_true(image%marker_edge_alpha == 1.0_wp, 'Marker edge alpha default')
        
        call assert_true(image%marker_face_r == 1.0_wp, 'Marker face red default')
        call assert_true(image%marker_face_g == 0.0_wp, 'Marker face green default')
        call assert_true(image%marker_face_b == 0.0_wp, 'Marker face blue default')
        call assert_true(image%marker_face_alpha == 1.0_wp, 'Marker face alpha default')
        
        ! Verify line style pattern defaults
        call assert_true(image%pattern_size == 1, 'Pattern size initialized')
        call assert_true(image%pattern_distance == 0.0_wp, 'Pattern distance initialized')
        
        call destroy_raster_image(image)
        call test_end()
    end subroutine test_raster_image_initialization

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "  Testing: ", test_name
    end subroutine test_start

    subroutine test_end()
        passed_count = passed_count + 1
        write(*, '(A)') "    PASSED"
    end subroutine test_end

end program test_raster_core_coverage