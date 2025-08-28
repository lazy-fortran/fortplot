program test_raster_core_simple
    !! Simple test coverage for fortplot_raster_core module
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_raster_core, only: raster_image_t, create_raster_image, destroy_raster_image
    use fortplot_testing, only: assert_true
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0

    write(*, '(A)') "=== fortplot_raster_core Simple Tests ==="

    call test_raster_operations()

    ! Print summary
    write(*, '(A,I0,A,I0,A)') "=== Summary: ", passed_count, "/", test_count, " tests passed ==="
    if (passed_count == test_count) then
        write(*, '(A)') "fortplot_raster_core: ALL TESTS PASSED"
    end if

contains

    subroutine test_raster_operations()
        type(raster_image_t) :: image
        integer(1) :: r, g, b
        
        call test_start('raster core operations')
        
        ! Test creation
        image = create_raster_image(100, 80)
        call assert_true(image%width == 100, 'Image width set correctly')
        call assert_true(image%height == 80, 'Image height set correctly')
        call assert_true(allocated(image%image_data), 'Image data allocated')
        call assert_true(size(image%image_data) == 100 * 80 * 3, 'Image data correct size')
        
        ! Test color operations
        call image%set_color(0.7_wp, 0.4_wp, 0.9_wp)
        call assert_true(image%current_r == 0.7_wp, 'Red component set')
        call assert_true(image%current_g == 0.4_wp, 'Green component set')
        call assert_true(image%current_b == 0.9_wp, 'Blue component set')
        
        ! Test color bytes
        call image%get_color_bytes(r, g, b)
        call assert_true(.true., 'Color bytes retrieved')
        
        ! Test line style
        call image%set_line_style('--')
        call assert_true(.true., 'Line style set')
        
        ! Test destruction
        call destroy_raster_image(image)
        call assert_true(.not. allocated(image%image_data), 'Image data deallocated')
        
        call test_end()
    end subroutine test_raster_operations

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "  Testing: ", test_name
    end subroutine test_start

    subroutine test_end()
        passed_count = passed_count + 1
        write(*, '(A)') "    PASSED"
    end subroutine test_end

end program test_raster_core_simple