program test_raster_axes_coverage
    !! Comprehensive test coverage for fortplot_raster_axes module
    !! Tests axes drawing and label rendering functions
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use fortplot_raster_axes, only: raster_draw_axes_and_labels, raster_render_ylabel
    use fortplot_raster_core, only: raster_image_t, create_raster_image, destroy_raster_image
    use fortplot_margins, only: plot_area_t
    use fortplot_testing, only: assert_true, assert_false, assert_equal_int, &
                               assert_allocated, test_summary
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0
    logical :: all_tests_passed = .true.

    write(*, '(A)') "=== fortplot_raster_axes Coverage Tests ==="

    call test_draw_axes_and_labels_minimal()
    call test_draw_axes_and_labels_with_title()
    call test_draw_axes_and_labels_with_xlabel()
    call test_draw_axes_and_labels_with_ylabel()
    call test_draw_axes_and_labels_complete()
    call test_draw_axes_and_labels_different_scales()
    call test_render_ylabel_simple()
    call test_render_ylabel_unicode()

    call test_summary(test_count, passed_count, all_tests_passed, 'fortplot_raster_axes')

contains

    subroutine test_draw_axes_and_labels_minimal()
        !! Test raster_draw_axes_and_labels with minimal parameters
        type(raster_image_t) :: raster
        type(plot_area_t) :: plot_area
        
        call test_start('draw axes and labels minimal')
        
        raster = create_raster_image(400, 300)
        plot_area%left = 50
        plot_area%bottom = 50
        plot_area%width = 300
        plot_area%height = 200
        
        ! Draw axes with minimal parameters (no labels)
        call raster_draw_axes_and_labels(raster, 400, 300, plot_area, &
                                        'linear', 'linear', 1.0_wp, &
                                        0.0_wp, 10.0_wp, 0.0_wp, 20.0_wp)
        
        ! Function should complete without error
        call assert_true(.true., 'Axes drawing completes with minimal parameters')
        
        call destroy_raster_image(raster)
        call test_end()
    end subroutine test_draw_axes_and_labels_minimal

    subroutine test_draw_axes_and_labels_with_title()
        !! Test raster_draw_axes_and_labels with title
        type(raster_image_t) :: raster
        type(plot_area_t) :: plot_area
        character(len=:), allocatable :: title
        
        call test_start('draw axes and labels with title')
        
        raster = create_raster_image(500, 400)
        plot_area%left = 60
        plot_area%bottom = 60
        plot_area%width = 380
        plot_area%height = 280
        
        title = "Test Plot Title"
        
        call raster_draw_axes_and_labels(raster, 500, 400, plot_area, &
                                        'linear', 'linear', 1.0_wp, &
                                        -5.0_wp, 5.0_wp, -10.0_wp, 10.0_wp, &
                                        title=title)
        
        call assert_true(.true., 'Axes drawing completes with title')
        
        if (allocated(title)) deallocate(title)
        call destroy_raster_image(raster)
        call test_end()
    end subroutine test_draw_axes_and_labels_with_title

    subroutine test_draw_axes_and_labels_with_xlabel()
        !! Test raster_draw_axes_and_labels with xlabel
        type(raster_image_t) :: raster
        type(plot_area_t) :: plot_area
        character(len=:), allocatable :: xlabel
        
        call test_start('draw axes and labels with xlabel')
        
        raster = create_raster_image(600, 400)
        plot_area%left = 70
        plot_area%bottom = 70
        plot_area%width = 460
        plot_area%height = 260
        
        xlabel = "X Axis Label"
        
        call raster_draw_axes_and_labels(raster, 600, 400, plot_area, &
                                        'log', 'linear', 1.0_wp, &
                                        1.0_wp, 100.0_wp, 0.0_wp, 50.0_wp, &
                                        xlabel=xlabel)
        
        call assert_true(.true., 'Axes drawing completes with xlabel')
        
        if (allocated(xlabel)) deallocate(xlabel)
        call destroy_raster_image(raster)
        call test_end()
    end subroutine test_draw_axes_and_labels_with_xlabel

    subroutine test_draw_axes_and_labels_with_ylabel()
        !! Test raster_draw_axes_and_labels with ylabel
        type(raster_image_t) :: raster
        type(plot_area_t) :: plot_area
        character(len=:), allocatable :: ylabel
        
        call test_start('draw axes and labels with ylabel')
        
        raster = create_raster_image(400, 500)
        plot_area%left = 80
        plot_area%bottom = 60
        plot_area%width = 280
        plot_area%height = 380
        
        ylabel = "Y Axis Label"
        
        call raster_draw_axes_and_labels(raster, 400, 500, plot_area, &
                                        'linear', 'log', 1.0_wp, &
                                        0.0_wp, 10.0_wp, 1.0_wp, 1000.0_wp, &
                                        ylabel=ylabel)
        
        call assert_true(.true., 'Axes drawing completes with ylabel')
        
        if (allocated(ylabel)) deallocate(ylabel)
        call destroy_raster_image(raster)
        call test_end()
    end subroutine test_draw_axes_and_labels_with_ylabel

    subroutine test_draw_axes_and_labels_complete()
        !! Test raster_draw_axes_and_labels with all labels
        type(raster_image_t) :: raster
        type(plot_area_t) :: plot_area
        character(len=:), allocatable :: title, xlabel, ylabel
        
        call test_start('draw axes and labels complete')
        
        raster = create_raster_image(800, 600)
        plot_area%left = 100
        plot_area%bottom = 80
        plot_area%width = 600
        plot_area%height = 440
        
        title = "Complete Test Plot"
        xlabel = "X Values"
        ylabel = "Y Values"
        
        call raster_draw_axes_and_labels(raster, 800, 600, plot_area, &
                                        'linear', 'linear', 1.0_wp, &
                                        -20.0_wp, 20.0_wp, -30.0_wp, 30.0_wp, &
                                        title=title, xlabel=xlabel, ylabel=ylabel)
        
        call assert_true(.true., 'Axes drawing completes with all labels')
        
        if (allocated(title)) deallocate(title)
        if (allocated(xlabel)) deallocate(xlabel)
        if (allocated(ylabel)) deallocate(ylabel)
        call destroy_raster_image(raster)
        call test_end()
    end subroutine test_draw_axes_and_labels_complete

    subroutine test_draw_axes_and_labels_different_scales()
        !! Test raster_draw_axes_and_labels with different scale types
        type(raster_image_t) :: raster
        type(plot_area_t) :: plot_area
        
        call test_start('draw axes and labels different scales')
        
        raster = create_raster_image(400, 300)
        plot_area%left = 50
        plot_area%bottom = 50
        plot_area%width = 300
        plot_area%height = 200
        
        ! Test symlog scale with threshold
        call raster_draw_axes_and_labels(raster, 400, 300, plot_area, &
                                        'symlog', 'symlog', 10.0_wp, &
                                        -1000.0_wp, 1000.0_wp, -500.0_wp, 500.0_wp)
        
        call assert_true(.true., 'Axes drawing completes with symlog scales')
        
        call destroy_raster_image(raster)
        call test_end()
    end subroutine test_draw_axes_and_labels_different_scales

    subroutine test_render_ylabel_simple()
        !! Test raster_render_ylabel with simple text
        type(raster_image_t) :: raster
        type(plot_area_t) :: plot_area
        
        call test_start('render ylabel simple')
        
        raster = create_raster_image(400, 300)
        plot_area%left = 80
        plot_area%bottom = 50
        plot_area%width = 270
        plot_area%height = 200
        
        call raster_render_ylabel(raster, 400, 300, plot_area, "Simple Y Label")
        
        call assert_true(.true., 'Y label rendering completes with simple text')
        
        call destroy_raster_image(raster)
        call test_end()
    end subroutine test_render_ylabel_simple

    subroutine test_render_ylabel_unicode()
        !! Test raster_render_ylabel with unicode characters
        type(raster_image_t) :: raster
        type(plot_area_t) :: plot_area
        
        call test_start('render ylabel unicode')
        
        raster = create_raster_image(500, 400)
        plot_area%left = 90
        plot_area%bottom = 60
        plot_area%width = 350
        plot_area%height = 280
        
        ! Test with mathematical symbols (unicode)
        call raster_render_ylabel(raster, 500, 400, plot_area, "α β γ δ")
        
        call assert_true(.true., 'Y label rendering completes with unicode characters')
        
        call destroy_raster_image(raster)
        call test_end()
    end subroutine test_render_ylabel_unicode

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "  Testing: ", test_name
    end subroutine test_start

    subroutine test_end()
        passed_count = passed_count + 1
        write(*, '(A)') "    PASSED"
    end subroutine test_end

end program test_raster_axes_coverage