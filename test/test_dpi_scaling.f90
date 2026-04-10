program test_dpi_scaling
    !! Test DPI-aware scaling for raster backend (issue #1572)
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: REFERENCE_DPI
    use fortplot_raster_core, only: raster_image_t, create_raster_image, &
                                    scale_px, pt2px
    use fortplot_raster, only: raster_context, create_raster_canvas
    use fortplot
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    logical :: all_passed
    character(len=:), allocatable :: output_dir

    all_passed = .true.
    call ensure_test_output_dir('dpi_scaling', output_dir)

    call test_pt2px_conversion()
    call test_scale_px_conversion()
    call test_raster_image_dpi_default()
    call test_raster_image_dpi_custom()
    call test_raster_canvas_dpi()
    call test_line_width_dpi_scaling()
    call test_default_line_width()
    call test_dpi_100_output()
    call test_dpi_200_output()

    if (all_passed) then
        print *, 'All DPI scaling tests passed'
    else
        print *, 'FAIL: Some DPI scaling tests failed'
        error stop 1
    end if

contains

    subroutine test_pt2px_conversion()
        real(wp) :: px
        print *, 'Testing pt2px conversion...'
        ! 1pt at 72 DPI = 1px
        px = pt2px(1.0_wp, 72.0_wp)
        if (abs(px - 1.0_wp) > 1e-10_wp) then
            print *, 'FAIL: pt2px(1, 72) =', px, 'expected 1.0'
            all_passed = .false.
            return
        end if
        ! 1pt at 100 DPI = 100/72 px
        px = pt2px(1.0_wp, 100.0_wp)
        if (abs(px - 100.0_wp/72.0_wp) > 1e-10_wp) then
            print *, 'FAIL: pt2px(1, 100) =', px
            all_passed = .false.
            return
        end if
        ! 1.5pt at 100 DPI = ~2.08px (matplotlib default)
        px = pt2px(1.5_wp, 100.0_wp)
        if (abs(px - 1.5_wp * 100.0_wp / 72.0_wp) > 1e-10_wp) then
            print *, 'FAIL: pt2px(1.5, 100) =', px
            all_passed = .false.
            return
        end if
        print *, '  PASS'
    end subroutine test_pt2px_conversion

    subroutine test_scale_px_conversion()
        integer :: s
        print *, 'Testing scale_px conversion...'
        ! At reference DPI, scale is 1:1
        s = scale_px(5, REFERENCE_DPI)
        if (s /= 5) then
            print *, 'FAIL: scale_px(5, 100) =', s, 'expected 5'
            all_passed = .false.
            return
        end if
        ! At 200 DPI, pixels double
        s = scale_px(5, 200.0_wp)
        if (s /= 10) then
            print *, 'FAIL: scale_px(5, 200) =', s, 'expected 10'
            all_passed = .false.
            return
        end if
        ! At 50 DPI, pixels halve
        s = scale_px(10, 50.0_wp)
        if (s /= 5) then
            print *, 'FAIL: scale_px(10, 50) =', s, 'expected 5'
            all_passed = .false.
            return
        end if
        print *, '  PASS'
    end subroutine test_scale_px_conversion

    subroutine test_raster_image_dpi_default()
        type(raster_image_t) :: img
        print *, 'Testing raster image default DPI...'
        img = create_raster_image(100, 100)
        if (abs(img%dpi - REFERENCE_DPI) > 1e-10_wp) then
            print *, 'FAIL: default DPI =', img%dpi, 'expected', REFERENCE_DPI
            all_passed = .false.
            return
        end if
        print *, '  PASS'
    end subroutine test_raster_image_dpi_default

    subroutine test_raster_image_dpi_custom()
        type(raster_image_t) :: img
        print *, 'Testing raster image custom DPI...'
        img = create_raster_image(100, 100, 200.0_wp)
        if (abs(img%dpi - 200.0_wp) > 1e-10_wp) then
            print *, 'FAIL: custom DPI =', img%dpi, 'expected 200'
            all_passed = .false.
            return
        end if
        print *, '  PASS'
    end subroutine test_raster_image_dpi_custom

    subroutine test_raster_canvas_dpi()
        type(raster_context) :: ctx
        print *, 'Testing raster canvas DPI propagation...'
        ctx = create_raster_canvas(640, 480, 150.0_wp)
        if (abs(ctx%raster%dpi - 150.0_wp) > 1e-10_wp) then
            print *, 'FAIL: canvas DPI =', ctx%raster%dpi, 'expected 150'
            all_passed = .false.
            return
        end if
        print *, '  PASS'
    end subroutine test_raster_canvas_dpi

    subroutine test_line_width_dpi_scaling()
        type(raster_context) :: ctx100, ctx200
        print *, 'Testing line width DPI scaling...'
        ctx100 = create_raster_canvas(640, 480, 100.0_wp)
        ctx200 = create_raster_canvas(640, 480, 200.0_wp)

        ! Set same point-based width on both
        call ctx100%set_line_width(1.5_wp)
        call ctx200%set_line_width(1.5_wp)

        ! At 200 DPI, pixel width should be ~2x the 100 DPI width
        if (abs(ctx200%raster%current_line_width / &
                ctx100%raster%current_line_width - 2.0_wp) > 0.1_wp) then
            print *, 'FAIL: line width ratio =', &
                ctx200%raster%current_line_width / ctx100%raster%current_line_width
            all_passed = .false.
            return
        end if
        print *, '  PASS'
    end subroutine test_line_width_dpi_scaling

    subroutine test_default_line_width()
        real(wp) :: x(10), y(10)
        integer :: i
        print *, 'Testing default line width is 1.5pt...'
        x = [(real(i, wp), i = 1, 10)]
        y = x * 2.0_wp

        call figure()
        call plot(x, y)
        call savefig(trim(output_dir)//'default_linewidth.png')
        print *, '  PASS (output generated)'
    end subroutine test_default_line_width

    subroutine test_dpi_100_output()
        real(wp) :: x(50), y(50)
        integer :: i
        print *, 'Testing DPI=100 output...'
        x = [(real(i, wp) * 0.2_wp, i = 1, 50)]
        y = sin(x)

        call figure(dpi=100)
        call plot(x, y, label='sin(x)')
        call title('DPI=100')
        call xlabel('x')
        call ylabel('y')
        call legend()
        call savefig(trim(output_dir)//'dpi_100.png')
        print *, '  PASS'
    end subroutine test_dpi_100_output

    subroutine test_dpi_200_output()
        real(wp) :: x(50), y(50)
        integer :: i
        print *, 'Testing DPI=200 output...'
        x = [(real(i, wp) * 0.2_wp, i = 1, 50)]
        y = sin(x)

        call figure(dpi=200)
        call plot(x, y, label='sin(x)')
        call title('DPI=200')
        call xlabel('x')
        call ylabel('y')
        call legend()
        call savefig(trim(output_dir)//'dpi_200.png')
        print *, '  PASS'
    end subroutine test_dpi_200_output

end program test_dpi_scaling
