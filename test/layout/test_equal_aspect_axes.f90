program test_equal_aspect_axes
    !! axis('equal') must give one data unit the same length on both axes, on
    !! single axes and on each subplot, in PNG and PDF output.
    !!
    !! Oracle: a unit circle drawn in a unique colour must cover a pixel
    !! bounding box whose width and height agree to within 2 pixels. PNG is
    !! probed in fortplot's raster buffer; PDF is rasterised by pdftoppm.
    !! Cases: automatic limits (limits adapt) and user xlim/ylim (box adapts).
    use fortplot
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use fortplot_test_raster_probe, only: color_bbox, rasterize_pdf, &
        have_command
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer, parameter :: n = 721
    integer, parameter :: red(3) = [255, 0, 0], blue(3) = [0, 0, 255]
    real(wp) :: t(n), cx(n), cy(n)
    character(len=:), allocatable :: dir
    integer :: i, failures
    logical :: pdf_ok

    call ensure_test_output_dir('equal_aspect_axes', dir)
    t = [(6.283185307179586_wp*real(i - 1, wp)/real(n - 1, wp), i = 1, n)]
    cx = cos(t); cy = sin(t)
    failures = 0
    pdf_ok = have_command('pdftoppm')
    if (.not. pdf_ok) print *, 'SKIPPED PDF part: pdftoppm not available'

    ! Single axes, automatic limits on a wide figure.
    call single(.false., dir//'single_auto')
    ! Single axes, user limits whose ratio differs from the box ratio.
    call single(.true., dir//'single_limits')
    ! Two subplots on a wide figure: automatic limits and user limits.
    call figure(figsize=[9.6_wp, 3.6_wp])
    call subplot(1, 2, 1)
    call plot(cx, cy, color=[1.0_wp, 0.0_wp, 0.0_wp])
    call axis('equal')
    call subplot(1, 2, 2)
    call plot(cx, cy, color=[0.0_wp, 0.0_wp, 1.0_wp])
    call xlim(-2.0_wp, 2.0_wp)
    call ylim(-1.2_wp, 1.2_wp)
    call axis('equal')
    call savefig(dir//'subplots.png')
    call check_raster('subplots left panel (PNG)', red)
    call check_raster('subplots right panel (PNG)', blue)
    if (pdf_ok) then
        call savefig(dir//'subplots.pdf')
        call check_pdf(dir//'subplots.pdf', 'subplots left panel (PDF)', red)
        call check_pdf(dir//'subplots.pdf', 'subplots right panel (PDF)', blue)
    end if

    if (failures > 0) then
        print '(a,i0,a)', ' FAIL: ', failures, ' equal-aspect check(s) failed'
        stop 1
    end if
    print *, 'PASS: unit circles render round under axis(''equal'')'

contains

    subroutine single(limits, stem)
        logical, intent(in) :: limits
        character(len=*), intent(in) :: stem
        call figure(figsize=[8.0_wp, 4.0_wp])
        call plot(cx, cy, color=[1.0_wp, 0.0_wp, 0.0_wp])
        if (limits) then
            call xlim(-1.5_wp, 1.5_wp)
            call ylim(-1.2_wp, 1.2_wp)
        end if
        call axis('equal')
        call savefig(stem//'.png')
        call check_raster(stem//' (PNG)', red)
        if (pdf_ok) then
            call savefig(stem//'.pdf')
            call check_pdf(stem//'.pdf', stem//' (PDF)', red)
        end if
    end subroutine single

    subroutine check_raster(what, rgb)
        character(len=*), intent(in) :: what
        integer, intent(in) :: rgb(3)
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            call check_image(what, bk%raster%image_data, bk%width, bk%height, rgb)
        class default
            print *, 'FAIL: expected a raster backend for ', what
            failures = failures + 1
        end select
    end subroutine check_raster

    subroutine check_pdf(pdf, what, rgb)
        character(len=*), intent(in) :: pdf, what
        integer, intent(in) :: rgb(3)
        integer(1), allocatable :: img(:)
        integer :: w, h
        logical :: ok
        call rasterize_pdf(pdf, 100, img, w, h, ok)
        if (.not. ok) then
            print *, 'FAIL: could not rasterise ', pdf
            failures = failures + 1
            return
        end if
        call check_image(what, img, w, h, rgb)
    end subroutine check_pdf

    subroutine check_image(what, img, w, h, rgb)
        character(len=*), intent(in) :: what
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h, rgb(3)
        integer :: x0, x1, y0, y1, count, bw, bh
        call color_bbox(img, w, h, rgb, 60, x0, x1, y0, y1, count)
        bw = x1 - x0 + 1; bh = y1 - y0 + 1
        print '(1x,a,a,i0,a,i0)', what, ': circle box ', bw, ' x ', bh
        if (count == 0 .or. bw < 40) then
            print *, 'FAIL: circle missing for ', what
            failures = failures + 1
        else if (abs(bw - bh) > 2) then
            print *, 'FAIL: circle not round for ', what
            failures = failures + 1
        end if
    end subroutine check_image

end program test_equal_aspect_axes
