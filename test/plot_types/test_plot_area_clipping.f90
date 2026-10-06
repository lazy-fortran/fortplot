program test_plot_area_clipping
    !! Lines, fill_between areas and scatter markers whose data extend beyond
    !! xlim/ylim must be clipped to the axes rectangle, in PNG and PDF, on
    !! single axes and on subplots.
    !!
    !! Oracle: the data use saturated colours while axes, ticks and labels are
    !! grey, so no saturated pixel may appear in the figure margins or in the
    !! gap between subplots. The bands follow matplotlib's default subplot
    !! parameters (left 0.125, right 0.9, bottom 0.11, top 0.88, wspace 0.2)
    !! with a few pixels of slack. PDF output is rasterised by pdftoppm.
    use fortplot
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use fortplot_test_raster_probe, only: rasterize_pdf, have_command
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer, parameter :: n = 101
    real(wp) :: x(n)
    character(len=:), allocatable :: dir
    integer :: i, failures
    logical :: pdf_ok

    call ensure_test_output_dir('plot_area_clipping', dir)
    x = [(-2.0_wp + 5.0_wp*real(i - 1, wp)/real(n - 1, wp), i = 1, n)]
    failures = 0
    pdf_ok = have_command('pdftoppm')
    if (.not. pdf_ok) print *, 'SKIPPED PDF part: pdftoppm not available'

    call figure()
    call draw_panel()
    call save_and_check(dir//'single', .false.)

    call figure()
    call subplot(1, 2, 1)
    call draw_panel()
    call subplot(1, 2, 2)
    call draw_panel()
    call save_and_check(dir//'subplots', .true.)

    if (failures > 0) then
        print '(a,i0,a)', ' FAIL: ', failures, ' clipping check(s) failed'
        stop 1
    end if
    print *, 'PASS: data outside the limits stay inside the axes rectangle'

contains

    subroutine draw_panel()
        call fill_between(x, spread(-1.0_wp, 1, n), spread(2.0_wp, 1, n), &
                          color='green', alpha=0.4_wp)
        call plot(x, 2.0_wp*x - 0.5_wp, color=[1.0_wp, 0.0_wp, 0.0_wp])
        call scatter([-0.5_wp, 1.5_wp, 0.5_wp, 0.5_wp, 0.5_wp], &
                     [0.5_wp, 0.5_wp, -0.5_wp, 1.5_wp, 0.5_wp], &
                     color=[0.0_wp, 0.0_wp, 1.0_wp], markersize=20.0_wp)
        call xlim(0.0_wp, 1.0_wp)
        call ylim(0.0_wp, 1.0_wp)
    end subroutine draw_panel

    subroutine save_and_check(stem, two_panels)
        character(len=*), intent(in) :: stem
        logical, intent(in) :: two_panels
        integer(1), allocatable :: img(:)
        integer :: w, h
        logical :: ok

        call savefig(stem//'.png')
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            call check(bk%raster%image_data, bk%width, bk%height, two_panels, &
                       stem//'.png')
        class default
            print *, 'FAIL: expected a raster backend'
            failures = failures + 1
        end select
        if (.not. pdf_ok) return
        call savefig(stem//'.pdf')
        call rasterize_pdf(stem//'.pdf', 100, img, w, h, ok)
        if (.not. ok) then
            print *, 'FAIL: could not rasterise ', stem//'.pdf'
            failures = failures + 1
            return
        end if
        call check(img, w, h, two_panels, stem//'.pdf')
    end subroutine save_and_check

    subroutine check(img, w, h, two_panels, what)
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h
        logical, intent(in) :: two_panels
        character(len=*), intent(in) :: what
        integer, parameter :: slack = 4
        integer :: left, right, top, bottom, gap0, gap1, outside, inside
        real(wp) :: ax_w

        left = nint(0.125_wp*w) - slack
        right = nint(0.9_wp*w) + slack
        top = nint(0.12_wp*h) - slack
        bottom = nint(0.89_wp*h) + slack
        outside = saturated(img, w, h, 0, left - 1, 0, h - 1) + &
                  saturated(img, w, h, right + 1, w - 1, 0, h - 1) + &
                  saturated(img, w, h, 0, w - 1, 0, top - 1) + &
                  saturated(img, w, h, 0, w - 1, bottom + 1, h - 1)
        if (two_panels) then
            ax_w = 0.775_wp/2.2_wp
            gap0 = nint((0.125_wp + ax_w)*w) + slack
            gap1 = nint((0.125_wp + 1.2_wp*ax_w)*w) - slack
            outside = outside + saturated(img, w, h, gap0, gap1, 0, h - 1)
        end if
        inside = saturated(img, w, h, left + 2*slack, right - 2*slack, &
                           top + 2*slack, bottom - 2*slack)
        print '(1x,2a,i0,a,i0)', what, ': saturated pixels outside/inside ', &
            outside, ' / ', inside
        if (inside < 1000) then
            print *, 'FAIL: data missing inside the axes of ', what
            failures = failures + 1
        end if
        if (outside > 0) then
            print *, 'FAIL: data drawn outside the axes of ', what
            failures = failures + 1
        end if
    end subroutine check

    integer function saturated(img, w, h, c0, c1, r0, r1) result(count)
        !! Pixels whose channel spread exceeds 30 (not grey) in the region.
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h, c0, c1, r0, r1
        integer :: row, col, k, c(3)
        count = 0
        do row = max(0, r0), min(h - 1, r1)
            do col = max(0, c0), min(w - 1, c1)
                do k = 1, 3
                    c(k) = iand(int(img(3*(row*w + col) + k)), 255)
                end do
                if (maxval(c) - minval(c) > 30) count = count + 1
            end do
        end do
    end function saturated

end program test_plot_area_clipping
