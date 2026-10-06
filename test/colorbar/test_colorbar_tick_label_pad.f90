program test_colorbar_tick_label_pad
    !! Colorbar tick labels must keep matplotlib's clearance from the bar:
    !! an outward tick of 3.5 pt plus a 3.5 pt pad, 7 pt in all (9.7 px at
    !! 100 dpi), not sit flush against it.
    !!
    !! Oracle: in each image the bar is the rightmost run of saturated
    !! (colormap) pixels. Along the bar rows, the median of the first dark
    !! pixel after the outline (and tick) is the start of the tick labels.
    !! Its distance from the bar edge must be at least MIN_GAP_PX (5 pt). Single axes and
    !! a subplot panel, PNG from the raster buffer, PDF through pdftoppm.
    use fortplot
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use fortplot_test_raster_probe, only: rasterize_pdf, have_command
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer, parameter :: MIN_GAP_PX = 7
    real(wp) :: x(11), y(9), c(8, 10)
    character(len=:), allocatable :: dir
    integer :: i, j, failures
    logical :: pdf_ok

    call ensure_test_output_dir('colorbar_tick_label_pad', dir)
    x = [(0.1_wp*real(i - 1, wp), i = 1, 11)]
    y = [(0.125_wp*real(j - 1, wp), j = 1, 9)]
    do j = 1, 8
        do i = 1, 10
            c(j, i) = real(i + j, wp)/18.0_wp
        end do
    end do
    failures = 0
    pdf_ok = have_command('pdftoppm')
    if (.not. pdf_ok) print *, 'SKIPPED PDF part: pdftoppm not available'

    call figure()
    call pcolormesh(x, y, c, cmap='viridis', vmin=0.0_wp, vmax=1.0_wp)
    call colorbar()
    call save_and_check(dir//'single')

    call figure(figsize=[8.0_wp, 4.0_wp])
    call subplot(1, 2, 1)
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp], color=[0.5_wp, 0.5_wp, 0.5_wp])
    call subplot(1, 2, 2)
    call pcolormesh(x, y, c, cmap='viridis', vmin=0.0_wp, vmax=1.0_wp)
    call colorbar()
    call save_and_check(dir//'subplot')

    ! Narrow bars of a 2x2 grid, automatic and explicit ticks.
    call grid_case(.false., dir//'grid_auto')
    call grid_case(.true., dir//'grid_custom')

    if (failures > 0) then
        print '(a,i0,a)', ' FAIL: ', failures, ' colorbar pad check(s) failed'
        stop 1
    end if
    print *, 'PASS: colorbar tick labels clear the bar'

contains

    subroutine grid_case(custom, stem)
        logical, intent(in) :: custom
        character(len=*), intent(in) :: stem
        call figure()
        call subplot(2, 2, 1)
        call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp], color=[0.5_wp, 0.5_wp, 0.5_wp])
        call subplot(2, 2, 4)
        call pcolormesh(x, y, c, cmap='viridis', vmin=0.0_wp, vmax=1.0_wp)
        if (custom) then
            call colorbar(label='density', ticks=[0.0_wp, 0.5_wp, 1.0_wp], &
                ticklabels=['low ', 'mid ', 'high'])
        else
            call colorbar(label='density')
        end if
        call save_and_check(stem)
    end subroutine grid_case

    subroutine save_and_check(stem)
        character(len=*), intent(in) :: stem
        integer(1), allocatable :: img(:)
        integer :: w, h
        logical :: ok

        call savefig(stem//'.png')
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            call check(bk%raster%image_data, bk%width, bk%height, stem//'.png')
        class default
            error stop 'expected a raster backend'
        end select
        if (.not. pdf_ok) return
        call savefig(stem//'.pdf')
        call rasterize_pdf(stem//'.pdf', 100, img, w, h, ok)
        if (.not. ok) error stop 'pdftoppm failed'
        call check(img, w, h, stem//'.pdf')
    end subroutine save_and_check

    subroutine check(img, w, h, what)
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h
        character(len=*), intent(in) :: what
        integer, parameter :: WINDOW = 24
        integer :: row, col, bar_right, r0, r1, n, gap
        integer :: first(h)
        logical :: outline

        ! Rightmost saturated column and the rows it spans.
        bar_right = -1
        do col = w - 1, 0, -1
            do row = 0, h - 1
                if (spread(img, w, col, row) > 30) then
                    bar_right = col
                    exit
                end if
            end do
            if (bar_right >= 0) exit
        end do
        if (bar_right < 0) error stop 'no colorbar found'
        r0 = h; r1 = -1
        do row = 0, h - 1
            if (spread(img, w, bar_right - 2, row) > 30) then
                r0 = min(r0, row); r1 = max(r1, row)
            end if
        end do

        ! Per bar row, the first ink column after the dark run of the bar
        ! outline (and tick mark, on tick rows) within WINDOW px; the median
        ! over rows is the label start.
        n = 0
        do row = r0, r1
            outline = .true.
            do col = bar_right + 1, min(w - 1, bar_right + WINDOW)
                if (outline) then
                    outline = dark(img, w, col, row)
                else if (dark(img, w, col, row)) then
                    n = n + 1
                    first(n) = col
                    exit
                end if
            end do
        end do
        if (n < 5) then
            print *, 'FAIL: no colorbar tick labels found in ', what
            failures = failures + 1
            return
        end if
        call sort(first(1:n))
        gap = first((n + 1)/2) - bar_right - 1
        print '(1x,2a,i0)', what, ': tick label gap from bar (px) ', gap
        if (gap < MIN_GAP_PX) then
            print *, 'FAIL: colorbar tick labels crowd the bar in ', what
            failures = failures + 1
        end if
    end subroutine check

    integer function spread(img, w, col, row) result(s)
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, col, row
        integer :: k, c(3)
        do k = 1, 3
            c(k) = iand(int(img(3*(row*w + col) + k)), 255)
        end do
        s = maxval(c) - minval(c)
    end function spread

    logical function dark(img, w, col, row)
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, col, row
        integer :: k, c(3)
        do k = 1, 3
            c(k) = iand(int(img(3*(row*w + col) + k)), 255)
        end do
        dark = maxval(c) < 160
    end function dark

    subroutine sort(v)
        integer, intent(inout) :: v(:)
        integer :: i, j, t
        do i = 2, size(v)
            t = v(i)
            j = i - 1
            do while (j >= 1)
                if (v(j) <= t) exit
                v(j + 1) = v(j)
                j = j - 1
            end do
            v(j + 1) = t
        end do
    end subroutine sort

end program test_colorbar_tick_label_pad
