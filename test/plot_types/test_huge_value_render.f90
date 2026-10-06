program test_huge_value_render
    !! Lines whose data reach ~1e21 must render in bounded time and in the
    !! right place, with and without a much smaller ylim, on single axes,
    !! subplots (PNG and PDF) and animation frames. Dashed segments used to
    !! be walked dash by dash over their full ~1e23 pixel length (PNG hang),
    !! and PDF emitted ~1e21 pt coordinates that viewers dash forever.
    !!
    !! Oracle: y = 1e21 (x - 1/2) crosses the window 0 <= y <= 1 only at
    !! x = 1/2, so with xlim(0, 1) the visible part is a vertical stroke at
    !! the horizontal centre of the axes spanning its full height (solid) or
    !! a fraction of it (dashed). A watchdog kills the test if it hangs.
    use fortplot
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_animation_rendering, only: extract_frame_rgb_data
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use fortplot_test_raster_probe, only: rasterize_pdf, have_command
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none

    interface
        function c_getpid() bind(C, name='getpid') result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_getpid
    end interface

    integer, parameter :: n = 201
    real(wp) :: x(n), y(n)
    character(len=:), allocatable :: dir
    integer :: i, failures
    logical :: pdf_ok
    integer(8) :: t0, t1, rate

    call ensure_test_output_dir('huge_value_render', dir)
    call start_watchdog(dir//'done', 90)
    call system_clock(t0, rate)
    x = [(real(i - 1, wp)/real(n - 1, wp), i = 1, n)]
    y = 1.0e21_wp*(x - 0.5_wp)
    failures = 0
    pdf_ok = have_command('pdftoppm')
    if (.not. pdf_ok) print *, 'SKIPPED PDF part: pdftoppm not available'

    call single_axes('-', 0.9_wp, 1.01_wp, 'single_solid')
    call single_axes('--', 0.3_wp, 0.95_wp, 'single_dashed')
    call single_axes(':', 0.1_wp, 0.9_wp, 'single_dotted')
    call unclipped_autoscale()
    call subplots_case()
    call animation_frame()

    call system_clock(t1)
    print '(a,f0.2,a)', ' elapsed ', real(t1 - t0, wp)/real(rate, wp), ' s'
    if (real(t1 - t0, wp)/real(rate, wp) > 60.0_wp) then
        print *, 'FAIL: rendering took longer than 60 s'
        failures = failures + 1
    end if
    call execute_command_line('touch "'//dir//'done"')
    if (failures > 0) then
        print '(a,i0,a)', ' FAIL: ', failures, ' check(s) failed'
        stop 1
    end if
    print *, 'PASS: huge-valued lines render in bounded time and place'

contains

    subroutine start_watchdog(sentinel, seconds)
        !! Kill this process (and a hung pdftoppm child) unless the sentinel
        !! appears within the given time. POSIX shells only.
        character(len=*), intent(in) :: sentinel
        integer, intent(in) :: seconds
        character(len=32) :: pid, secs

        if (.not. (have_command('kill') .and. have_command('sleep'))) then
            print *, 'NOTE: no watchdog (kill/sleep unavailable)'
            return
        end if
        write (pid, '(i0)') c_getpid()
        write (secs, '(i0)') seconds
        call execute_command_line('rm -f "'//sentinel//'"')
        call execute_command_line('( sleep '//trim(secs)//'; [ -f "'// &
            sentinel//'" ] || { echo "WATCHDOG: render hang" >&2; pkill -9 -P '// &
            trim(pid)//'; kill -9 '//trim(pid)//'; } ) >/dev/null 2>&1 &')
    end subroutine start_watchdog

    subroutine single_axes(style, cover_lo, cover_hi, stem)
        character(len=*), intent(in) :: style, stem
        real(wp), intent(in) :: cover_lo, cover_hi

        call figure()
        call plot(x, y, color=[1.0_wp, 0.0_wp, 0.0_wp], linestyle=style)
        call xlim(0.0_wp, 1.0_wp)
        call ylim(0.0_wp, 1.0_wp)
        call save_and_check(dir//stem, [0.125_wp, 0.9_wp, 0.12_wp, 0.89_wp], &
            cover_lo, cover_hi)
    end subroutine single_axes

    subroutine unclipped_autoscale()
        !! Without ylim the axis spans ~1e21; only bounded time is asserted
        !! plus a visible stroke inside the axes.
        integer :: cnt, c0, c1, rows

        call figure()
        call plot(x, y, color=[1.0_wp, 0.0_wp, 0.0_wp], linestyle='--')
        call savefig(dir//'autoscale.png')
        call savefig(dir//'autoscale.pdf')
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            call red_stats(bk%raster%image_data, bk%width, bk%height, &
                [0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp], cnt, c0, c1, rows)
            if (cnt < 100) then
                print *, 'FAIL: autoscaled huge line not drawn'
                failures = failures + 1
            end if
        end select
    end subroutine unclipped_autoscale

    subroutine subplots_case()
        call figure()
        call subplot(2, 1, 1)
        call plot(x, y, color=[1.0_wp, 0.0_wp, 0.0_wp], linestyle='--')
        call xlim(0.0_wp, 1.0_wp)
        call ylim(0.0_wp, 1.0_wp)
        call subplot(2, 1, 2)
        call plot(x, x, color=[0.0_wp, 0.0_wp, 0.0_wp])
        ! Top panel of a 2x1 grid with hspace 0.2: axes height 0.77/2.2.
        call save_and_check(dir//'subplots', &
            [0.125_wp, 0.9_wp, 0.12_wp, 0.12_wp + 0.77_wp/2.2_wp], 0.3_wp, 0.95_wp)
    end subroutine subplots_case

    subroutine animation_frame()
        type(figure_t) :: fig
        real(wp), allocatable :: rgb(:, :, :)
        integer(1), allocatable :: img(:)
        integer :: status, w, h, row, col, k

        w = 640; h = 480
        call fig%initialize(width=w, height=h)
        call fig%add_plot(x, y, linestyle='--', color=[1.0_wp, 0.0_wp, 0.0_wp])
        call fig%set_xlim(0.0_wp, 1.0_wp)
        call fig%set_ylim(0.0_wp, 1.0_wp)
        allocate (rgb(w, h, 3), img(3*w*h))
        call extract_frame_rgb_data(fig, rgb, status)
        do row = 0, h - 1
            do col = 0, w - 1
                do k = 1, 3
                    img(3*(row*w + col) + k) = &
                        int(nint(255.0_wp*rgb(col + 1, row + 1, k)), 1)
                end do
            end do
        end do
        call check(img, w, h, [0.125_wp, 0.9_wp, 0.12_wp, 0.89_wp], &
            0.3_wp, 0.95_wp, 'animation frame')
    end subroutine animation_frame

    subroutine save_and_check(stem, box, cover_lo, cover_hi)
        character(len=*), intent(in) :: stem
        real(wp), intent(in) :: box(4), cover_lo, cover_hi
        integer(1), allocatable :: img(:)
        integer :: w, h
        logical :: ok

        call savefig(stem//'.png')
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            call check(bk%raster%image_data, bk%width, bk%height, box, &
                cover_lo, cover_hi, stem//'.png')
        end select
        call savefig(stem//'.pdf')
        if (.not. pdf_ok) return
        call rasterize_pdf(stem//'.pdf', 100, img, w, h, ok)
        if (.not. ok) then
            print *, 'FAIL: could not rasterise ', stem//'.pdf'
            failures = failures + 1
            return
        end if
        ! PDF dashes use round caps that nearly close the gaps at 100 dpi, so
        ! only bound the stroke by the axes height there.
        call check(img, w, h, box, cover_lo, 1.01_wp, stem//'.pdf')
    end subroutine save_and_check

    subroutine check(img, w, h, box, cover_lo, cover_hi, what)
        !! box = [left, right, top, bottom] axes fractions (top from above).
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h
        real(wp), intent(in) :: box(4), cover_lo, cover_hi
        character(len=*), intent(in) :: what
        integer :: cnt, c0, c1, rows, centre
        real(wp) :: cover

        call red_stats(img, w, h, [0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp], cnt, c0, &
            c1, rows)
        centre = nint(0.5_wp*(box(1) + box(2))*w)
        cover = real(rows, wp)/((box(4) - box(3))*h)
        print '(1x,2a,i0,a,2(1x,i0),a,i0,a,f0.3)', what, ': red ', cnt, &
            ' cols', c0, c1, ' centre ', centre, ' cover ', cover
        if (cnt == 0 .or. c0 < centre - 6 .or. c1 > centre + 6) then
            print *, 'FAIL: stroke missing or off the x = 1/2 column in ', what
            failures = failures + 1
        else if (cover < cover_lo .or. cover > cover_hi) then
            print *, 'FAIL: stroke covers the wrong share of the axes in ', what
            failures = failures + 1
        end if
    end subroutine check

    subroutine red_stats(img, w, h, box, cnt, c0, c1, rows)
        !! Red pixels (r - max(g, b) > 60) inside the fractional box.
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h
        real(wp), intent(in) :: box(4)
        integer, intent(out) :: cnt, c0, c1, rows
        integer :: row, col, base, r, g, b
        logical :: hit

        cnt = 0; c0 = w; c1 = -1; rows = 0
        do row = nint(box(3)*(h - 1)), nint(box(4)*(h - 1))
            hit = .false.
            do col = nint(box(1)*(w - 1)), nint(box(2)*(w - 1))
                base = 3*(row*w + col)
                r = iand(int(img(base + 1)), 255)
                g = iand(int(img(base + 2)), 255)
                b = iand(int(img(base + 3)), 255)
                if (r - max(g, b) > 60) then
                    cnt = cnt + 1; hit = .true.
                    c0 = min(c0, col); c1 = max(c1, col)
                end if
            end do
            if (hit) rows = rows + 1
        end do
    end subroutine red_stats

end program test_huge_value_render
