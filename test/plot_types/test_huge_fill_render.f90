program test_huge_fill_render
    !! Filled polygons whose vertices reach ~1e21 must fill exactly their
    !! visible part under a small xlim/ylim, in PNG and PDF. Raster fills
    !! used to convert off-canvas coordinates with nint() (integer overflow,
    !! nothing drawn) and PDF emitted ~1e21 pt paths that viewers drop.
    !!
    !! Oracles (xlim 0..1, ylim -1..1; u, v are axes fractions, v upward):
    !! - fill_between(x, -1e21 x, 1e21 x): the band covers the whole axes.
    !! - fill(x, 1e21 (x - 1/2)): filled between y = 0 and the curve, i.e.
    !!   the quadrants u > 1/2, v > 1/2 and u < 1/2, v < 1/2 only.
    !! - bar(0.5, 1e21, width 0.5): 0.25 < u < 0.75 and v > 1/2 only.
    !! - pcolormesh with cell edges y = -1e21, 1e21: the whole axes.
    use fortplot
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use fortplot_test_raster_probe, only: rasterize_pdf, have_command
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer, parameter :: n = 101
    real(wp), parameter :: big = 1.0e21_wp
    real(wp), parameter :: axes_box(4) = [0.125_wp, 0.9_wp, 0.12_wp, 0.89_wp]
    real(wp), parameter :: red(3) = [1.0_wp, 0.0_wp, 0.0_wp]
    real(wp) :: x(n), z(1, 1)
    character(len=:), allocatable :: dir
    integer :: i, failures
    logical :: pdf_ok

    call ensure_test_output_dir('huge_fill_render', dir)
    x = [(real(i - 1, wp)/real(n - 1, wp), i = 1, n)]
    failures = 0
    pdf_ok = have_command('pdftoppm')
    if (.not. pdf_ok) print *, 'SKIPPED PDF part: pdftoppm not available'

    call figure()
    call fill_between(x, -big*x, big*x, color=red)
    call limits()
    call save_and_check(dir//'fill_between', [0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp], &
                        [0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp])

    call figure()
    call fill(x, big*(x - 0.5_wp), color=red)
    call limits()
    call save_and_check(dir//'fill_ur', [0.5_wp, 1.0_wp, 0.5_wp, 1.0_wp], &
                        [0.0_wp, 0.5_wp, 0.5_wp, 1.0_wp])
    call save_and_check(dir//'fill_ll', [0.0_wp, 0.5_wp, 0.0_wp, 0.5_wp], &
                        [0.5_wp, 1.0_wp, 0.0_wp, 0.5_wp])

    call figure()
    call bar([0.5_wp], [big], width=0.5_wp, color=red)
    call limits()
    call save_and_check(dir//'bar', [0.25_wp, 0.75_wp, 0.5_wp, 1.0_wp], &
                        [0.0_wp, 0.25_wp, 0.0_wp, 1.0_wp])

    call figure()
    z = 1.0_wp
    call pcolormesh([0.0_wp, 1.0_wp], [-big, big], z, vmin=0.0_wp, vmax=2.0_wp)
    call limits()
    call save_and_check(dir//'pcolormesh', [0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp], &
                        [0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp])

    if (failures > 0) then
        print '(a,i0,a)', ' FAIL: ', failures, ' check(s) failed'
        stop 1
    end if
    print *, 'PASS: huge-vertex fills cover exactly their visible region'

contains

    subroutine limits()
        call xlim(0.0_wp, 1.0_wp)
        call ylim(-1.0_wp, 1.0_wp)
    end subroutine limits

    subroutine save_and_check(stem, filled, empty)
        !! filled/empty = [u0, u1, v0, v1] axes-fraction boxes that must be
        !! (almost) all colour / all background; a zero-size empty box is
        !! skipped.
        character(len=*), intent(in) :: stem
        real(wp), intent(in) :: filled(4), empty(4)
        integer(1), allocatable :: img(:)
        integer :: w, h
        logical :: ok

        call savefig(stem//'.png')
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            call check(bk%raster%image_data, bk%width, bk%height, filled, &
                       empty, stem//'.png')
        end select
        call savefig(stem//'.pdf')
        if (.not. pdf_ok) return
        call rasterize_pdf(stem//'.pdf', 100, img, w, h, ok)
        if (.not. ok) then
            print *, 'FAIL: could not rasterise ', stem//'.pdf'
            failures = failures + 1
            return
        end if
        call check(img, w, h, filled, empty, stem//'.pdf')
    end subroutine save_and_check

    subroutine check(img, w, h, filled, empty, what)
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h
        real(wp), intent(in) :: filled(4), empty(4)
        character(len=*), intent(in) :: what
        real(wp) :: f_in, f_out

        f_in = colour_share(img, w, h, filled)
        f_out = 0.0_wp
        if (empty(2) > empty(1)) f_out = colour_share(img, w, h, empty)
        print '(1x,2a,f0.3,a,f0.3)', what, ': filled share ', f_in, &
            ', empty share ', f_out
        if (f_in < 0.98_wp .or. f_out > 0.02_wp) then
            print *, 'FAIL: wrong fill region in ', what
            failures = failures + 1
        end if
    end subroutine check

    real(wp) function colour_share(img, w, h, uv) result(share)
        !! Share of saturated (max - min channel > 60) pixels in the axes
        !! sub-box uv, shrunk by 3 % of the axes on each side so that edges,
        !! spines and anti-aliasing do not count.
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h
        real(wp), intent(in) :: uv(4)
        real(wp) :: aw, ah, c0, c1, r0, r1
        integer :: row, col, base, rgb(3), cnt, tot

        aw = axes_box(2) - axes_box(1)
        ah = axes_box(4) - axes_box(3)
        c0 = (axes_box(1) + (uv(1) + 0.03_wp)*aw)*(w - 1)
        c1 = (axes_box(1) + (uv(2) - 0.03_wp)*aw)*(w - 1)
        r0 = (axes_box(4) - (uv(4) - 0.03_wp)*ah)*(h - 1)
        r1 = (axes_box(4) - (uv(3) + 0.03_wp)*ah)*(h - 1)
        cnt = 0; tot = 0
        do row = ceiling(r0), floor(r1)
            do col = ceiling(c0), floor(c1)
                base = 3*(row*w + col)
                rgb = iand(int(img(base + 1:base + 3)), 255)
                tot = tot + 1
                if (maxval(rgb) - minval(rgb) > 60) cnt = cnt + 1
            end do
        end do
        share = real(cnt, wp)/real(max(tot, 1), wp)
    end function colour_share

end program test_huge_fill_render
