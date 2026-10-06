program test_subplot_stem
    !! stem() must draw into the selected subplot, like plot() and step().
    !!
    !! Oracle: the left panel holds only a black reference line, the right
    !! panel only the stem plot, whose artists use the coloured default
    !! cycle. Coloured pixels must therefore appear inside the right axes
    !! and nowhere in the left half. Panels follow matplotlib's default
    !! subplot parameters (left 0.125, right 0.9, wspace 0.2).
    use fortplot
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use fortplot_test_raster_probe, only: rasterize_pdf, have_command
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    character(len=:), allocatable :: dir
    integer(1), allocatable :: img(:)
    integer :: failures, w, h
    logical :: ok

    call ensure_test_output_dir('subplot_stem', dir)
    failures = 0

    call figure()
    call subplot(1, 2, 1)
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp], color=[0.0_wp, 0.0_wp, 0.0_wp])
    call subplot(1, 2, 2)
    call stem([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], [1.0_wp, 3.0_wp, 2.0_wp, 4.0_wp])
    call savefig(dir//'stem.png')
    select type (bk => global_figure%state%backend)
    class is (raster_context)
        call check(bk%raster%image_data, bk%width, bk%height, 'PNG')
    class default
        print *, 'FAIL: expected a raster backend'
        failures = failures + 1
    end select

    call savefig(dir//'stem.pdf')
    if (have_command('pdftoppm')) then
        call rasterize_pdf(dir//'stem.pdf', 100, img, w, h, ok)
        if (ok) then
            call check(img, w, h, 'PDF')
        else
            print *, 'FAIL: could not rasterise stem.pdf'
            failures = failures + 1
        end if
    else
        print *, 'SKIPPED PDF part: pdftoppm not available'
    end if

    if (failures > 0) then
        print '(a,i0,a)', ' FAIL: ', failures, ' check(s) failed'
        stop 1
    end if
    print *, 'PASS: stem() draws into the selected subplot'

contains

    subroutine check(img, w, h, what)
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h
        character(len=*), intent(in) :: what
        integer :: left_half, right_axes
        real(wp) :: ax_w

        ax_w = 0.775_wp/2.2_wp
        left_half = coloured(img, w, h, 0, w/2 - 1)
        right_axes = coloured(img, w, h, nint((0.125_wp + 1.2_wp*ax_w)*w) + 3, &
            nint(0.9_wp*w) - 3)
        print '(1x,2a,i0,a,i0)', what, ': coloured pixels left/right ', &
            left_half, ' / ', right_axes
        if (right_axes < 100) then
            print *, 'FAIL: stem missing from the selected subplot in ', what
            failures = failures + 1
        end if
        if (left_half > 0) then
            print *, 'FAIL: coloured pixels in the other subplot in ', what
            failures = failures + 1
        end if
    end subroutine check

    integer function coloured(img, w, h, c0, c1) result(count)
        !! Pixels whose channel spread exceeds 60 within columns c0..c1.
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h, c0, c1
        integer :: row, col, k, c(3)

        count = 0
        do row = 0, h - 1
            do col = max(0, c0), min(w - 1, c1)
                do k = 1, 3
                    c(k) = iand(int(img(3*(row*w + col) + k)), 255)
                end do
                if (maxval(c) - minval(c) > 60) count = count + 1
            end do
        end do
    end function coloured

end program test_subplot_stem
