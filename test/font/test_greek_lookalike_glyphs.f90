program test_greek_lookalike_glyphs
    !! Greek letters that resemble Latin ones must stay distinguishable:
    !! nu/v, upsilon/u, rho/p and kappa/k, as unicode text and as mathtext
    !! (\nu ...), in PNG and PDF. (Omicron is drawn as an o in every typeface
    !! and is not tested.)
    !!
    !! Oracle: each letter is drawn alone at 48 pt in an empty axes; its ink
    !! (pixels darker than 128) is aligned at the bounding-box corner and
    !! compared with the Latin letter. The dissimilarity |A xor B| / |A or B|
    !! must reach MIN_DIFF; a font whose nu is a plain v gives about 0.1.
    !! A Greek ink box over twice the Latin width means the command was drawn
    !! literally (e.g. "\nu") instead of as one glyph.
    !! PDF output is rasterised by pdftoppm.
    use fortplot
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use fortplot_test_raster_probe, only: rasterize_pdf, have_command
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    real(wp), parameter :: MIN_DIFF = 0.3_wp
    integer, parameter :: np = 4
    character(len=12) :: greek(np), mathtext(np), latin(np)
    character(len=:), allocatable :: dir
    integer :: k, failures
    logical :: pdf_ok

    greek = [character(len=8) :: 'ν', 'υ', 'ρ', 'κ']
    mathtext = [character(len=12) :: '$\nu$', '$\upsilon$', '$\rho$', '$\kappa$']
    latin = [character(len=8) :: 'v', 'u', 'p', 'k']
    call ensure_test_output_dir('greek_lookalike_glyphs', dir)
    failures = 0
    pdf_ok = have_command('pdftoppm')
    if (.not. pdf_ok) print *, 'SKIPPED PDF part: pdftoppm not available'

    do k = 1, np
        call compare(trim(greek(k)), trim(latin(k)), 'unicode '//trim(latin(k)), k)
        call compare(trim(mathtext(k)), trim(latin(k)), &
                     'mathtext '//trim(mathtext(k)), k + np)
    end do

    if (failures > 0) then
        print '(a,i0,a)', ' FAIL: ', failures, ' look-alike check(s) failed'
        stop 1
    end if
    print *, 'PASS: Greek look-alikes are distinct from Latin letters'

contains

    subroutine compare(g, l, what, tag)
        character(len=*), intent(in) :: g, l, what
        integer, intent(in) :: tag
        logical, allocatable :: mg(:, :), ml(:, :)
        character(len=8) :: stem
        real(wp) :: d

        write (stem, '(a,i0)') 'case', tag
        call ink(g, dir//trim(stem)//'_greek', .false., mg)
        call ink(l, dir//trim(stem)//'_latin', .false., ml)
        d = dissimilarity(mg, ml)
        print '(1x,a,a,f6.3)', what, ' PNG dissimilarity ', d
        if (size(mg, 1) > 2*size(ml, 1)) then
            print *, 'FAIL: PNG draws more than one glyph for ', what
            failures = failures + 1
        else if (d < MIN_DIFF) then
            print *, 'FAIL: PNG Greek glyph looks like its Latin twin: ', what
            failures = failures + 1
        end if
        if (.not. pdf_ok) return
        call ink(g, dir//trim(stem)//'_greek', .true., mg)
        call ink(l, dir//trim(stem)//'_latin', .true., ml)
        d = dissimilarity(mg, ml)
        print '(1x,a,a,f6.3)', what, ' PDF dissimilarity ', d
        if (size(mg, 1) > 2*size(ml, 1)) then
            print *, 'FAIL: PDF draws more than one glyph for ', what
            failures = failures + 1
        else if (d < MIN_DIFF) then
            print *, 'FAIL: PDF Greek glyph looks like its Latin twin: ', what
            failures = failures + 1
        end if
    end subroutine compare

    subroutine ink(s, stem, pdf, mask)
        !! Ink mask of s drawn at the centre of an empty axes, cropped to its
        !! bounding box.
        character(len=*), intent(in) :: s, stem
        logical, intent(in) :: pdf
        logical, allocatable, intent(out) :: mask(:, :)
        integer(1), allocatable :: img(:)
        integer :: w, h
        logical :: ok

        call figure(figsize=[4.0_wp, 3.0_wp])
        call xlim(0.0_wp, 1.0_wp)
        call ylim(0.0_wp, 1.0_wp)
        call text(0.5_wp, 0.5_wp, s, font_size=48.0_wp, ha='center')
        if (pdf) then
            call savefig(stem//'.pdf')
            call rasterize_pdf(stem//'.pdf', 100, img, w, h, ok)
            if (.not. ok) error stop 'pdftoppm failed'
            call crop(img, w, h, mask)
        else
            call savefig(stem//'.png')
            select type (bk => global_figure%state%backend)
            class is (raster_context)
                call crop(bk%raster%image_data, bk%width, bk%height, mask)
            class default
                error stop 'expected a raster backend'
            end select
        end if
    end subroutine ink

    subroutine crop(img, w, h, mask)
        integer(1), intent(in) :: img(:)
        integer, intent(in) :: w, h
        logical, allocatable, intent(out) :: mask(:, :)
        logical :: full(0:w - 1, 0:h - 1)
        integer :: row, col, k, c(3), x0, x1, y0, y1
        full = .false.
        do row = h/5, 4*h/5
            do col = w/5, 4*w/5
                do k = 1, 3
                    c(k) = iand(int(img(3*(row*w + col) + k)), 255)
                end do
                full(col, row) = minval(c) < 128
            end do
        end do
        if (.not. any(full)) error stop 'no ink found'
        x0 = w; x1 = -1; y0 = h; y1 = -1
        do row = 0, h - 1
            do col = 0, w - 1
                if (.not. full(col, row)) cycle
                x0 = min(x0, col); x1 = max(x1, col)
                y0 = min(y0, row); y1 = max(y1, row)
            end do
        end do
        mask = full(x0:x1, y0:y1)
    end subroutine crop

    real(wp) function dissimilarity(a, b) result(d)
        !! Masks aligned at the bottom-left corner of their bounding boxes.
        logical, intent(in) :: a(:, :), b(:, :)
        integer :: nx, ny, i, j, n_or, n_xor
        logical :: pa, pb
        nx = max(size(a, 1), size(b, 1))
        ny = max(size(a, 2), size(b, 2))
        n_or = 0; n_xor = 0
        do j = 1, ny
            do i = 1, nx
                pa = pick(a, i, j, ny)
                pb = pick(b, i, j, ny)
                if (pa .or. pb) n_or = n_or + 1
                if (pa .neqv. pb) n_xor = n_xor + 1
            end do
        end do
        d = real(n_xor, wp)/real(max(1, n_or), wp)
    end function dissimilarity

    logical function pick(m, i, j, ny) result(p)
        !! Pixel (i, j) of m in a frame of height ny, bottom aligned.
        logical, intent(in) :: m(:, :)
        integer, intent(in) :: i, j, ny
        integer :: jj
        p = .false.
        jj = j - (ny - size(m, 2))
        if (i > size(m, 1)) return
        if (jj < 1) return
        p = m(i, jj)
    end function pick

end program test_greek_lookalike_glyphs
