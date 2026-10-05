program test_scientific_glyph_coverage
    !! Common scientific label glyphs must render as real glyphs, not as the
    !! font's missing-glyph box, in both raster and PDF output.
    !!
    !! Raster oracle: each glyph is rendered alone and compared with the
    !! rendering of U+E000 (private use, never mapped), i.e. the .notdef box
    !! of the default font; it must differ and leave ink.
    !! PDF oracle: pdftotext of a figure whose title holds the glyphs yields
    !! no '?' replacement characters and contains the Symbol-font glyphs.
    use fortplot
    use fortplot_text_rendering, only: render_text_to_image
    use fortplot_text_fonts, only: init_text_system
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer, parameter :: w = 48, h = 40, ng = 31
    character(len=4) :: glyphs(ng)
    character(len=:), allocatable :: dir, title_text
    integer(1), allocatable :: box(:), img(:)
    integer :: k, failures

    glyphs = [character(len=4) :: '→', '←', '↑', '↓', '⇒', '±', '×', '·', '≈', &
              '≤', '≥', '∞', '∂', '∇', 'ħ', 'α', 'ω', 'Δ', 'Ω', '²', '³', &
              '₁', '₂', '⁴', '∈', '∑', '∫', '√', '−', '⟨', '⟩']
    if (.not. init_text_system()) error stop 'no font available'

    call render(achar(238)//achar(128)//achar(128), box)
    failures = 0
    do k = 1, ng
        call render(trim(glyphs(k)), img)
        if (all(img == box)) then
            print *, 'FAIL: raster renders the missing-glyph box for ', trim(glyphs(k))
            failures = failures + 1
        else if (all(img == -1_1)) then
            print *, 'FAIL: raster renders nothing for ', trim(glyphs(k))
            failures = failures + 1
        end if
    end do

    call ensure_test_output_dir('scientific_glyph_coverage', dir)
    title_text = ''
    do k = 1, ng
        title_text = title_text//trim(glyphs(k))//' '
    end do
    call figure()
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
    call title(title_text)
    call savefig(dir//'glyphs.pdf')
    call check_pdf_text(dir//'glyphs.pdf', failures)

    if (failures > 0) then
        print '(a,i0,a)', ' FAIL: ', failures, ' glyph check(s) failed'
        stop 1
    end if
    print *, 'PASS: scientific glyphs render in raster and PDF output'

contains

    subroutine render(text, image)
        character(len=*), intent(in) :: text
        integer(1), allocatable, intent(out) :: image(:)
        allocate (image(3*w*h))
        image = -1_1
        call render_text_to_image(image, w, h, 8, 28, text, 0_1, 0_1, 0_1)
    end subroutine render

    subroutine check_pdf_text(pdf, failures)
        character(len=*), intent(in) :: pdf
        integer, intent(inout) :: failures
        ! pdftotext reports the Symbol angle brackets as U+2329/U+232A.
        character(len=*), parameter :: needles(8) = [character(len=4) :: &
            '→', '⇒', '∈', '∑', '∇', '≤', '〈', '〉']
        character(len=2048) :: line
        character(len=:), allocatable :: txt
        integer :: stat, unit, ios, n

        call execute_command_line('command -v pdftotext >/dev/null 2>&1', &
                                  exitstat=stat)
        if (stat /= 0) then
            print *, 'SKIPPED PDF part: pdftotext not available'
            return
        end if
        call execute_command_line('pdftotext "'//pdf//'" "'//pdf//'.txt"', &
                                  exitstat=stat)
        if (stat /= 0) error stop 'pdftotext failed'
        txt = ''
        open (newunit=unit, file=pdf//'.txt', status='old', action='read')
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            txt = txt//trim(line)//' '
        end do
        close (unit)
        if (index(txt, '?') > 0) then
            print *, 'FAIL: PDF text has ? replacement glyphs: ', txt(1:min(200, len(txt)))
            failures = failures + 1
        end if
        do n = 1, size(needles)
            if (index(txt, trim(needles(n))) == 0) then
                print *, 'FAIL: PDF text lacks ', trim(needles(n))
                failures = failures + 1
            end if
        end do
    end subroutine check_pdf_text

end program test_scientific_glyph_coverage
