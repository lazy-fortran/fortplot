program test_mathtext_layout_rendering
    use, intrinsic :: iso_fortran_env, only: wp => real64, int8
    use fortplot_raster_text_rendering, only: render_text_with_size
    use fortplot_text_layout, only: calculate_text_width_with_size
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_pdf_text_metrics, only: estimate_pdf_text_width, helv_width_units
    use fortplot_pdf_text_escape, only: unicode_to_symbol_char
    use fortplot_pdf_mathtext_render, only: draw_pdf_mathtext
    use fortplot_pdf_core, only: pdf_context_core, create_pdf_canvas_core
    use fortplot_pdf_io, only: write_pdf_file
    use fortplot_png, only: write_png_file
    use fortplot_test_helpers, only: test_initialize_environment, test_get_temp_path
    implicit none

    integer, parameter :: width = 900, height = 560
    real(wp), parameter :: fs = 36.0_wp
    character(len=64), parameter :: expressions(*) = [character(len=64) :: &
        '$x_i^2 + y_{i+1}^{n-1} = e^{-x/3}$', &
        '$x^{y^2} + x_{i_j}$', '$x_α + x_{α}$', &
        '$\sqrt{x^2+y^2} = \alpha + \beta$', &
        '$\frac{1}{2} + \frac{x^2}{1+x}$', &
        '$\frac{\sqrt{x_i^2}}{\frac{1}{2}+y}$', &
        'Unicode radical: √x']
    integer(int8), allocatable :: pixels(:), first(:), second(:)
    type(pdf_context_core) :: pdf
    character(len=:), allocatable :: path
    character(len=512) :: processed
    character(len=8) :: symbol
    integer :: i, y, unit, codepoint, symbol_code, status, plen
    logical :: success

    call test_initialize_environment('mathtext_layout')
    allocate (pixels(width*height*3), first(width*height*3), &
        second(width*height*3))
    call assert_same_pixels('$x_i^2$', '$x^2_i$', 'script ordering')
    call assert_same_pixels('$x_α$', '$x_{α}$', 'UTF-8 single-character scripts')
    call assert_same_pixels('$\frac{1}{2}$', '$\frac {1}{2}$', 'fraction space')
    call assert_same_pixels('$\frac{1}{2}$', '$\frac{1} {2}$', 'denominator space')
    call assert_same_pixels('$\sqrt{x}$', '$\sqrt {x}$', 'radical space')
    call assert_same_pixels('$\sqrt{α}$', '$\sqrt α$', 'UTF-8 radical argument')
    call assert_same_pdf('$\frac{1}{2}$', '$\frac {1} {2}$')
    call assert_same_pdf('$\sqrt{x}$', '$\sqrt {x}$')
    call assert_shared_width()
    pixels = -1_int8
    pdf = create_pdf_canvas_core(real(width, wp), real(height, wp))
    path = test_get_temp_path('math-widths.tsv')
    open (newunit=unit, file=path, status='replace', action='write')
    do i = 1, size(expressions)
        y = 65 + (i - 1)*72
        call process_latex_in_text(trim(expressions(i)), processed, plen)
        call render_text_with_size(pixels, width, height, 30, y, processed(1:plen), &
            0_int8, 0_int8, 0_int8, fs)
        ! Formula rules must remain solid even after a dashed plot line.
        pdf%stream_data = pdf%stream_data//'[4 2] 0 d'//new_line('a')
        call draw_pdf_mathtext(pdf, 30.0_wp, real(height - y, wp), &
            trim(expressions(i)), fs)
        write (unit, '(A,A,I0,A,F0.6)') trim(expressions(i)), achar(9), &
            calculate_text_width_with_size(processed(1:plen), fs), achar(9), &
            estimate_pdf_text_width(processed(1:plen), fs)
    end do
    close (unit)
    call write_png_file(test_get_temp_path('math-layout.png'), width, height, pixels)
    call write_pdf_file(pdf, test_get_temp_path('math-layout.pdf'), success)
    if (.not. success) error stop 'Unable to write math PDF'
    open (newunit=unit, file=test_get_temp_path('symbol-metrics.tsv'), &
        status='replace', action='write')
    do codepoint = 1, 10000
        call unicode_to_symbol_char(codepoint, symbol)
        if (len_trim(symbol) == 0) cycle
        read (symbol(2:), '(O3)', iostat=status) symbol_code
        if (status /= 0) error stop 'Invalid Symbol encoding'
        write (unit, '(I0,A,I0,A,I0)') codepoint, achar(9), symbol_code, &
            achar(9), helv_width_units(codepoint)
    end do
    close (unit)
    print *, 'PASS: math rendering invariants; PNG/PDF oracle artifacts generated'

contains

    subroutine assert_same_pixels(a, b, description)
        character(len=*), intent(in) :: a, b, description

        first = -1_int8
        second = -1_int8
        call render_text_with_size(first, width, height, 30, 80, a, &
            0_int8, 0_int8, 0_int8, fs)
        call render_text_with_size(second, width, height, 30, 80, b, &
            0_int8, 0_int8, 0_int8, fs)
        if (all(first == -1_int8)) error stop 'Empty math rendering'
        if (any(first /= second)) then
            print *, 'FAIL: equivalent notation rendered differently: ', description
            error stop 1
        end if
    end subroutine assert_same_pixels

    subroutine assert_same_pdf(a, b)
        character(len=*), intent(in) :: a, b
        type(pdf_context_core) :: first_pdf, second_pdf

        first_pdf = create_pdf_canvas_core(900.0_wp, 560.0_wp)
        second_pdf = create_pdf_canvas_core(900.0_wp, 560.0_wp)
        call draw_pdf_mathtext(first_pdf, 30.0_wp, 100.0_wp, a, fs)
        call draw_pdf_mathtext(second_pdf, 30.0_wp, 100.0_wp, b, fs)
        if (first_pdf%stream_data /= second_pdf%stream_data) then
            error stop 'Equivalent math spacing changed PDF rendering'
        end if
    end subroutine assert_same_pdf

    subroutine assert_shared_width()
        real(wp) :: pair, single, bare
        integer :: raster_pair, raster_single, raster_bare

        pair = estimate_pdf_text_width('$x_i^2$', fs)
        single = estimate_pdf_text_width('$x^2$', fs)
        bare = estimate_pdf_text_width('$x_i$', fs)
        if (pair > max(single, bare) + 0.001_wp) then
            error stop 'PDF scripts must share the base anchor'
        end if
        raster_pair = calculate_text_width_with_size('$x_i^2$', fs)
        raster_single = calculate_text_width_with_size('$x^2$', fs)
        raster_bare = calculate_text_width_with_size('$x_i$', fs)
        if (raster_pair > max(raster_single, raster_bare)) then
            error stop 'Raster scripts must share the base anchor'
        end if
    end subroutine assert_shared_width

end program test_mathtext_layout_rendering
