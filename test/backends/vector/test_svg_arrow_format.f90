program test_svg_arrow_format
    !! Regression test for the SVG arrow format-string bug introduced when
    !! fortplot_svg_draw was split off from fortplot_svg (refs #1694).
    !! The arrow write used a 16-item format with 15 arguments; gfortran
    !! aborts on the mismatch ("End of formatted record"). This test calls
    !! draw_arrow for both a filled arrow (line + polygon) and a plain ->
    !! style, and checks the resulting SVG fragment is well-formed.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_svg, only: svg_context, create_svg_canvas
    implicit none

    type(svg_context) :: ctx
    integer :: line_start, line_close, poly_start, poly_close
    integer :: scan, balance

    ctx = create_svg_canvas(800, 600)
    call ctx%set_coordinates(0.0_wp, 10.0_wp, 0.0_wp, 5.0_wp)
    call ctx%color(0.2_wp, 0.4_wp, 0.6_wp)

    ! 'filled' emits both <line ...stroke=.../> and <polygon ...fill=.../>.
    call ctx%draw_arrow(5.0_wp, 2.5_wp, 1.0_wp, 0.5_wp, 6.0_wp, 'filled')

    if (.not. allocated(ctx%content_stream)) then
        print *, 'FAIL: draw_arrow did not allocate content_stream'
        error stop 1
    end if

    line_start = index(ctx%content_stream, '<line ')
    if (line_start == 0) then
        print *, 'FAIL: <line element missing'
        error stop 1
    end if
    line_close = index(ctx%content_stream(line_start:), '/>')
    if (line_close == 0) then
        print *, 'FAIL: <line element has no /> close'
        error stop 1
    end if
    if (index(ctx%content_stream(line_start:line_start + line_close), &
              'stroke="rgb(') == 0) then
        print *, 'FAIL: <line missing stroke="rgb(...)"'
        error stop 1
    end if

    poly_start = index(ctx%content_stream, '<polygon ')
    if (poly_start == 0) then
        print *, 'FAIL: <polygon element missing'
        error stop 1
    end if
    poly_close = index(ctx%content_stream(poly_start:), '/>')
    if (poly_close == 0) then
        print *, 'FAIL: <polygon element has no /> close'
        error stop 1
    end if
    if (index(ctx%content_stream(poly_start:poly_start + poly_close), &
              'fill="rgb(') == 0) then
        print *, 'FAIL: <polygon missing fill="rgb(...)"'
        error stop 1
    end if

    ! Every <line and <polygon must be self-closed; no stray opening tag.
    balance = count_substr(ctx%content_stream, '<line ') &
            + count_substr(ctx%content_stream, '<polygon ')
    scan = count_substr(ctx%content_stream, '/>')
    if (balance > scan) then
        print *, 'FAIL: unbalanced open vs self-close tags', balance, scan
        error stop 1
    end if

    print *, 'PASS: SVG arrow format-string is well-formed'

contains

    integer function count_substr(s, needle) result(n)
        character(len=*), intent(in) :: s, needle
        integer :: i, step
        n = 0
        step = len(needle)
        if (step <= 0) return
        i = 1
        do
            if (i > len(s) - step + 1) exit
            if (s(i:i + step - 1) == needle) then
                n = n + 1
                i = i + step
            else
                i = i + 1
            end if
        end do
    end function count_substr

end program test_svg_arrow_format
