program test_pdf_axes_matplotlib_defaults
    !! Behavioral oracle: Matplotlib 3.11 rcdefaults() uses outward major ticks
    !! of 3.5pt, minor ticks of 2pt, 10pt tick/axis labels, and 12pt titles.
    !! Inspect emitted drawing operations, rather than importing size constants.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core, create_pdf_canvas_core
    use fortplot_pdf_axes_drawing, only: draw_pdf_tick_marks_with_area, &
        draw_pdf_minor_tick_marks, &
        draw_pdf_tick_labels_with_area
    use fortplot_pdf_axes_text, only: draw_pdf_title_and_labels
    use fortplot_pdf_secondary_axes, only: draw_pdf_secondary_y_axis, &
        draw_pdf_secondary_x_axis_top
    implicit none

    type(pdf_context_core) :: ctx
    integer :: failures

    failures = 0
    ctx = create_pdf_canvas_core(460.8_wp, 345.6_wp)
    ctx%stream_data = ''
    call draw_pdf_tick_marks_with_area(ctx, [100.0_wp], [120.0_wp], &
        1, 1, 60.0_wp, 40.0_wp)
    call require_segment(100.0_wp, 40.0_wp, 100.0_wp, 36.5_wp)
    call require_segment(60.0_wp, 120.0_wp, 56.5_wp, 120.0_wp)
    call require_text('.800 w', 'major tick width')

    ctx%stream_data = ''
    call draw_pdf_minor_tick_marks(ctx, [100.0_wp], [120.0_wp], &
        1, 1, 60.0_wp, 40.0_wp)
    call require_segment(100.0_wp, 40.0_wp, 100.0_wp, 38.0_wp)
    call require_segment(60.0_wp, 120.0_wp, 58.0_wp, 120.0_wp)
    call require_text('.600 w', 'minor tick width')

    ctx%stream_data = ''
    call draw_pdf_tick_labels_with_area(ctx, [100.0_wp], [120.0_wp], &
        ['1'], ['2'], 1, 1, 60.0_wp, &
        40.0_wp, 260.0_wp)
    call require_text('10.0 Tf', 'tick label font size')
    if (index(ctx%stream_data, '12.0 Tf') /= 0) failures = failures + 1

    ctx%stream_data = ''
    call draw_pdf_title_and_labels(ctx, title='Title', &
        plot_area_left=60.0_wp, &
        plot_area_bottom=40.0_wp, &
        plot_area_width=350.0_wp, &
        plot_area_height=260.0_wp)
    call require_text('12.0 Tf', 'title font size')

    ctx%stream_data = ''
    call draw_pdf_title_and_labels(ctx, xlabel='x', ylabel='y', &
        plot_area_left=60.0_wp, &
        plot_area_bottom=40.0_wp, &
        plot_area_width=350.0_wp, &
        plot_area_height=260.0_wp)
    call require_text('10.0 Tf', 'axis label font size')
    if (index(ctx%stream_data, '12.0 Tf') /= 0) failures = failures + 1

    ctx%stream_data = ''
    call draw_pdf_secondary_y_axis(ctx, 'linear', 1.0_wp, 0.0_wp, 1.0_wp, &
        60.0_wp, 40.0_wp, 350.0_wp, 260.0_wp)
    call require_segment(410.0_wp, 40.0_wp, 413.5_wp, 40.0_wp)
    ctx%stream_data = ''
    call draw_pdf_secondary_x_axis_top(ctx, 'linear', 1.0_wp, 0.0_wp, 1.0_wp, &
        60.0_wp, 40.0_wp, 350.0_wp, 260.0_wp)
    call require_segment(60.0_wp, 300.0_wp, 60.0_wp, 303.5_wp)

    if (failures /= 0) then
        print *, 'FAIL: PDF axes differ from matplotlib defaults:', failures
        stop 1
    end if
    print *, 'PASS: PDF tick geometry and font sizes match matplotlib defaults'

contains

    subroutine require_segment(x1, y1, x2, y2)
        real(wp), intent(in) :: x1, y1, x2, y2
        character(len=128) :: expected

        write (expected, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
            x1, y1, x2, y2
        call require_text(trim(expected), 'outward tick segment')
    end subroutine require_segment

    subroutine require_text(expected, description)
        character(len=*), intent(in) :: expected, description

        if (index(ctx%stream_data, expected) /= 0) return
        print *, 'FAIL: missing ', description, ': ', expected
        failures = failures + 1
    end subroutine require_text

end program test_pdf_axes_matplotlib_defaults
