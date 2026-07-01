program test_text_axis_policy
    !! Regression tests for the polished text-axis rendering policy (issue #2069).
    !!
    !! Positive fixtures: solid bottom/left spines, tick marks at labeled ticks,
    !! and the documented layer collision order.
    !! Negative fixtures: a dashed data-like bottom axis, undocumented bare-gap
    !! rows on the left axis, and plot data overwriting labels must all fail.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure, only: figure_t
    use fortplot_system_runtime, only: create_directory_runtime
    use fortplot_ascii_elements, only: put_cell, glyph_layer, map_ticks_to_cells, &
                                       LAYER_GRID, LAYER_DATA, LAYER_AXIS, &
                                       LAYER_TICK, LAYER_LABEL
    implicit none

    logical :: ok

    ok = .true.
    call test_layer_policy(ok)
    call test_tick_mapping(ok)
    call test_solid_axes_and_ticks(ok)

    if (.not. ok) then
        error stop "test_text_axis_policy: one or more assertions failed"
    end if
    print *, "PASS: text-axis rendering policy"

contains

    subroutine test_layer_policy(ok)
        logical, intent(inout) :: ok
        character(len=1) :: canvas(2, 2)

        if (.not. (LAYER_GRID < LAYER_DATA .and. LAYER_DATA < LAYER_AXIS .and. &
                   LAYER_AXIS < LAYER_TICK .and. LAYER_TICK < LAYER_LABEL)) then
            print *, "FAIL: layer ordering is not grid<data<axis<tick<label"
            ok = .false.
        end if

        if (glyph_layer('|') /= LAYER_AXIS .or. glyph_layer('-') /= LAYER_AXIS) then
            print *, "FAIL: spine glyphs must classify as the axis layer"
            ok = .false.
        end if
        if (glyph_layer('+') /= LAYER_TICK) then
            print *, "FAIL: tick glyph must classify as the tick layer"
            ok = .false.
        end if
        if (glyph_layer('5') /= LAYER_LABEL) then
            print *, "FAIL: digit must classify as the label layer"
            ok = .false.
        end if

        canvas = ' '
        call put_cell(canvas, 1, 1, '|', LAYER_AXIS)
        ! Data must not overwrite the axis spine.
        call put_cell(canvas, 1, 1, 'o', LAYER_DATA)
        if (canvas(1, 1) /= '|') then
            print *, "FAIL: data overwrote axis spine, got ", canvas(1, 1)
            ok = .false.
        end if
        ! A tick overwrites the spine.
        call put_cell(canvas, 1, 1, '+', LAYER_TICK)
        if (canvas(1, 1) /= '+') then
            print *, "FAIL: tick did not overwrite spine, got ", canvas(1, 1)
            ok = .false.
        end if
        ! A label overwrites the tick, and data must not overwrite the label.
        call put_cell(canvas, 1, 1, '5', LAYER_LABEL)
        call put_cell(canvas, 1, 1, 'o', LAYER_DATA)
        if (canvas(1, 1) /= '5') then
            print *, "FAIL: data overwrote a tick label, got ", canvas(1, 1)
            ok = .false.
        end if
    end subroutine test_layer_policy

    subroutine test_tick_mapping(ok)
        logical, intent(inout) :: ok
        real(wp) :: values(3)
        integer :: cells(3)
        logical :: in_range(3)

        values = [0.0_wp, 5.0_wp, 10.0_wp]
        call map_ticks_to_cells(values, 3, 0.0_wp, 10.0_wp, 2, 12, cells, in_range)
        if (cells(1) /= 2 .or. cells(2) /= 7 .or. cells(3) /= 12) then
            print *, "FAIL: tick-to-cell mapping wrong: ", cells
            ok = .false.
        end if
        if (.not. all(in_range)) then
            print *, "FAIL: in-range ticks flagged out of range"
            ok = .false.
        end if
    end subroutine test_tick_mapping

    subroutine test_solid_axes_and_ticks(ok)
        logical, intent(inout) :: ok
        type(figure_t) :: fig
        real(wp) :: x(11), y(11)
        integer :: i, unit, ios
        logical :: dir_ok
        character(len=*), parameter :: outfile = &
            'build/test/output/text_axis_policy.txt'
        character(len=256) :: line, lines(64)
        integer :: nlines, top_frame, bottom_frame
        integer :: spine_rows, blank_rows, plus_bottom, run, best_run
        integer :: c
        character(len=1) :: col2

        call create_directory_runtime('build/test/output', dir_ok)
        x = [(real(i, wp), i=0, 10)]
        y = x
        call fig%initialize(80, 24)
        call fig%add_plot(x, y, label="line")
        call fig%savefig(outfile)

        nlines = 0
        open (newunit=unit, file=outfile, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "FAIL: cannot read ", outfile
            ok = .false.
            return
        end if
        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            if (nlines <= size(lines)) lines(nlines) = line
        end do
        close (unit)

        top_frame = 0
        bottom_frame = 0
        do i = 1, min(nlines, size(lines))
            if (is_frame_line(lines(i))) then
                if (top_frame == 0) then
                    top_frame = i
                else
                    bottom_frame = i
                end if
            end if
        end do
        if (top_frame == 0 .or. bottom_frame == 0) then
            print *, "FAIL: could not locate ASCII frame"
            ok = .false.
            return
        end if

        ! Left spine: canvas column 2 lands at string index 3 (border + col1).
        ! Count consistent spine rows versus undocumented blank-gap rows.
        spine_rows = 0
        blank_rows = 0
        do i = top_frame + 1, bottom_frame - 1
            col2 = ' '
            if (len_trim(lines(i)) >= 3) col2 = lines(i)(3:3)
            if (col2 == '|') spine_rows = spine_rows + 1
            if (col2 == ' ') blank_rows = blank_rows + 1
        end do
        if (spine_rows < 10) then
            print *, "FAIL: left spine not continuous, spine rows=", spine_rows
            ok = .false.
        end if
        if (blank_rows > 3) then
            print *, "FAIL: left axis has undocumented bare-gap rows=", blank_rows
            ok = .false.
        end if

        ! Bottom spine is the last framed row that is a long solid run of '-'.
        best_run = 0
        plus_bottom = 0
        do i = bottom_frame - 1, top_frame + 1, -1
            run = max_dash_run(lines(i))
            if (run >= 8) then
                best_run = run
                plus_bottom = count_char(lines(i), '+')
                if (index(trim(lines(i)), '- - - -') > 0) then
                    print *, "FAIL: bottom axis is a dashed data-like line"
                    ok = .false.
                end if
                exit
            end if
        end do
        if (best_run < 8) then
            print *, "FAIL: bottom spine is not a solid line, best run=", best_run
            ok = .false.
        end if
        if (plus_bottom < 2) then
            print *, "FAIL: bottom spine lacks tick marks, plus count=", plus_bottom
            ok = .false.
        end if
    end subroutine test_solid_axes_and_ticks

    pure logical function is_frame_line(line) result(found)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: t

        t = trim(adjustl(line))
        found = .false.
        if (len(t) < 3) return
        found = t(1:1) == '+' .and. index(t, '---') > 0
    end function is_frame_line

    pure integer function max_dash_run(line) result(best)
        character(len=*), intent(in) :: line
        integer :: i, run

        best = 0
        run = 0
        do i = 1, len(line)
            if (line(i:i) == '-') then
                run = run + 1
                if (run > best) best = run
            else
                run = 0
            end if
        end do
    end function max_dash_run

    pure integer function count_char(line, ch) result(n)
        character(len=*), intent(in) :: line
        character(len=1), intent(in) :: ch
        integer :: i

        n = 0
        do i = 1, len(line)
            if (line(i:i) == ch) n = n + 1
        end do
    end function count_char

end program test_text_axis_policy
