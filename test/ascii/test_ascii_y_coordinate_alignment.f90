program test_ascii_y_coordinate_alignment
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_ascii, only: ascii_context, create_ascii_canvas
    implicit none

    type(ascii_context) :: ctx
    integer :: row, col, first_row

    ctx = create_ascii_canvas(80, 24)
    call ctx%set_coordinates(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp)
    call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)
    call ctx%line(0.0_wp, 0.8_wp, 1.0_wp, 0.8_wp)

    first_row = 0
    do row = 1, size(ctx%canvas, 1)
        do col = 1, size(ctx%canvas, 2)
            if (ctx%canvas(row, col) == '*') then
                first_row = row
                exit
            end if
        end do
        if (first_row > 0) exit
    end do

    if (first_row == 0) then
        print *, 'FAIL: no ASCII line glyphs rendered'
        stop 1
    end if

    if (first_row > 9) then
        print *, 'FAIL: y=0.8 rendered too low, row=', first_row
        stop 1
    end if

    print *, 'PASS: ASCII y coordinates align with axis range'
end program test_ascii_y_coordinate_alignment
