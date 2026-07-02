program test_polar_angular_labels
    !! Verify polar angular tick labels match matplotlib defaults and that the
    !! text backend keeps labels, radial-label corridor, and curve glyphs from
    !! overwriting each other (issue #2072).
    use fortplot_polar, only: compute_angular_ticks, format_angle_label, &
                              RAD_TO_DEG
    use fortplot_polar_text_layout, only: polar_frame_t, polar_to_text_cell, &
                                          inside_polar_frame, reserve_label_cells, &
                                          can_place_data
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    real(wp), parameter :: tol = 1.0e-9_wp
    character(len=2), parameter :: degree_sign = achar(194)//achar(176)

    call test_format_uses_degree_sign()
    call test_eight_spokes_at_45_degrees()
    call test_layout_matches_matplotlib_orientation()
    call test_layout_clips_to_frame()
    call test_reservation_blocks_data()
    call test_text_output_readable()

    print *, 'All polar angular label tests passed!'

contains

    subroutine test_format_uses_degree_sign()
        character(len=8) :: label

        label = format_angle_label(45)
        if (trim(label) /= '45'//degree_sign) then
            print *, 'FAIL: expected 45 with degree sign, got [', trim(label), ']'
            stop 1
        end if

        ! The literal " deg" suffix must be gone.
        if (index(label, 'deg') /= 0) then
            print *, 'FAIL: label must not contain literal "deg"'
            stop 1
        end if
    end subroutine test_format_uses_degree_sign

    subroutine test_eight_spokes_at_45_degrees()
        integer, parameter :: n = 8
        real(wp) :: angles(n)
        character(len=8) :: labels(n)
        integer :: i, deg

        call compute_angular_ticks(n, angles, labels)

        do i = 1, n
            deg = nint(angles(i)*RAD_TO_DEG)
            if (deg /= (i - 1)*45) then
                print *, 'FAIL: spoke', i, 'expected', (i-1)*45, 'deg got', deg
                stop 1
            end if
            if (index(labels(i), degree_sign) == 0) then
                print *, 'FAIL: label', i, 'missing degree sign'
                stop 1
            end if
        end do
    end subroutine test_eight_spokes_at_45_degrees

    subroutine test_layout_matches_matplotlib_orientation()
        !! 0 rad lands due east, pi/2 due north (matplotlib geometry oracle).
        type(polar_frame_t) :: frame
        integer :: row, col
        real(wp), parameter :: pi = acos(-1.0_wp)

        frame = polar_frame_t(center_row=12, center_col=40, radius_rows=10, &
                              radius_cols=20)

        call polar_to_text_cell(frame, 0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, .false., &
                                row, col)
        if (row /= 12 .or. col /= 60) then
            print *, 'FAIL: 0 rad expected (12,60) got (', row, ',', col, ')'
            stop 1
        end if

        call polar_to_text_cell(frame, 0.5_wp*pi, 1.0_wp, 1.0_wp, 0.0_wp, .false., &
                                row, col)
        if (row /= 2 .or. col /= 40) then
            print *, 'FAIL: pi/2 expected (2,40) got (', row, ',', col, ')'
            stop 1
        end if
    end subroutine test_layout_matches_matplotlib_orientation

    subroutine test_layout_clips_to_frame()
        !! r == r_max sits on the frame; r beyond r_max is clipped out.
        type(polar_frame_t) :: frame
        integer :: row, col

        frame = polar_frame_t(center_row=12, center_col=40, radius_rows=10, &
                              radius_cols=20)

        call polar_to_text_cell(frame, 0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, .false., &
                                row, col)
        if (.not. inside_polar_frame(frame, row, col)) then
            print *, 'FAIL: boundary sample should be inside the frame'
            stop 1
        end if

        call polar_to_text_cell(frame, 0.0_wp, 1.5_wp, 1.0_wp, 0.0_wp, .false., &
                                row, col)
        if (inside_polar_frame(frame, row, col)) then
            print *, 'FAIL: sample beyond r_max must be clipped'
            stop 1
        end if
    end subroutine test_layout_clips_to_frame

    subroutine test_reservation_blocks_data()
        !! Reserved label cells and their halo reject data; other cells accept.
        logical :: reserved(20, 20)

        reserved = .false.
        call reserve_label_cells(reserved, 5, 8, 3, 1)

        if (can_place_data(reserved, 5, 8)) then
            print *, 'FAIL: label cell must reject data'
            stop 1
        end if
        if (can_place_data(reserved, 5, 10)) then
            print *, 'FAIL: end of label span must reject data'
            stop 1
        end if
        if (can_place_data(reserved, 4, 8)) then
            print *, 'FAIL: halo cell must reject data'
            stop 1
        end if
        if (.not. can_place_data(reserved, 15, 15)) then
            print *, 'FAIL: free cell must accept data'
            stop 1
        end if
        if (can_place_data(reserved, 0, 5)) then
            print *, 'FAIL: out-of-range cell must reject data'
            stop 1
        end if
    end subroutine test_reservation_blocks_data

    subroutine test_text_output_readable()
        !! End-to-end: two polar series render to a text file with all eight
        !! angular labels present and both series glyphs kept inside the frame.
        use fortplot, only: figure_t
        use fortplot_test_helpers, only: test_initialize_figure, test_savefig, &
                                         test_get_temp_path
        type(figure_t) :: fig
        integer, parameter :: n = 180
        real(wp) :: theta(n), r1(n), r2(n)
        real(wp), parameter :: pi = acos(-1.0_wp)
        integer :: i

        do i = 1, n
            theta(i) = 2.0_wp*pi*real(i - 1, wp)/real(n, wp)
            r1(i) = 1.0_wp + 0.3_wp*sin(4.0_wp*theta(i))
            r2(i) = 0.6_wp + 0.2_wp*cos(3.0_wp*theta(i))
        end do

        call test_initialize_figure(fig, 80, 24, 'ascii')
        call fig%add_polar(theta, r1, label='primary rose', marker='o', color='red')
        call fig%add_polar(theta, r2, label='secondary petals', marker='s', &
                           color='blue')
        call test_savefig(fig, 'polar_text_readable.txt')

        call assert_text_output(test_get_temp_path('polar_text_readable.txt'))
    end subroutine test_text_output_readable

    subroutine assert_text_output(path)
        !! All eight angular labels and both series glyphs render, and every
        !! series glyph stays within the plotted frame (columns 2..79).
        character(len=*), intent(in) :: path
        character(len=400) :: line
        character(len=:), allocatable :: content
        character(len=16) :: expected
        character(len=3), parameter :: radial_labels(6) = [character(len=3) :: &
                                                           '0.2', '0.4', '0.6', &
                                                           '0.8', '1.0', '1.2']
        integer :: unit, ios, i, deg, last, row
        logical :: has_o, has_hash

        content = ''
        row = 0
        open (newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open rendered polar text at ', trim(path)
            stop 1
        end if
        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            row = row + 1
            ! Clipping keeps series glyphs inside the frame, so the border
            ! columns of a data row must stay clear of them.
            last = len_trim(line)
            if (last >= 1) then
                if (is_series_glyph(line(1:1))) then
                    print *, 'FAIL: series glyph on left frame border at row', row
                    stop 1
                end if
                if (is_series_glyph(line(last:last))) then
                    print *, 'FAIL: series glyph on right frame border at row', row
                    stop 1
                end if
            end if
            do i = 1, size(radial_labels)
                call assert_label_clear(line, trim(radial_labels(i)), row)
            end do
            content = content//trim(line)//achar(10)
        end do
        close (unit)

        ! Angular degree labels are written with the degree sign transliterated
        ! to the ASCII suffix "deg" by the text backend.
        do i = 1, 8
            deg = (i - 1)*45
            write (expected, '(I0,A)') deg, 'deg'
            if (index(content, trim(expected)) == 0) then
                print *, 'FAIL: rendered polar text missing label ', trim(expected)
                stop 1
            end if
        end do

        has_o = index(content, 'o') /= 0
        has_hash = index(content, '#') /= 0
        if (.not. has_o) then
            print *, 'FAIL: primary series glyph "o" not rendered'
            stop 1
        end if
        if (.not. has_hash) then
            print *, 'FAIL: secondary series glyph "#" not rendered'
            stop 1
        end if

        do i = 1, size(radial_labels)
            if (index(content, trim(radial_labels(i))) == 0) then
                print *, 'FAIL: rendered polar text missing radial label ', &
                         trim(radial_labels(i))
                stop 1
            end if
        end do
    end subroutine assert_text_output

    subroutine assert_label_clear(line, label, row)
        character(len=*), intent(in) :: line, label
        integer, intent(in) :: row

        integer :: pos, start_pos, label_len, left_col, right_col

        start_pos = 1
        label_len = len_trim(label)
        do
            pos = index(line(start_pos:), label(1:label_len))
            if (pos == 0) exit
            pos = start_pos + pos - 1
            left_col = pos - 1
            if (left_col >= 1) then
                if (is_radial_label_char(line(left_col:left_col))) then
                    print *, 'FAIL: radial label touches another label ', &
                             label(1:label_len), ' on row', row
                    stop 1
                end if
                if (is_series_glyph(line(left_col:left_col))) then
                    print *, 'FAIL: series glyph touches radial label ', &
                             label(1:label_len), ' on row', row
                    stop 1
                end if
            end if
            right_col = pos + label_len
            if (right_col <= len(line)) then
                if (is_radial_label_char(line(right_col:right_col))) then
                    print *, 'FAIL: radial label touches another label ', &
                             label(1:label_len), ' on row', row
                    stop 1
                end if
                if (is_series_glyph(line(right_col:right_col))) then
                    print *, 'FAIL: series glyph touches radial label ', &
                             label(1:label_len), ' on row', row
                    stop 1
                end if
            end if
            start_pos = pos + label_len
            if (start_pos > len(line)) exit
        end do
    end subroutine assert_label_clear

    pure logical function is_radial_label_char(ch) result(is_label)
        character(len=1), intent(in) :: ch

        is_label = (ch >= '0' .and. ch <= '9') .or. ch == '.'
    end function is_radial_label_char

    pure logical function is_series_glyph(ch) result(is_glyph)
        character(len=1), intent(in) :: ch

        is_glyph = ch == 'o' .or. ch == '#' .or. ch == '*' .or. ch == '%'
    end function is_series_glyph

end program test_polar_angular_labels
