program test_pdf_coordinate_mapping_985
    !! Regression test for Issue #985: PDF coordinate mapping must align with axes/ticks
    !! Validates that plotted line X coordinates map within the PDF frame rectangle.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_errors, only: SUCCESS
    use test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=:), allocatable :: pdf_path
    integer :: save_status
    logical :: ok

    ! Create a simple line plot with explicit linear limits
    call figure()
    call plot([0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], &
              [0.0_wp, 0.5_wp, 0.0_wp, -0.5_wp, 0.0_wp], 'b-')
    call xlim(0.0_wp, 4.0_wp)
    call ylim(-1.0_wp, 1.0_wp)

    pdf_path = 'build/test/output/coord_map_985_manual.pdf'
    call savefig_with_status(pdf_path, save_status)
    if (save_status /= SUCCESS) then
        print *, 'FAIL: could not save PDF file: ', trim(pdf_path)
        stop 1
    end if

    ok = verify_pdf_coordinates_within_frame(trim(pdf_path))
    if (.not. ok) then
        print *, 'FAIL: PDF line path exceeds frame bounds (Issue #985 regression)'
        stop 1
    end if

    print *, 'PASS: PDF coordinate mapping aligns with frame (Issue #985)'
contains

    logical function verify_pdf_coordinates_within_frame(path) result(within)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: stream
        real(wp) :: frame_left, frame_bottom, frame_w, frame_h
        real(wp) :: min_x, max_x, val1, val2
        integer :: start, nl, status
        character(len=:), allocatable :: line

        within = .false.

        call extract_pdf_stream_text(trim(path), stream, status)
        if (status /= 0) return
        if (len(stream) == 0) return

        if (.not. parse_frame(stream, frame_left, frame_bottom, frame_w, frame_h)) return

        min_x = huge(1.0_wp)
        max_x = -huge(1.0_wp)
        start = 1
        do
            nl = index(stream(start:), new_line('a'))
            if (nl == 0) then
                line = adjustl(stream(start:))
            else
                line = adjustl(stream(start:start+nl-2))
            end if
            if (len_trim(line) > 0) then
                if (is_path_line(line)) then
                    if (parse_first_two_reals(line, val1, val2)) then
                        min_x = min(min_x, val1)
                        max_x = max(max_x, val1)
                    end if
                end if
            end if
            if (nl == 0) exit
            start = start + nl
            if (start > len(stream)) exit
        end do

        if (min_x >= huge(1.0_wp) .or. max_x <= -huge(1.0_wp)) return

        within = (min_x >= frame_left - 1.0e-3_wp) .and. &
                 (max_x <= (frame_left + frame_w) + 1.0e-3_wp)
    end function verify_pdf_coordinates_within_frame

    logical function parse_frame(stream, left, bottom, width, height) result(ok)
        character(len=*), intent(in) :: stream
        real(wp), intent(out) :: left, bottom, width, height
        integer :: p, q, start
        character(len=:), allocatable :: line

        ok = .false.
        left = 0.0_wp
        bottom = 0.0_wp
        width = 0.0_wp
        height = 0.0_wp

        p = index(stream, ' re S')
        if (p == 0) return

        q = p - 1
        do while (q >= 1 .and. stream(q:q) /= new_line('a'))
            q = q - 1
        end do
        start = max(1, q + 1)
        line = adjustl(stream(start:p-1))
        line = line(1:len_trim(line))
        if (len_trim(line) == 0) return

        ok = parse_four_reals(line, left, bottom, width, height)
    end function parse_frame

    logical function is_path_line(line) result(yes)
        character(len=*), intent(in) :: line
        integer :: L
        character(len=1) :: c

        yes = .false.
        L = len_trim(line)
        if (L < 2) return
        c = line(L:L)

        select case (c)
        case ('m')
            yes = (line(L-1:L-1) == ' ')
        case ('l')
            yes = (line(L-1:L-1) == ' ')
        end select
    end function is_path_line

    logical function parse_first_two_reals(line, a, b) result(ok)
        character(len=*), intent(in) :: line
        real(wp), intent(out) :: a, b
        integer :: i1, i2, i3, i4
        character(len=:), allocatable :: s1, s2
        ok = .false.
        call split_tokens(line, i1, i2, i3, i4)
        if (i1 == 0 .or. i2 == 0) return
        s1 = line(1:i1-1)
        s2 = line(i1+1:i2-1)
        read(s1, *, iostat=i3) a
        if (i3 /= 0) return
        read(s2, *, iostat=i3) b
        if (i3 /= 0) return
        ok = .true.
    end function parse_first_two_reals

    logical function parse_four_reals(line, a, b, c, d) result(ok)
        character(len=*), intent(in) :: line
        real(wp), intent(out) :: a, b, c, d
        integer :: i1, i2, i3, i4, ios
        character(len=:), allocatable :: s1, s2, s3, s4
        ok = .false.
        call split_tokens(line, i1, i2, i3, i4)
        if (min(i1, i2, i3, i4) <= 1) return
        s1 = line(1:i1-1)
        s2 = line(i1+1:i2-1)
        s3 = line(i2+1:i3-1)
        s4 = line(i3+1:i4-1)
        read(s1, *, iostat=ios) a; if (ios /= 0) return
        read(s2, *, iostat=ios) b; if (ios /= 0) return
        read(s3, *, iostat=ios) c; if (ios /= 0) return
        read(s4, *, iostat=ios) d; if (ios /= 0) return
        ok = .true.
    end function parse_four_reals

    subroutine split_tokens(line, i1, i2, i3, i4)
        !! Find positions of first 4 space-separated tokens' ends
        character(len=*), intent(in) :: line
        integer, intent(out) :: i1, i2, i3, i4
        integer :: pos, L, count
        L = len_trim(line)
        i1 = 0; i2 = 0; i3 = 0; i4 = 0
        pos = 1; count = 0
        do while (pos <= L)
            do while (pos <= L .and. line(pos:pos) == ' ')
                pos = pos + 1
            end do
            if (pos > L) exit
            do while (pos <= L)
                if (line(pos:pos) == ' ') exit
                pos = pos + 1
            end do
            count = count + 1
            select case (count)
            case (1); i1 = pos
            case (2); i2 = pos
            case (3); i3 = pos
            case (4); i4 = pos; return
            end select
        end do
    end subroutine split_tokens

end program test_pdf_coordinate_mapping_985
