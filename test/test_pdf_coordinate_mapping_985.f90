program test_pdf_coordinate_mapping_985
    !! Regression test for Issue #985: PDF coordinate mapping must align with axes/ticks
    !! Validates that plotted line X coordinates map within the PDF frame rectangle.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_errors, only: SUCCESS
    use fortplot_test_helpers, only: test_initialize_environment, test_get_temp_path
    implicit none

    character(len=:), allocatable :: pdf_path
    integer :: save_status
    integer :: ios
    logical :: ok

    call test_initialize_environment("issue_985_pdf_coordinate_mapping")

    ! Create a simple line plot with explicit linear limits
    call figure()
    call plot([0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], &
              [0.0_wp, 0.5_wp, 0.0_wp, -0.5_wp, 0.0_wp], 'b-')
    call xlim(0.0_wp, 4.0_wp)
    call ylim(-1.0_wp, 1.0_wp)

    pdf_path = test_get_temp_path('coord_map_985.pdf')
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

    print *, 'PASS: PDF coordinate mapping aligns with frame (fix #985)'
contains

    logical function verify_pdf_coordinates_within_frame(path) result(within)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: content, stream
        real(wp) :: frame_left, frame_bottom, frame_w, frame_h
        real(wp) :: min_x, max_x, val1, val2
        integer :: i, n, pos_s, pos_e
        integer :: unit, sz
        integer :: stat
        character(len=:), allocatable :: line
        integer :: start, nl

        within = .false.
        content = read_file_as_text(trim(path), stat)
        if (stat /= 0 .or. len_trim(content) == 0) return
        
        pos_s = index(content, 'stream')
        pos_e = index(content, 'endstream')
        if (pos_s == 0 .or. pos_e == 0 .or. pos_e <= pos_s) return
        stream = content(pos_s+len('stream'):pos_e-1)

        ! Parse frame line: "<left> <bottom> <width> <height> re S"
        if (.not. parse_frame(stream, frame_left, frame_bottom, frame_w, frame_h)) return

        ! Iterate through stream lines; collect x from 'm' and 'l' commands
        min_x = 1.0e99_wp
        max_x = -1.0e99_wp
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

        if (min_x > 9.9e98_wp .or. max_x < -9.9e98_wp) return

        ! Allow tiny epsilon
        within = (min_x >= frame_left - 1.0e-3_wp) .and. &
                 (max_x <= (frame_left + frame_w) + 1.0e-3_wp)
    end function verify_pdf_coordinates_within_frame

    logical function parse_frame(stream, left, bottom, width, height) result(ok)
        character(len=*), intent(in) :: stream
        real(wp), intent(out) :: left, bottom, width, height
        integer :: p, q, ios
        character(len=:), allocatable :: line
        ok = .false.
        left = 0.0_wp; bottom = 0.0_wp; width = 0.0_wp; height = 0.0_wp
        
        p = index(stream, ' re S')
        if (p == 0) return
        ! Extract the line up to ' re S'
        q = p
        do while (q > 1 .and. stream(q:q) /= new_line('a'))
            q = q - 1
        end do
        if (q < 1) q = 1
        line = adjustl(stream(q:p-1))
        read(line, *, iostat=ios) left, bottom, width, height
        ok = (ios == 0)
    end function parse_frame

    logical function is_path_line(line) result(yes)
        character(len=*), intent(in) :: line
        integer :: L
        character(len=1) :: c
        yes = .false.
        L = len_trim(line)
        if (L < 3) return
        c = line(L:L)
        yes = (c == 'm' .or. c == 'l')
    end function is_path_line

    logical function parse_first_two_reals(line, a, b) result(ok)
        character(len=*), intent(in) :: line
        real(wp), intent(out) :: a, b
        integer :: ios
        ok = .false.
        read(line, *, iostat=ios) a, b
        if (ios == 0) ok = .true.
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
            ! Skip leading spaces
            do while (pos <= L .and. line(pos:pos) == ' ')
                pos = pos + 1
            end do
            if (pos > L) exit
            ! Find token end
            do while (pos <= L .and. line(pos:pos) /= ' ')
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

    function read_file_as_text(path, status) result(text)
        character(len=*), intent(in) :: path
        integer, intent(out) :: status
        character(len=:), allocatable :: text
        integer :: unit, sz, ios
        integer :: i
        character(len=1), allocatable :: ch(:)
        integer(1), allocatable :: bytes(:)
        
        status = 0
        inquire(file=path, size=sz)
        if (sz <= 0) then
            status = 1; text = ""; return
        end if
        allocate(bytes(sz))
        open(newunit=unit, file=path, access='stream', form='unformatted', status='old', action='read', iostat=ios)
        if (ios /= 0) then
            status = 2; text = ""; return
        end if
        read(unit) bytes
        close(unit)
        allocate(character(len=sz) :: text)
        do i = 1, sz
            ! Convert signed byte to unsigned code point [0,255]
            call byte_to_char(bytes(i), text(i:i))
        end do
    end function read_file_as_text

    subroutine byte_to_char(b, ch)
        integer(1), intent(in) :: b
        character(len=1), intent(out) :: ch
        integer :: v
        v = transfer(b, v)
        if (v < 0) v = v + 256
        if (v < 0 .or. v > 255) then
            ch = ' '
        else
            ch = achar(v)
        end if
    end subroutine byte_to_char

end program test_pdf_coordinate_mapping_985
