program test_braille
    !! Braille text-mode regression tests (issue #2061).
    !!
    !! Covers the encoder (dot masks -> U+2800 codepoints), subpixel
    !! resolution versus ASCII cells, invalid-mask handling, and the
    !! figure-level set_text_charset('braille') plot path.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_braille, only: braille_canvas_t, create_braille_canvas, &
        braille_dot_bit, set_braille_pixel, braille_draw_line, &
        braille_char, braille_codepoint, braille_mask_valid
    use fortplot_figure, only: figure_t
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer :: failures
    logical :: dir_ok

    failures = 0
    call create_directory_runtime('build/test/output', dir_ok)

    call test_dot_codepoints(failures)
    call test_utf8_bytes(failures)
    call test_invalid_mask(failures)
    call test_out_of_range_pixel(failures)
    call test_subpixel_resolution(failures)
    call test_scatter_distinguishable(failures)
    call test_figure_braille_vs_ascii(failures)

    if (failures /= 0) then
        print *, "FAIL: braille tests failed:", failures
        stop 1
    end if
    print *, "PASS: all braille tests passed"

contains

    subroutine test_dot_codepoints(nfail)
        integer, intent(inout) :: nfail
        integer :: dot_mask(8), expected_cp(8), i

        ! dot1..dot8 bitmasks and their U+2800 + mask codepoints.
        dot_mask = [int(z'01'), int(z'02'), int(z'04'), int(z'08'), &
            int(z'10'), int(z'20'), int(z'40'), int(z'80')]
        expected_cp = [int(z'2801'), int(z'2802'), int(z'2804'), int(z'2808'), &
            int(z'2810'), int(z'2820'), int(z'2840'), int(z'2880')]
        do i = 1, 8
            if (braille_codepoint(dot_mask(i)) /= expected_cp(i)) then
                print *, "FAIL: dot", i, "codepoint", braille_codepoint(dot_mask(i)), &
                    "expected", expected_cp(i)
                nfail = nfail + 1
            end if
        end do

        ! Dot-bit layout: left column dots 1,2,3,7; right column dots 4,5,6,8.
        if (braille_dot_bit(0, 0) /= int(z'01')) nfail = nfail + 1
        if (braille_dot_bit(0, 1) /= int(z'02')) nfail = nfail + 1
        if (braille_dot_bit(0, 2) /= int(z'04')) nfail = nfail + 1
        if (braille_dot_bit(0, 3) /= int(z'40')) nfail = nfail + 1
        if (braille_dot_bit(1, 0) /= int(z'08')) nfail = nfail + 1
        if (braille_dot_bit(1, 1) /= int(z'10')) nfail = nfail + 1
        if (braille_dot_bit(1, 2) /= int(z'20')) nfail = nfail + 1
        if (braille_dot_bit(1, 3) /= int(z'80')) nfail = nfail + 1
    end subroutine test_dot_codepoints

    subroutine test_utf8_bytes(nfail)
        integer, intent(inout) :: nfail
        character(len=3) :: c

        ! mask=1 -> U+2801 -> E2 A0 81
        c = braille_char(1)
        if (iachar(c(1:1)) /= int(z'E2')) nfail = nfail + 1
        if (iachar(c(2:2)) /= int(z'A0')) nfail = nfail + 1
        if (iachar(c(3:3)) /= int(z'81')) nfail = nfail + 1

        ! mask=255 -> U+28FF -> E2 A3 BF
        c = braille_char(255)
        if (iachar(c(1:1)) /= int(z'E2')) nfail = nfail + 1
        if (iachar(c(2:2)) /= int(z'A3')) nfail = nfail + 1
        if (iachar(c(3:3)) /= int(z'BF')) nfail = nfail + 1

        if (nfail /= 0) print *, "NOTE: utf8 byte mismatch detected"
    end subroutine test_utf8_bytes

    subroutine test_invalid_mask(nfail)
        integer, intent(inout) :: nfail
        character(len=3) :: c
        integer :: cp

        ! Invalid masks are rejected by validity check ...
        if (braille_mask_valid(256)) then
            print *, "FAIL: mask 256 reported valid"
            nfail = nfail + 1
        end if
        if (braille_mask_valid(-1)) then
            print *, "FAIL: mask -1 reported valid"
            nfail = nfail + 1
        end if
        if (.not. braille_mask_valid(0)) nfail = nfail + 1
        if (.not. braille_mask_valid(255)) nfail = nfail + 1

        ! ... and clamped before encoding so the codepoint never leaves the
        ! U+2800..U+28FF braille block.
        cp = braille_codepoint(300)
        if (cp < int(z'2800') .or. cp > int(z'28FF')) then
            print *, "FAIL: invalid mask encodes outside braille block:", cp
            nfail = nfail + 1
        end if
        c = braille_char(300)
        if (iachar(c(1:1)) /= int(z'E2')) then
            print *, "FAIL: clamped braille char not in braille block"
            nfail = nfail + 1
        end if
    end subroutine test_invalid_mask

    subroutine test_out_of_range_pixel(nfail)
        integer, intent(inout) :: nfail
        type(braille_canvas_t) :: canvas

        ! Out-of-range subpixels must not corrupt the canvas or set a bit.
        canvas = create_braille_canvas(4, 4)
        call set_braille_pixel(canvas, -1, 0)
        call set_braille_pixel(canvas, 0, -1)
        call set_braille_pixel(canvas, canvas%sub_w, 0)
        call set_braille_pixel(canvas, 0, canvas%sub_h)
        if (any(canvas%mask /= 0)) then
            print *, "FAIL: out-of-range pixel set a mask bit"
            nfail = nfail + 1
        end if

        ! A valid subpixel sets exactly one dot.
        call set_braille_pixel(canvas, 0, 0)
        if (canvas%mask(1, 1) /= int(z'01')) then
            print *, "FAIL: valid pixel did not set dot1"
            nfail = nfail + 1
        end if
    end subroutine test_out_of_range_pixel

    subroutine test_subpixel_resolution(nfail)
        integer, intent(inout) :: nfail
        type(braille_canvas_t) :: canvas
        integer :: y, distinct_cell_rows, distinct_sub_rows
        logical :: cell_seen(0:23), sub_seen(0:95)

        ! A diagonal across the whole canvas touches many more distinct
        ! subpixel rows than character-cell rows: braille resolves finer.
        canvas = create_braille_canvas(80, 24)
        call braille_draw_line(canvas, 0, 0, canvas%sub_w - 1, canvas%sub_h - 1)

        cell_seen = .false.
        sub_seen = .false.
        do y = 0, canvas%sub_h - 1
            if (row_has_dot(canvas, y)) then
                sub_seen(y) = .true.
                cell_seen(y / 4) = .true.
            end if
        end do
        distinct_sub_rows = count(sub_seen)
        distinct_cell_rows = count(cell_seen)

        if (distinct_sub_rows <= distinct_cell_rows) then
            print *, "FAIL: braille not higher resolution:", &
                distinct_sub_rows, "sub rows vs", distinct_cell_rows, "cell rows"
            nfail = nfail + 1
        end if
    end subroutine test_subpixel_resolution

    logical function row_has_dot(canvas, y_sub) result(has)
        !! True if any subpixel dot is set on absolute subpixel row y_sub.
        type(braille_canvas_t), intent(in) :: canvas
        integer, intent(in) :: y_sub
        integer :: cell_y, local_y, x_sub, cell_x, local_x, bit

        has = .false.
        cell_y = y_sub / 4 + 1
        local_y = mod(y_sub, 4)
        do x_sub = 0, canvas%sub_w - 1
            cell_x = x_sub / 2 + 1
            local_x = mod(x_sub, 2)
            bit = braille_dot_bit(local_x, local_y)
            if (iand(canvas%mask(cell_y, cell_x), bit) /= 0) then
                has = .true.
                return
            end if
        end do
    end function row_has_dot

    subroutine test_scatter_distinguishable(nfail)
        integer, intent(inout) :: nfail
        type(braille_canvas_t) :: canvas
        integer :: bits

        ! Two points in the same character cell but different dot rows stay
        ! distinguishable in braille (two bits set), while a single ASCII cell
        ! would collapse them into one indistinct glyph.
        canvas = create_braille_canvas(8, 8)
        call set_braille_pixel(canvas, 2, 0) ! cell (1,2) top dot
        call set_braille_pixel(canvas, 2, 1) ! same cell, second dot row
        bits = popcount(canvas%mask(1, 2))
        if (bits /= 2) then
            print *, "FAIL: nearby points not distinguishable, bits=", bits
            nfail = nfail + 1
        end if
    end subroutine test_scatter_distinguishable

    integer function popcount(v) result(n)
        integer, intent(in) :: v
        integer :: b
        n = 0
        do b = 0, 7
            if (iand(v, ishft(1, b)) /= 0) n = n + 1
        end do
    end function popcount

    subroutine test_figure_braille_vs_ascii(nfail)
        integer, intent(inout) :: nfail
        type(figure_t) :: fig_ascii, fig_braille
        real(wp) :: x(50), yv(50)
        integer :: i
        character(len=*), parameter :: ascii_file = 'build/test/output/braille_ascii.txt'
        character(len=*), parameter :: braille_file = 'build/test/output/braille_mode.txt'

        x = [(real(i - 1, wp) / 49.0_wp, i = 1, 50)]
        yv = x

        call fig_ascii%initialize(80, 24)
        call fig_ascii%add_plot(x, yv)
        call fig_ascii%savefig(ascii_file)

        call fig_braille%initialize(80, 24)
        call fig_braille%set_text_charset('braille')
        call fig_braille%add_plot(x, yv)
        call fig_braille%savefig(braille_file)

        if (file_has_braille(ascii_file)) then
            print *, "FAIL: ascii mode emitted braille codepoints"
            nfail = nfail + 1
        end if
        if (.not. file_has_braille(braille_file)) then
            print *, "FAIL: braille mode emitted no braille codepoints"
            nfail = nfail + 1
        end if

        ! Re-saving the already-rendered figure must keep the braille dots.
        call fig_braille%savefig(braille_file)
        if (.not. file_has_braille(braille_file)) then
            print *, "FAIL: braille dots lost on re-save"
            nfail = nfail + 1
        end if
    end subroutine test_figure_braille_vs_ascii

    logical function file_has_braille(filename) result(found)
        !! Scan raw bytes for a U+2800-block lead sequence: 0xE2 followed by
        !! a continuation byte in 0xA0..0xA3 (covers U+2800..U+28FF).
        character(len=*), intent(in) :: filename
        integer :: unit, ios, fsize, i, b1, b2
        integer(1), allocatable :: bytes(:)

        found = .false.
        inquire(file=filename, size=fsize)
        if (fsize <= 0) return
        allocate(bytes(fsize))
        open(newunit=unit, file=filename, access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read(unit, iostat=ios) bytes
        close(unit)
        if (ios /= 0) return
        do i = 1, fsize - 1
            b1 = iand(int(bytes(i)), 255)
            b2 = iand(int(bytes(i + 1)), 255)
            if (b1 == int(z'E2') .and. b2 >= int(z'A0') .and. b2 <= int(z'A3')) then
                found = .true.
                return
            end if
        end do
    end function file_has_braille

end program test_braille
