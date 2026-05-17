program test_tt_collection_header
    !! Regression test for TTC (TrueType Collection) header parsing.
    !! The TTC header "version" field is a 16.16 fixed-point number:
    !! 1.0 = 0x00010000 and 2.0 = 0x00020000, not integers 1 and 2.
    !! macOS system fonts like /System/Library/Fonts/Helvetica.ttc are
    !! TTC v2.0, so the integer comparison bug made the font unloadable.
    use fortplot_tt_tables, only: tt_get_font_offset_for_index
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none

    call check_ttc_v1_zero_font_returns_zero()
    call check_ttc_v2_zero_font_returns_zero()
    call check_ttc_v2_second_font_returns_correct_offset()
    call check_ttc_invalid_version_returns_minus_one()
    call check_ttc_out_of_range_index_returns_minus_one()
    call check_standalone_font_zero_index_returns_zero()
    call check_standalone_font_nonzero_index_returns_minus_one()

    print *, 'All TTC header parsing tests passed'

contains

    subroutine check_ttc_v1_zero_font_returns_zero()
        integer(int8), allocatable :: d(:)
        d = make_ttc_header(version_hi=1, num_fonts=2, &
            offsets=[int(Z'00000030'), int(Z'00001234')])
        call ensure(tt_get_font_offset_for_index(d, 0) == int(Z'30'), &
            'TTC v1.0 index 0 -> offset 0x30')
    end subroutine check_ttc_v1_zero_font_returns_zero

    subroutine check_ttc_v2_zero_font_returns_zero()
        integer(int8), allocatable :: d(:)
        d = make_ttc_header(version_hi=2, num_fonts=6, &
            offsets=[int(Z'00000030'), int(Z'0000017C'), int(Z'000002B8'), &
                     int(Z'00000404'), int(Z'00000540'), int(Z'0000066C')])
        call ensure(tt_get_font_offset_for_index(d, 0) == int(Z'30'), &
            'TTC v2.0 (macOS Helvetica) index 0 -> offset 0x30')
        call ensure(tt_get_font_offset_for_index(d, 5) == int(Z'66C'), &
            'TTC v2.0 index 5 -> offset 0x66C')
    end subroutine check_ttc_v2_zero_font_returns_zero

    subroutine check_ttc_v2_second_font_returns_correct_offset()
        integer(int8), allocatable :: d(:)
        d = make_ttc_header(version_hi=2, num_fonts=3, &
            offsets=[int(Z'00000030'), int(Z'000000A0'), int(Z'00000110')])
        call ensure(tt_get_font_offset_for_index(d, 1) == int(Z'A0'), &
            'TTC v2.0 index 1 -> offset 0xA0')
    end subroutine check_ttc_v2_second_font_returns_correct_offset

    subroutine check_ttc_invalid_version_returns_minus_one()
        integer(int8), allocatable :: d(:)
        d = make_ttc_header(version_hi=3, num_fonts=1, &
            offsets=[int(Z'00000030')])
        call ensure(tt_get_font_offset_for_index(d, 0) == -1, &
            'TTC unknown version 3.0 -> -1')
    end subroutine check_ttc_invalid_version_returns_minus_one

    subroutine check_ttc_out_of_range_index_returns_minus_one()
        integer(int8), allocatable :: d(:)
        d = make_ttc_header(version_hi=2, num_fonts=2, &
            offsets=[int(Z'00000030'), int(Z'00000100')])
        call ensure(tt_get_font_offset_for_index(d, 2) == -1, &
            'TTC index past end -> -1')
        call ensure(tt_get_font_offset_for_index(d, -1) == -1, &
            'TTC negative index -> -1')
    end subroutine check_ttc_out_of_range_index_returns_minus_one

    subroutine check_standalone_font_zero_index_returns_zero()
        integer(int8) :: d(12)
        ! Standalone TTF signature 00 01 00 00 followed by padding.
        d = 0_int8
        d(1:4) = [0_int8, 1_int8, 0_int8, 0_int8]
        call ensure(tt_get_font_offset_for_index(d, 0) == 0, &
            'Standalone font index 0 -> 0')
    end subroutine check_standalone_font_zero_index_returns_zero

    subroutine check_standalone_font_nonzero_index_returns_minus_one()
        integer(int8) :: d(12)
        d = 0_int8
        d(1:4) = [0_int8, 1_int8, 0_int8, 0_int8]
        call ensure(tt_get_font_offset_for_index(d, 1) == -1, &
            'Standalone font index 1 -> -1')
    end subroutine check_standalone_font_nonzero_index_returns_minus_one

    function make_ttc_header(version_hi, num_fonts, offsets) result(buf)
        !! Build a minimal TTC header: 'ttcf' + version(4) + num_fonts(4)
        !! + offsets(4*n). Matches the macOS Helvetica.ttc layout.
        integer, intent(in) :: version_hi
        integer, intent(in) :: num_fonts
        integer, intent(in) :: offsets(:)
        integer(int8), allocatable :: buf(:)
        integer :: n, i, p

        n = size(offsets)
        allocate(buf(12 + 4*n))
        buf(1:4) = [int(iachar('t'), int8), int(iachar('t'), int8), &
                    int(iachar('c'), int8), int(iachar('f'), int8)]
        ! Version as 16.16 fixed-point: high word = version_hi, low word = 0.
        call write_u32(buf, 4, ishft(version_hi, 16))
        call write_u32(buf, 8, num_fonts)
        do i = 1, n
            p = 12 + 4*(i - 1)
            call write_u32(buf, p, offsets(i))
        end do
    end function make_ttc_header

    subroutine write_u32(buf, offset, val)
        !! Write val as a big-endian 32-bit unsigned integer at 0-based offset.
        integer(int8), intent(inout) :: buf(:)
        integer, intent(in) :: offset
        integer, intent(in) :: val
        buf(offset + 1) = int(iand(ishft(val, -24), 255), int8)
        buf(offset + 2) = int(iand(ishft(val, -16), 255), int8)
        buf(offset + 3) = int(iand(ishft(val, -8), 255), int8)
        buf(offset + 4) = int(iand(val, 255), int8)
    end subroutine write_u32

    subroutine ensure(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        if (.not. cond) then
            print *, 'FAIL: ', trim(msg)
            stop 1
        end if
        print *, 'PASS: ', trim(msg)
    end subroutine ensure

end program test_tt_collection_header
