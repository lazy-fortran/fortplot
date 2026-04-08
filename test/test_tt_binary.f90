program test_tt_binary
    !! Unit tests for TrueType binary parsing primitives.
    use fortplot_tt_binary, only: tt_byte, tt_ushort, tt_short, tt_ulong, tt_tag_match
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none

    integer(int8) :: d(4)

    ! -- tt_byte --
    d(1) = 0_int8
    call ensure(tt_byte(d, 0) == 0, 'tt_byte: 0x00 -> 0')

    d(1) = 127_int8
    call ensure(tt_byte(d, 0) == 127, 'tt_byte: 0x7F -> 127')

    d(1) = int(Z'80', int8)  ! -128 signed = 128 unsigned
    call ensure(tt_byte(d, 0) == 128, 'tt_byte: 0x80 -> 128')

    d(1) = -1_int8  ! 0xFF unsigned = 255
    call ensure(tt_byte(d, 0) == 255, 'tt_byte: 0xFF -> 255')

    ! -- tt_ushort --
    d(1:2) = [0_int8, 0_int8]
    call ensure(tt_ushort(d, 0) == 0, 'tt_ushort: 0x0000 -> 0')

    d(1:2) = [0_int8, 1_int8]
    call ensure(tt_ushort(d, 0) == 1, 'tt_ushort: 0x0001 -> 1')

    d(1:2) = [1_int8, 0_int8]
    call ensure(tt_ushort(d, 0) == 256, 'tt_ushort: 0x0100 -> 256')

    d(1:2) = [-1_int8, -1_int8]
    call ensure(tt_ushort(d, 0) == 65535, 'tt_ushort: 0xFFFF -> 65535')

    d(1:2) = [int(Z'12', int8), int(Z'34', int8)]
    call ensure(tt_ushort(d, 0) == 4660, 'tt_ushort: 0x1234 -> 4660')

    ! -- tt_short --
    d(1:2) = [0_int8, 1_int8]
    call ensure(tt_short(d, 0) == 1, 'tt_short: 0x0001 -> 1')

    d(1:2) = [-1_int8, -1_int8]
    call ensure(tt_short(d, 0) == -1, 'tt_short: 0xFFFF -> -1')

    d(1:2) = [int(Z'80', int8), 0_int8]
    call ensure(tt_short(d, 0) == -32768, 'tt_short: 0x8000 -> -32768')

    d(1:2) = [int(Z'7F', int8), -1_int8]
    call ensure(tt_short(d, 0) == 32767, 'tt_short: 0x7FFF -> 32767')

    ! -- tt_ulong --
    d = [0_int8, 0_int8, 0_int8, 1_int8]
    call ensure(tt_ulong(d, 0) == 1, 'tt_ulong: 0x00000001 -> 1')

    d = [0_int8, 1_int8, 0_int8, 0_int8]
    call ensure(tt_ulong(d, 0) == 65536, 'tt_ulong: 0x00010000 -> 65536')

    d = [int(Z'12', int8), int(Z'34', int8), int(Z'56', int8), int(Z'78', int8)]
    call ensure(tt_ulong(d, 0) == 305419896, 'tt_ulong: 0x12345678 -> 305419896')

    ! -- tt_tag_match --
    d = [int(iachar('h'), int8), int(iachar('e'), int8), &
         int(iachar('a'), int8), int(iachar('d'), int8)]
    call ensure(tt_tag_match(d, 0, 'head'), 'tt_tag_match: head matches head')
    call ensure(.not. tt_tag_match(d, 0, 'hhea'), 'tt_tag_match: head /= hhea')

    print *, 'All tt_binary tests passed'

contains

    subroutine ensure(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        if (.not. cond) then
            print *, 'FAIL: ', trim(msg)
            stop 1
        end if
        print *, 'PASS: ', trim(msg)
    end subroutine ensure

end program test_tt_binary
