module fortplot_tt_binary
    !! Big-endian binary readers for TrueType font parsing.
    !! Fortran equivalents of stb_truetype.h macros: ttBYTE, ttUSHORT, ttSHORT, ttULONG.
    !! All offsets are 0-based to match the TTF specification directly.
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none

    private
    public :: tt_byte, tt_ushort, tt_short, tt_ulong
    public :: tt_tag_match, tt_load_file

contains

    pure function tt_byte(data, offset) result(val)
        !! Read one unsigned byte (0..255) at a 0-based offset.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: offset
        integer :: val
        val = iand(int(data(offset + 1)), 255)
    end function tt_byte

    pure function tt_ushort(data, offset) result(val)
        !! Read big-endian unsigned 16-bit integer (0..65535).
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: offset
        integer :: val
        val = ishft(tt_byte(data, offset), 8) + tt_byte(data, offset + 1)
    end function tt_ushort

    pure function tt_short(data, offset) result(val)
        !! Read big-endian signed 16-bit integer (-32768..32767).
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: offset
        integer :: val
        val = tt_ushort(data, offset)
        if (val >= 32768) val = val - 65536
    end function tt_short

    pure function tt_ulong(data, offset) result(val)
        !! Read big-endian unsigned 32-bit integer via bit operations.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: offset
        integer :: val
        val = ior(ishft(tt_byte(data, offset), 24), &
                  ior(ishft(tt_byte(data, offset + 1), 16), &
                      ior(ishft(tt_byte(data, offset + 2), 8), &
                          tt_byte(data, offset + 3))))
    end function tt_ulong

    pure function tt_tag_match(data, offset, tag) result(match)
        !! Compare 4 bytes at offset to a character tag (e.g. "cmap", "head").
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: offset
        character(len=4), intent(in) :: tag
        logical :: match
        match = tt_byte(data, offset) == iachar(tag(1:1)) .and. &
                tt_byte(data, offset + 1) == iachar(tag(2:2)) .and. &
                tt_byte(data, offset + 2) == iachar(tag(3:3)) .and. &
                tt_byte(data, offset + 3) == iachar(tag(4:4))
    end function tt_tag_match

    function tt_load_file(filename, data, file_size) result(success)
        !! Load entire file into a byte array via stream I/O.
        character(len=*), intent(in) :: filename
        integer(int8), allocatable, intent(out) :: data(:)
        integer, intent(out) :: file_size
        logical :: success
        integer :: unit_num, ios
        logical :: exists

        success = .false.
        file_size = 0

        inquire(file=filename, exist=exists, size=file_size)
        if (.not. exists .or. file_size <= 0) return

        allocate(data(file_size))
        open(newunit=unit_num, file=filename, access='stream', &
             form='unformatted', status='old', iostat=ios)
        if (ios /= 0) then
            deallocate(data)
            file_size = 0
            return
        end if

        read(unit_num, iostat=ios) data
        close(unit_num)

        if (ios /= 0) then
            deallocate(data)
            file_size = 0
            return
        end if

        success = .true.
    end function tt_load_file

end module fortplot_tt_binary
