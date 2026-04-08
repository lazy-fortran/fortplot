module fortplot_spec_json_reader
    !! Low-level JSON token readers and I/O utilities.
    !!
    !! Provides character-level JSON parsing primitives: whitespace
    !! skipping, string/number/bool reading, and value skipping.
    !! Also provides stdin and file reading utilities.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: skip_ws, expect_char, read_string, read_real, &
              read_int, read_bool, read_literal, skip_value, &
              read_stdin, read_file

contains

    subroutine skip_ws(json, pos)
        !! Skip whitespace characters
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos

        do while (pos <= len(json))
            select case (json(pos:pos))
            case (' ', char(9), char(10), char(13))
                pos = pos + 1
            case default
                return
            end select
        end do
    end subroutine skip_ws

    function expect_char(json, pos, ch) result(ok)
        !! Expect and consume a specific character
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        character(len=1), intent(in) :: ch
        logical :: ok

        ok = .false.
        if (pos > len(json)) return
        if (json(pos:pos) == ch) then
            ok = .true.
            pos = pos + 1
        end if
    end function expect_char

    subroutine read_string(json, pos, val, status)
        !! Read a JSON quoted string, advancing pos past closing
        !! quote.  Unescapes standard JSON escape sequences.
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        character(len=:), allocatable, intent(out) :: val
        integer, intent(out) :: status

        status = 0
        if (pos > len(json) .or. json(pos:pos) /= '"') then
            status = 200
            return
        end if
        pos = pos + 1

        val = ''
        do while (pos <= len(json))
            if (json(pos:pos) == '\' .and. &
                pos + 1 <= len(json)) then
                pos = pos + 1
                select case (json(pos:pos))
                case ('"')
                    val = val//'"'
                case ('\')
                    val = val//'\'
                case ('/')
                    val = val//'/'
                case ('b')
                    val = val//char(8)
                case ('t')
                    val = val//char(9)
                case ('n')
                    val = val//char(10)
                case ('f')
                    val = val//char(12)
                case ('r')
                    val = val//char(13)
                case default
                    val = val//json(pos:pos)
                end select
                pos = pos + 1
                cycle
            end if
            if (json(pos:pos) == '"') then
                pos = pos + 1
                return
            end if
            val = val//json(pos:pos)
            pos = pos + 1
        end do

        status = 201
    end subroutine read_string

    subroutine read_real(json, pos, val, status)
        !! Read a JSON number as real(wp).
        !! Recognises JSON null and maps it to IEEE NaN.
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        real(wp), intent(out) :: val
        integer, intent(out) :: status
        integer :: start, ios

        status = 0

        if (pos + 3 <= len(json) .and. &
            json(pos:pos + 3) == 'null') then
            val = transfer(int(Z'7FF8000000000000', 8), 1.0_wp)
            pos = pos + 4
            return
        end if

        start = pos

        if (pos <= len(json) .and. json(pos:pos) == '-') then
            pos = pos + 1
        end if

        do while (pos <= len(json))
            select case (json(pos:pos))
            case ('0':'9', '.', 'e', 'E', '+', '-')
                pos = pos + 1
            case default
                exit
            end select
        end do

        if (pos == start) then
            status = 210
            return
        end if

        read (json(start:pos - 1), *, iostat=ios) val
        if (ios /= 0) status = 211
    end subroutine read_real

    subroutine read_int(json, pos, val, status)
        !! Read a JSON integer
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        integer, intent(out) :: val
        integer, intent(out) :: status
        real(wp) :: rval

        call read_real(json, pos, rval, status)
        if (status == 0) val = nint(rval)
    end subroutine read_int

    subroutine read_bool(json, pos, val, status)
        !! Read a JSON boolean (true/false)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        logical, intent(out) :: val
        integer, intent(out) :: status

        status = 0
        if (pos + 3 <= len(json) .and. &
            json(pos:pos + 3) == 'true') then
            val = .true.
            pos = pos + 4
        else if (pos + 4 <= len(json) .and. &
                 json(pos:pos + 4) == 'false') then
            val = .false.
            pos = pos + 5
        else
            status = 220
        end if
    end subroutine read_bool

    subroutine read_literal(json, pos, val, status)
        !! Read an arbitrary JSON value as a literal string.
        !! For strings returns the quoted content; for numbers,
        !! booleans, null returns the raw text.
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        character(len=:), allocatable, intent(out) :: val
        integer, intent(out) :: status
        integer :: start

        status = 0
        call skip_ws(json, pos)

        if (json(pos:pos) == '"') then
            call read_string(json, pos, val, status)
            val = '"'//val//'"'
            return
        end if

        start = pos
        do while (pos <= len(json))
            select case (json(pos:pos))
            case (',', '}', ']', ' ', char(9), char(10), char(13))
                exit
            case default
                pos = pos + 1
            end select
        end do
        val = json(start:pos - 1)
    end subroutine read_literal

    subroutine skip_value(json, pos)
        !! Skip an arbitrary JSON value (string, number, bool,
        !! null, object, or array) without storing it.
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        integer :: depth

        call skip_ws(json, pos)
        if (pos > len(json)) return

        select case (json(pos:pos))
        case ('"')
            pos = pos + 1
            do while (pos <= len(json))
                if (json(pos:pos) == '\') then
                    pos = pos + 2
                    cycle
                end if
                if (json(pos:pos) == '"') then
                    pos = pos + 1
                    return
                end if
                pos = pos + 1
            end do
        case ('{')
            depth = 1
            pos = pos + 1
            do while (pos <= len(json) .and. depth > 0)
                if (json(pos:pos) == '"') then
                    pos = pos + 1
                    do while (pos <= len(json))
                        if (json(pos:pos) == '\') then
                            pos = pos + 2
                            cycle
                        end if
                        if (json(pos:pos) == '"') then
                            pos = pos + 1
                            exit
                        end if
                        pos = pos + 1
                    end do
                    cycle
                end if
                if (json(pos:pos) == '{') depth = depth + 1
                if (json(pos:pos) == '}') depth = depth - 1
                pos = pos + 1
            end do
        case ('[')
            depth = 1
            pos = pos + 1
            do while (pos <= len(json) .and. depth > 0)
                if (json(pos:pos) == '"') then
                    pos = pos + 1
                    do while (pos <= len(json))
                        if (json(pos:pos) == '\') then
                            pos = pos + 2
                            cycle
                        end if
                        if (json(pos:pos) == '"') then
                            pos = pos + 1
                            exit
                        end if
                        pos = pos + 1
                    end do
                    cycle
                end if
                if (json(pos:pos) == '[') depth = depth + 1
                if (json(pos:pos) == ']') depth = depth - 1
                pos = pos + 1
            end do
        case default
            do while (pos <= len(json))
                select case (json(pos:pos))
                case (',', '}', ']', ' ', char(9), char(10), &
                     char(13))
                    return
                case default
                    pos = pos + 1
                end select
            end do
        end select
    end subroutine skip_value

    subroutine read_stdin(content, status)
        !! Read entire stdin into a single string
        character(len=:), allocatable, intent(out) :: content
        integer, intent(out) :: status
        character(len=4096) :: buf
        integer :: ios, nread

        status = 0
        content = ''
        do
            read (*, '(a)', iostat=ios, advance='no', &
                  size=nread) buf
            if (ios == -1) then
                if (nread > 0) content = content//buf(1:nread)
                exit
            else if (ios == -2) then
                content = content//buf(1:nread)//new_line('a')
            else if (ios == 0) then
                content = content//buf(1:nread)
            else
                status = 1
                return
            end if
        end do
    end subroutine read_stdin

    subroutine read_file(filename, content, status)
        !! Read entire file into a single string
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: content
        integer, intent(out) :: status
        integer :: unit_num, ios, fsize
        logical :: exists

        status = 0
        inquire (file=filename, exist=exists)
        if (.not. exists) then
            status = 1
            return
        end if

        inquire (file=filename, size=fsize)
        if (fsize <= 0) then
            status = 2
            return
        end if

        allocate (character(len=fsize) :: content)
        open (newunit=unit_num, file=filename, status='old', &
              access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            status = 3
            return
        end if

        read (unit_num, iostat=ios) content
        close (unit_num)
        if (ios /= 0) status = 4
    end subroutine read_file

end module fortplot_spec_json_reader
