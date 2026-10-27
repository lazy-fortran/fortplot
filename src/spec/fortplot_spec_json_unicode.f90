module fortplot_spec_json_unicode
    implicit none
    private
    public :: read_unicode_escape
contains
    subroutine read_hex_quad(json, pos, code, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        integer, intent(out) :: code, status
        integer :: i, digit
        code = 0
        status = 202
        if (pos + 3 > len(json)) return
        do i = 1, 4
            select case (json(pos:pos))
            case ('0':'9')
                digit = iachar(json(pos:pos)) - iachar('0')
            case ('a':'f')
                digit = iachar(json(pos:pos)) - iachar('a') + 10
            case ('A':'F')
                digit = iachar(json(pos:pos)) - iachar('A') + 10
            case default
                return
            end select
            code = 16*code + digit
            pos = pos + 1
        end do
        status = 0
    end subroutine read_hex_quad

    subroutine read_unicode_escape(json, pos, text, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status
        integer :: code, low
        pos = pos + 1
        call read_hex_quad(json, pos, code, status)
        if (status /= 0) return
        if (code >= int(z'D800') .and. code <= int(z'DBFF')) then
            status = 202
            if (pos + 1 > len(json)) return
            if (json(pos:pos + 1) /= '\u') return
            pos = pos + 2
            call read_hex_quad(json, pos, low, status)
            if (status /= 0) return
            status = 202
            if (low < int(z'DC00') .or. low > int(z'DFFF')) return
            code = int(z'10000') + (code - int(z'D800'))*1024 + low - int(z'DC00')
        else if (code >= int(z'DC00') .and. code <= int(z'DFFF')) then
            status = 202
            return
        end if
        if (code < 128) then
            text = achar(code)
        else if (code < 2048) then
            text = achar(192 + code/64)//achar(128 + mod(code, 64))
        else if (code < 65536) then
            text = achar(224 + code/4096)//achar(128 + mod(code/64, 64))// &
                achar(128 + mod(code, 64))
        else
            text = achar(240 + code/262144)//achar(128 + mod(code/4096, 64))// &
                achar(128 + mod(code/64, 64))//achar(128 + mod(code, 64))
        end if
        status = 0
    end subroutine read_unicode_escape
end module fortplot_spec_json_unicode
