module fortplot_spec_json_arrays
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spec_types, only: data_array_t, data_column_t
    use fortplot_spec_json_reader, only: skip_ws, expect_char, read_real, read_string
    implicit none
    private
    public :: parse_data_array, inspect_data_value, array_row
contains
    pure integer function array_row(column, row) result(index)
        type(data_column_t), intent(in) :: column
        integer, intent(in) :: row
        index = row
        if (allocated(column%array_rows)) index = column%array_rows(row)
    end function array_row

    subroutine inspect_data_value(json, pos, is_array, is_string, status)
        character(len=*), intent(in) :: json
        integer, intent(in) :: pos
        logical, intent(out) :: is_array, is_string
        integer, intent(out) :: status
        integer :: cursor
        type(data_array_t) :: array

        status = 0
        is_array = .false.
        is_string = .false.
        if (pos > len(json)) then
            status = 75
            return
        end if
        is_array = json(pos:pos) == '['
        if (.not. is_array) then
            is_string = json(pos:pos) == '"'
            return
        end if
        cursor = pos
        call parse_data_array(json, cursor, array, status)
        is_string = allocated(array%strings)
    end subroutine inspect_data_value

    subroutine parse_data_array(json, pos, array, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(data_array_t), intent(out) :: array
        integer, intent(out) :: status
        integer :: start, count, i, cursor, kind
        real(wp) :: number
        character(len=:), allocatable :: text

        status = 0
        if (.not. expect_char(json, pos, '[')) then
            status = 75
            return
        end if
        start = pos
        count = 0
        kind = 0
        do
            call skip_ws(json, pos)
            if (pos > len(json)) then
                status = 75
                return
            end if
            if (expect_char(json, pos, ']')) exit
            if (count > 0) then
                if (.not. expect_char(json, pos, ',')) then
                    status = 75
                    return
                end if
                call skip_ws(json, pos)
            end if
            if (pos > len(json)) then
                status = 75
                return
            end if
            if (json(pos:pos) == '"') then
                if (kind == 1) then
                    status = 76
                    return
                end if
                kind = 2
                call read_string(json, pos, text, status)
            else
                cursor = pos
                call read_real(json, pos, number, status)
                if (json(cursor:cursor) /= 'n') then
                    if (kind == 2) then
                        status = 76
                        return
                    end if
                    kind = 1
                end if
            end if
            if (status /= 0) return
            count = count + 1
        end do
        cursor = start
        if (kind == 2) then
            allocate (character(len=256) :: array%strings(count))
            array%strings = ''
            allocate (array%valid(count))
            array%valid = .false.
        else
            allocate (array%values(count))
        end if
        do i = 1, count
            call skip_ws(json, cursor)
            if (i > 1) cursor = cursor + 1
            call skip_ws(json, cursor)
            if (kind == 2 .and. json(cursor:cursor) == '"') then
                call read_string(json, cursor, text, status)
                array%strings(i) = text
                array%valid(i) = .true.
            else
                call read_real(json, cursor, number, status)
                if (kind /= 2) array%values(i) = number
            end if
            if (status /= 0) return
        end do
    end subroutine parse_data_array
end module fortplot_spec_json_arrays
