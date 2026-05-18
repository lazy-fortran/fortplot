module fortplot_spec_config_parse
    !! JSON parser for the Vega-Lite config block and related top-level
    !! properties (padding, autosize).

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spec_config_types, only: config_t, padding_t
    use fortplot_spec_json_reader, only: skip_ws, expect_char, &
        read_string, read_real, read_int, read_bool, skip_value
    use fortplot_spec_config_axes, only: parse_config_axis, &
        parse_config_view, parse_config_line, parse_config_point, &
        parse_config_bar
    use fortplot_spec_config_styling, only: parse_config_title, &
        parse_config_legend, parse_config_range
    implicit none

    private
    public :: parse_config, parse_padding, parse_autosize

contains

    subroutine parse_config(json, pos, cfg, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(config_t), intent(inout) :: cfg
        integer, intent(out) :: status

        character(len=:), allocatable :: key, sval

        status = 0
        call skip_ws(json, pos)
        if (.not. expect_char(json, pos, '{')) then
            status = 20
            return
        end if

        cfg%defined = .true.

        do
            call skip_ws(json, pos)
            if (pos > len(json)) exit
            if (json(pos:pos) == '}') then
                pos = pos + 1
                return
            end if
            if (json(pos:pos) == ',') pos = pos + 1

            call skip_ws(json, pos)
            if (pos > len(json)) exit
            if (json(pos:pos) == '}') then
                pos = pos + 1
                return
            end if

            call read_string(json, pos, key, status)
            if (status /= 0) return
            call skip_ws(json, pos)
            if (.not. expect_char(json, pos, ':')) then
                status = 21
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('background')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    cfg%background = sval(1:min(len(sval), 7))
                    cfg%background_set = .true.
                end if
            case ('axis')
                call parse_config_axis(json, pos, cfg%axis, status)
            case ('view')
                call parse_config_view(json, pos, cfg%view, status)
            case ('line')
                call parse_config_line(json, pos, cfg%mark, status)
            case ('point')
                call parse_config_point(json, pos, cfg%mark, status)
            case ('bar')
                call parse_config_bar(json, pos, cfg%mark, status)
            case ('title')
                call parse_config_title(json, pos, &
                    cfg%title_config, status)
            case ('legend')
                call parse_config_legend(json, pos, &
                    cfg%legend, status)
            case ('range')
                call parse_config_range(json, pos, cfg, status)
            case default
                call skip_value(json, pos)
            end select

            if (status /= 0) return
        end do
    end subroutine parse_config

    subroutine parse_padding(json, pos, pad, status)
        !! Parse padding as integer object or single integer.
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(padding_t), intent(inout) :: pad
        integer, intent(out) :: status

        character(len=:), allocatable :: key
        integer :: ival

        status = 0
        call skip_ws(json, pos)

        ! Padding can be a single number or an object
        if (json(pos:pos) /= '{') then
            call read_int(json, pos, ival, status)
            if (status == 0) then
                pad%left = ival
                pad%right = ival
                pad%top = ival
                pad%bottom = ival
                pad%defined = .true.
            end if
            return
        end if

        if (.not. expect_char(json, pos, '{')) then
            status = 39
            return
        end if
        pad%defined = .true.

        do
            call skip_ws(json, pos)
            if (pos > len(json)) exit
            if (json(pos:pos) == '}') then
                pos = pos + 1
                return
            end if
            if (json(pos:pos) == ',') pos = pos + 1
            call skip_ws(json, pos)
            if (json(pos:pos) == '}') then
                pos = pos + 1
                return
            end if

            call read_string(json, pos, key, status)
            if (status /= 0) return
            call skip_ws(json, pos)
            if (.not. expect_char(json, pos, ':')) then
                status = 40
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('left')
                call read_int(json, pos, pad%left, status)
            case ('right')
                call read_int(json, pos, pad%right, status)
            case ('top')
                call read_int(json, pos, pad%top, status)
            case ('bottom')
                call read_int(json, pos, pad%bottom, status)
            case default
                call skip_value(json, pos)
            end select
            if (status /= 0) return
        end do
    end subroutine parse_padding

    subroutine parse_autosize(json, pos, autosize_type, status)
        !! Parse autosize as a string or object (extracting type).
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        character(len=:), allocatable, intent(inout) :: autosize_type
        integer, intent(out) :: status

        character(len=:), allocatable :: key, sval

        status = 0
        call skip_ws(json, pos)

        ! autosize can be a string or an object
        if (json(pos:pos) == '"') then
            call read_string(json, pos, autosize_type, status)
            return
        end if

        if (.not. expect_char(json, pos, '{')) then
            status = 41
            return
        end if

        do
            call skip_ws(json, pos)
            if (pos > len(json)) exit
            if (json(pos:pos) == '}') then
                pos = pos + 1
                return
            end if
            if (json(pos:pos) == ',') pos = pos + 1
            call skip_ws(json, pos)
            if (json(pos:pos) == '}') then
                pos = pos + 1
                return
            end if

            call read_string(json, pos, key, status)
            if (status /= 0) return
            call skip_ws(json, pos)
            if (.not. expect_char(json, pos, ':')) then
                status = 42
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('type')
                call read_string(json, pos, sval, status)
                if (status == 0) autosize_type = sval
            case default
                call skip_value(json, pos)
            end select
            if (status /= 0) return
        end do
    end subroutine parse_autosize

end module fortplot_spec_config_parse
