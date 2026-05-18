module fortplot_spec_config_axes
    !! Parse Vega-Lite config axis, view, and mark defaults sub-objects.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spec_config_types, only: config_axis_t, &
        config_view_t, config_mark_defaults_t
    use fortplot_spec_json_reader, only: skip_ws, expect_char, &
        read_string, read_real, read_bool, skip_value
    implicit none

    private
    public :: parse_config_axis, parse_config_view, &
        parse_config_line, parse_config_point, parse_config_bar

contains

    subroutine parse_config_axis(json, pos, ax, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(config_axis_t), intent(inout) :: ax
        integer, intent(out) :: status

        character(len=:), allocatable :: key, sval
        logical :: bval

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 22
            return
        end if
        ax%defined = .true.

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
                status = 23
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('domain')
                call read_bool(json, pos, bval, status)
                if (status == 0) then
                    ax%domain = bval
                    ax%domain_set = .true.
                end if
            case ('grid')
                call read_bool(json, pos, bval, status)
                if (status == 0) then
                    ax%grid = bval
                    ax%grid_set = .true.
                end if
            case ('gridOpacity')
                call read_real(json, pos, ax%grid_opacity, status)
            case ('gridWidth')
                call read_real(json, pos, ax%grid_width, status)
            case ('gridColor')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    ax%grid_color = sval(1:min(len(sval), 7))
                    ax%grid_color_set = .true.
                end if
            case ('labelFontSize')
                call read_real(json, pos, &
                    ax%label_font_size, status)
            case ('titleFontSize')
                call read_real(json, pos, &
                    ax%title_font_size, status)
            case ('tickSize')
                call read_real(json, pos, ax%tick_size, status)
            case ('tickWidth')
                call read_real(json, pos, ax%tick_width, status)
            case ('tickColor')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    ax%tick_color = sval(1:min(len(sval), 7))
                    ax%tick_color_set = .true.
                end if
            case ('labelFont')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    ax%label_font = sval(1:min(len(sval), 40))
                    ax%label_font_set = .true.
                end if
            case ('titleFont')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    ax%title_font = sval(1:min(len(sval), 40))
                    ax%title_font_set = .true.
                end if
            case ('titleFontWeight')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    ax%title_font_weight = &
                        sval(1:min(len(sval), 20))
                    ax%title_font_weight_set = .true.
                end if
            case default
                call skip_value(json, pos)
            end select
            if (status /= 0) return
        end do
    end subroutine parse_config_axis

    subroutine parse_config_view(json, pos, vw, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(config_view_t), intent(inout) :: vw
        integer, intent(out) :: status

        character(len=:), allocatable :: key, sval

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 24
            return
        end if
        vw%defined = .true.

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
                status = 25
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('stroke')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    vw%stroke = sval(1:min(len(sval), 7))
                    vw%stroke_set = .true.
                end if
            case ('strokeWidth')
                call read_real(json, pos, vw%stroke_width, status)
            case default
                call skip_value(json, pos)
            end select
            if (status /= 0) return
        end do
    end subroutine parse_config_view

    subroutine parse_config_line(json, pos, mk, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(config_mark_defaults_t), intent(inout) :: mk
        integer, intent(out) :: status

        character(len=:), allocatable :: key

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 26
            return
        end if
        mk%defined = .true.

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
                status = 27
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('strokeWidth')
                call read_real(json, pos, &
                    mk%line_stroke_width, status)
            case default
                call skip_value(json, pos)
            end select
            if (status /= 0) return
        end do
    end subroutine parse_config_line

    subroutine parse_config_point(json, pos, mk, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(config_mark_defaults_t), intent(inout) :: mk
        integer, intent(out) :: status

        character(len=:), allocatable :: key
        logical :: bval

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 28
            return
        end if
        mk%defined = .true.

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
                status = 29
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('filled')
                call read_bool(json, pos, bval, status)
                if (status == 0) then
                    mk%point_filled = bval
                    mk%point_filled_set = .true.
                end if
            case ('size')
                call read_real(json, pos, mk%point_size, status)
            case default
                call skip_value(json, pos)
            end select
            if (status /= 0) return
        end do
    end subroutine parse_config_point

    subroutine parse_config_bar(json, pos, mk, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(config_mark_defaults_t), intent(inout) :: mk
        integer, intent(out) :: status

        character(len=:), allocatable :: key, sval

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 30
            return
        end if
        mk%defined = .true.

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
                status = 31
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('fill')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    mk%bar_fill = sval(1:min(len(sval), 7))
                    mk%bar_fill_set = .true.
                end if
            case default
                call skip_value(json, pos)
            end select
            if (status /= 0) return
        end do
    end subroutine parse_config_bar

end module fortplot_spec_config_axes
