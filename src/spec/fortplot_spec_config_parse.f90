module fortplot_spec_config_parse
    !! JSON parser for the Vega-Lite config block and related top-level
    !! properties (padding, autosize).

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spec_config_types, only: config_t, config_axis_t, &
        config_view_t, config_mark_defaults_t, config_title_t, &
        config_legend_t, padding_t
    use fortplot_spec_json_reader, only: skip_ws, expect_char, &
        read_string, read_real, read_int, read_bool, skip_value
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

    subroutine parse_config_title(json, pos, tc, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(config_title_t), intent(inout) :: tc
        integer, intent(out) :: status

        character(len=:), allocatable :: key, sval

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 32
            return
        end if
        tc%defined = .true.

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
                status = 33
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('fontSize')
                call read_real(json, pos, tc%font_size, status)
            case ('font')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    tc%font = sval(1:min(len(sval), 40))
                    tc%font_set = .true.
                end if
            case ('fontWeight')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    tc%font_weight = sval(1:min(len(sval), 20))
                    tc%font_weight_set = .true.
                end if
            case ('anchor')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    tc%anchor = sval(1:min(len(sval), 10))
                    tc%anchor_set = .true.
                end if
            case ('offset')
                call read_real(json, pos, tc%offset, status)
            case ('color')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    tc%color = sval(1:min(len(sval), 7))
                    tc%color_set = .true.
                end if
            case default
                call skip_value(json, pos)
            end select
            if (status /= 0) return
        end do
    end subroutine parse_config_title

    subroutine parse_config_legend(json, pos, lg, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(config_legend_t), intent(inout) :: lg
        integer, intent(out) :: status

        character(len=:), allocatable :: key, sval

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 34
            return
        end if
        lg%defined = .true.

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
                status = 35
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('orient')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    lg%orient = sval(1:min(len(sval), 20))
                    lg%orient_set = .true.
                end if
            case ('labelFontSize')
                call read_real(json, pos, &
                    lg%label_font_size, status)
            case ('symbolStrokeWidth')
                call read_real(json, pos, &
                    lg%symbol_stroke_width, status)
            case ('fillColor')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    lg%fill_color = sval(1:min(len(sval), 7))
                    lg%fill_color_set = .true.
                end if
            case ('strokeColor')
                call read_string(json, pos, sval, status)
                if (status == 0 .and. allocated(sval)) then
                    lg%stroke_color = sval(1:min(len(sval), 7))
                    lg%stroke_color_set = .true.
                end if
            case ('cornerRadius')
                call read_real(json, pos, lg%corner_radius, status)
            case ('padding')
                call read_real(json, pos, lg%padding, status)
            case ('frameAlpha')
                call read_real(json, pos, lg%frame_alpha, status)
            case default
                call skip_value(json, pos)
            end select
            if (status /= 0) return
        end do
    end subroutine parse_config_legend

    subroutine parse_config_range(json, pos, cfg, status)
        !! Parse config.range object, extracting category colors.
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(config_t), intent(inout) :: cfg
        integer, intent(out) :: status

        character(len=:), allocatable :: key

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 36
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
                status = 37
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('category')
                call parse_color_array(json, pos, cfg, status)
            case default
                call skip_value(json, pos)
            end select
            if (status /= 0) return
        end do
    end subroutine parse_config_range

    subroutine parse_color_array(json, pos, cfg, status)
        !! Parse a JSON array of color strings into cfg%category_colors.
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(config_t), intent(inout) :: cfg
        integer, intent(out) :: status

        character(len=:), allocatable :: sval
        integer :: count

        status = 0
        call skip_ws(json, pos)
        if (.not. expect_char(json, pos, '[')) then
            status = 38
            return
        end if

        count = 0
        do
            call skip_ws(json, pos)
            if (pos > len(json)) exit
            if (json(pos:pos) == ']') then
                pos = pos + 1
                cfg%category_color_count = count
                return
            end if
            if (json(pos:pos) == ',') pos = pos + 1
            call skip_ws(json, pos)
            if (json(pos:pos) == ']') then
                pos = pos + 1
                cfg%category_color_count = count
                return
            end if

            call read_string(json, pos, sval, status)
            if (status /= 0) return
            count = count + 1
            if (count <= 10 .and. allocated(sval)) then
                cfg%category_colors(count) = &
                    sval(1:min(len(sval), 7))
            end if
        end do
        cfg%category_color_count = min(count, 10)
    end subroutine parse_color_array

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
