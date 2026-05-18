module fortplot_spec_config_styling
    !! Parse Vega-Lite config title, legend, range (color palette),
    !! and color-array helpers.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spec_config_types, only: config_t, &
        config_title_t, config_legend_t
    use fortplot_spec_json_reader, only: skip_ws, expect_char, &
        read_string, read_real, skip_value
    implicit none

    private
    public :: parse_config_title, parse_config_legend, &
        parse_config_range

contains

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

end module fortplot_spec_config_styling
