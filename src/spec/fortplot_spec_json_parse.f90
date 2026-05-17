module fortplot_spec_json_parse
    !! JSON deserializer: Vega-Lite JSON string -> spec_t
    !!
    !! Parses the subset of Vega-Lite v5 that fortplot produces and
    !! consumes. Operates as a recursive-descent parser over a JSON
    !! character string with integer position tracking.
    !!
    !! Delegates channel/scale/axis parsing to fortplot_spec_json_channels
    !! and data parsing to fortplot_spec_json_data.

    use fortplot_spec_types, only: spec_t, mark_t, encoding_t, &
                                   channel_t, layer_t
    use fortplot_spec_json_reader, only: skip_ws, expect_char, &
                                          read_string, read_real, &
                                          read_int, read_bool, &
                                          read_literal, skip_value, &
                                          read_stdin, read_file
    use fortplot_spec_config_parse, only: parse_config, &
                                           parse_padding, &
                                           parse_autosize
    use fortplot_spec_json_channels, only: parse_channel, parse_scale, &
                                          parse_axis, parse_real_array, &
                                          parse_mark, parse_encoding
    use fortplot_spec_json_data, only: parse_layers, parse_data, parse_field_plot

    implicit none
    private
    public :: json_to_spec, read_stdin, read_file

contains

    subroutine json_to_spec(json, spec, status)
        !! Parse a Vega-Lite JSON string into spec_t
        character(len=*), intent(in) :: json
        type(spec_t), intent(out) :: spec
        integer, intent(out) :: status

        integer :: pos
        character(len=:), allocatable :: key

        status = 0
        pos = 1

        call skip_ws(json, pos)
        if (.not. expect_char(json, pos, '{')) then
            status = 1
            return
        end if

        do
            call skip_ws(json, pos)
            if (pos > len(json)) then
                status = 2
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
                status = 3
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('$schema')
                call read_string(json, pos, spec%schema, status)
            case ('title')
                call read_string(json, pos, spec%title, status)
            case ('width')
                call read_int(json, pos, spec%width, status)
            case ('height')
                call read_int(json, pos, spec%height, status)
            case ('mark')
                call parse_mark(json, pos, spec%mark, status)
            case ('encoding')
                call parse_encoding(json, pos, spec%encoding, &
                                    status)
            case ('data')
                call parse_data(json, pos, spec%data, status)
            case ('fortplotField')
                call parse_field_plot(json, pos, spec%field, status)
            case ('layer')
                call parse_layers(json, pos, spec, status)
            case ('config')
                call parse_config(json, pos, spec%config, status)
            case ('padding')
                call parse_padding(json, pos, spec%padding, &
                                   status)
            case ('autosize')
                call parse_autosize(json, pos, &
                                    spec%autosize_type, status)
            case default
                call skip_value(json, pos)
            end select

            if (status /= 0) return
        end do
    end subroutine json_to_spec

end module fortplot_spec_json_parse
