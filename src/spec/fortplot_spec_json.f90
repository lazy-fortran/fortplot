module fortplot_spec_json
    !! JSON serialization for spec_t -> Vega-Lite JSON
    !!
    !! Produces valid Vega-Lite v5 JSON from Fortran spec_t types.
    !! Row-oriented output: data.values is an array of objects.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spec_types, only: spec_t, mark_t, encoding_t, &
                                   channel_t, data_t, data_column_t, scale_t, axis_t, &
                                   field_plot_t, layer_t
    implicit none

    private
    public :: spec_to_json, spec_to_json_file

    character(len=*), parameter :: VL_SCHEMA = &
                                   'https://vega.github.io/schema/vega-lite/v5.json'
    character(len=*), parameter :: NL = new_line('a')
    character(len=*), parameter :: Q = '"'

    public :: escape_json_string

contains

    pure function escape_json_string(s) result(escaped)
        !! Escape a string for safe JSON embedding.
        !! Handles: " -> \", \ -> \\, and control characters.
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: escaped
        integer :: i
        character(len=1) :: ch

        escaped = ''
        do i = 1, len(s)
            ch = s(i:i)
            select case (ch)
            case ('"')
                escaped = escaped//'\"'
            case ('\')
                escaped = escaped//'\\'
            case (char(8))
                escaped = escaped//'\b'
            case (char(9))
                escaped = escaped//'\t'
            case (char(10))
                escaped = escaped//'\n'
            case (char(12))
                escaped = escaped//'\f'
            case (char(13))
                escaped = escaped//'\r'
            case default
                escaped = escaped//ch
            end select
        end do
    end function escape_json_string

    function spec_to_json(spec) result(json)
        !! Serialize spec_t to a Vega-Lite JSON string
        type(spec_t), intent(in) :: spec
        character(len=:), allocatable :: json

        json = '{'//NL
        json = json//'  "$schema": '//Q//VL_SCHEMA//Q

        if (allocated(spec%title)) then
            json = json//','//NL
            json = json//'  "title": '//Q// &
                   escape_json_string(spec%title)//Q
        end if

        json = json//','//NL
        json = json//'  "width": '//int_to_str(spec%width)
        json = json//','//NL
        json = json//'  "height": '//int_to_str(spec%height)

        if (spec%is_layered .and. spec%layer_count > 0) then
            json = json//','//NL
            json = json//serialize_data(spec%data, 2)
            json = json//','//NL
            json = json//serialize_encoding(spec%encoding, 2)
            if (spec%field%defined) then
                json = json//','//NL
                json = json//serialize_field_plot(spec%field, 2)
            end if
            json = json//','//NL
            json = json//serialize_layers(spec%layers, &
                                          spec%layer_count)
        else
            json = json//','//NL
            json = json//serialize_data(spec%data, 2)
            json = json//','//NL
            json = json//serialize_mark(spec%mark, 2)
            json = json//','//NL
            json = json//serialize_encoding(spec%encoding, 2)
            if (spec%field%defined) then
                json = json//','//NL
                json = json//serialize_field_plot(spec%field, 2)
            end if
        end if

        json = json//NL//'}'
    end function spec_to_json

    subroutine spec_to_json_file(spec, filename, status)
        !! Write spec_t as Vega-Lite JSON to a file
        type(spec_t), intent(in) :: spec
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status

        character(len=:), allocatable :: json
        integer :: unit_num, ios

        status = 0
        json = spec_to_json(spec)

        open (newunit=unit_num, file=filename, status='replace', &
              action='write', iostat=ios)
        if (ios /= 0) then
            status = 1
            return
        end if

        write (unit_num, '(a)', iostat=ios) json
        if (ios /= 0) status = 1
        close (unit_num)
    end subroutine spec_to_json_file

    function serialize_mark(m, indent) result(json)
        !! Serialize mark_t to JSON
        type(mark_t), intent(in) :: m
        integer, intent(in) :: indent
        character(len=:), allocatable :: json
        character(len=:), allocatable :: pad
        logical :: has_props

        pad = repeat(' ', indent)

        has_props = (m%size > 0.0_wp) .or. &
                    (m%opacity < 1.0_wp) .or. &
                    (m%stroke_width >= 0.0_wp) .or. &
                    allocated(m%stroke) .or. &
                    allocated(m%fill) .or. &
                    (.not. m%filled) .or. &
                    allocated(m%interpolate) .or. &
                    allocated(m%point)

        if (.not. has_props) then
            json = pad//'"mark": '//Q//m%type//Q
            return
        end if

        json = pad//'"mark": {'//NL
        json = json//pad//'  "type": '//Q//m%type//Q
        if (m%size > 0.0_wp) then
            json = json//','//NL
            json = json//pad//'  "size": '// &
                   real_to_str(m%size)
        end if
        if (m%opacity < 1.0_wp) then
            json = json//','//NL
            json = json//pad//'  "opacity": '// &
                   real_to_str(m%opacity)
        end if
        if (m%stroke_width >= 0.0_wp) then
            json = json//','//NL
            json = json//pad//'  "strokeWidth": '// &
                   real_to_str(m%stroke_width)
        end if
        if (allocated(m%stroke)) then
            json = json//','//NL
            json = json//pad//'  "stroke": '//Q// &
                   m%stroke//Q
        end if
        if (allocated(m%fill)) then
            json = json//','//NL
            json = json//pad//'  "fill": '//Q// &
                   m%fill//Q
        end if
        if (.not. m%filled) then
            json = json//','//NL
            json = json//pad//'  "filled": false'
        end if
        if (allocated(m%interpolate)) then
            json = json//','//NL
            json = json//pad//'  "interpolate": '//Q// &
                   m%interpolate//Q
        end if
        if (allocated(m%point)) then
            json = json//','//NL
            json = json//pad//'  "point": '//m%point
        end if
        json = json//NL//pad//'}'
    end function serialize_mark

    function serialize_encoding(enc, indent) result(json)
        !! Serialize encoding_t to JSON
        type(encoding_t), intent(in) :: enc
        integer, intent(in) :: indent
        character(len=:), allocatable :: json, pad
        logical :: first

        pad = repeat(' ', indent)
        json = pad//'"encoding": {'//NL
        first = .true.

        call append_channel(json, 'x', enc%x, indent + 2, first)
        call append_channel(json, 'y', enc%y, indent + 2, first)
        call append_channel(json, 'x2', enc%x2, indent + 2, first)
        call append_channel(json, 'y2', enc%y2, indent + 2, first)
        call append_channel(json, 'color', enc%color, &
                            indent + 2, first)
        call append_channel(json, 'size', enc%size, &
                            indent + 2, first)
        call append_channel(json, 'shape', enc%shape, &
                            indent + 2, first)
        call append_channel(json, 'opacity', enc%opacity, &
                            indent + 2, first)
        call append_channel(json, 'text', enc%text, &
                            indent + 2, first)

        json = json//NL//pad//'}'
    end function serialize_encoding

    subroutine append_channel(json, name, ch, indent, first)
        !! Append a channel to the JSON encoding block
        character(len=:), allocatable, intent(inout) :: json
        character(len=*), intent(in) :: name
        type(channel_t), intent(in) :: ch
        integer, intent(in) :: indent
        logical, intent(inout) :: first
        character(len=:), allocatable :: pad

        if (.not. ch%defined) return

        pad = repeat(' ', indent)
        if (.not. first) then
            json = json//','//NL
        end if
        first = .false.

        json = json//pad//Q//name//Q//': {'
        if (allocated(ch%field)) then
            json = json//NL//pad//'  "field": '// &
                   Q//ch%field//Q
            if (allocated(ch%type)) then
                json = json//','//NL
                json = json//pad//'  "type": '// &
                       Q//ch%type//Q
            end if
        else if (allocated(ch%value)) then
            json = json//NL//pad//'  "value": '//ch%value
        end if

        call append_scale(json, ch%scale, indent + 2)
        call append_axis(json, ch%axis, indent + 2)

        json = json//NL//pad//'}'
    end subroutine append_channel

    subroutine append_scale(json, sc, indent)
        !! Append scale properties to channel JSON
        character(len=:), allocatable, intent(inout) :: json
        type(scale_t), intent(in) :: sc
        integer, intent(in) :: indent
        character(len=:), allocatable :: pad
        logical :: has_content, first_prop

        has_content = allocated(sc%type) .or. sc%domain_set .or. &
                      sc%zero .or. &
                      (abs(sc%exponent - 1.0_wp) > 1.0d-10)
        if (.not. has_content) return

        pad = repeat(' ', indent)
        json = json//','//NL
        json = json//pad//'"scale": {'
        first_prop = .true.

        if (allocated(sc%type)) then
            json = json//NL//pad//'  "type": '// &
                   Q//sc%type//Q
            first_prop = .false.
        end if
        if (sc%domain_set) then
            if (.not. first_prop) json = json//','
            json = json//NL//pad//'  "domain": ['// &
                   real_to_str(sc%domain_min)//', '// &
                   real_to_str(sc%domain_max)//']'
            first_prop = .false.
        end if
        if (sc%zero) then
            if (.not. first_prop) json = json//','
            json = json//NL//pad//'  "zero": true'
            first_prop = .false.
        end if
        if (abs(sc%exponent - 1.0_wp) > 1.0d-10) then
            if (.not. first_prop) json = json//','
            json = json//NL//pad//'  "exponent": '// &
                   real_to_str(sc%exponent)
        end if
        json = json//NL//pad//'}'
    end subroutine append_scale

    subroutine append_axis(json, ax, indent)
        !! Append axis properties to channel JSON
        character(len=:), allocatable, intent(inout) :: json
        type(axis_t), intent(in) :: ax
        integer, intent(in) :: indent
        character(len=:), allocatable :: pad
        logical :: has_content

        has_content = ax%title_set .or. ax%grid .or. &
                      (abs(ax%label_angle) > 1.0d-10)
        if (.not. has_content) return

        pad = repeat(' ', indent)
        json = json//','//NL
        json = json//pad//'"axis": {'

        if (ax%title_set .and. allocated(ax%title)) then
            json = json//NL//pad//'  "title": '// &
                   Q//escape_json_string(ax%title)//Q
            if (ax%grid) json = json//','
        end if
        if (ax%grid) then
            json = json//NL//pad//'  "grid": true'
        end if
        if (abs(ax%label_angle) > 1.0d-10) then
            if (ax%title_set .or. ax%grid) json = json//','
            json = json//NL//pad//'  "labelAngle": '// &
                   real_to_str(ax%label_angle)
        end if
        json = json//NL//pad//'}'
    end subroutine append_axis

    function serialize_data(d, indent) result(json)
        !! Serialize data_t to JSON (row-oriented for Vega-Lite)
        type(data_t), intent(in) :: d
        integer, intent(in) :: indent
        character(len=:), allocatable :: json, pad, row_pad
        integer :: i, j, ncols
        logical :: first_field

        pad = repeat(' ', indent)
        row_pad = repeat(' ', indent + 4)

        if (.not. allocated(d%columns) .or. d%nrows == 0) then
            json = pad//'"data": {"values": []}'
            return
        end if

        ncols = size(d%columns)
        json = pad//'"data": {"values": ['//NL

        do i = 1, d%nrows
            if (i > 1) json = json//','//NL
            json = json//row_pad//'{'
            first_field = .true.
            do j = 1, ncols
                if (.not. first_field) json = json//', '
                first_field = .false.
                if (d%columns(j)%is_string) then
                    json = json//Q// &
                           escape_json_string( &
                           d%columns(j)%field)//Q// &
                           ': '//Q// &
                           escape_json_string(trim( &
                                              d%columns(j)%string_values(i)))//Q
                else
                    json = json//Q// &
                           escape_json_string( &
                           d%columns(j)%field)//Q// &
                           ': '//real_to_str( &
                           d%columns(j)%values(i))
                end if
            end do
            json = json//'}'
        end do

        json = json//NL//pad//'  ]}'
    end function serialize_data

    function serialize_layers(layers, nlayers) result(json)
        !! Serialize layer array to JSON
        type(layer_t), intent(in) :: layers(:)
        integer, intent(in) :: nlayers
        character(len=:), allocatable :: json
        integer :: i

        json = '  "layer": ['//NL
        do i = 1, nlayers
            if (i > 1) json = json//','//NL
            json = json//'    {'//NL
            json = json//serialize_mark(layers(i)%mark, 6)
            json = json//','//NL
            json = json//serialize_encoding( &
                   layers(i)%encoding, 6)
            if (layers(i)%has_data) then
                json = json//','//NL
                json = json//serialize_data( &
                       layers(i)%data, 6)
            end if
            if (layers(i)%field%defined) then
                json = json//','//NL
                json = json//serialize_field_plot( &
                       layers(i)%field, 6)
            end if
            json = json//NL//'    }'
        end do
        json = json//NL//'  ]'
    end function serialize_layers

    function serialize_field_plot(field, indent) result(json)
        type(field_plot_t), intent(in) :: field
        integer, intent(in) :: indent
        character(len=:), allocatable :: json, pad
        logical :: first

        pad = repeat(' ', indent)
        json = pad//'"fortplotField": {'//NL
        first = .true.

        call append_field_real_array(json, 'x', field%x, indent + 2, first)
        call append_field_real_array(json, 'y', field%y, indent + 2, first)
        call append_field_real_array(json, 'z', field%z, indent + 2, first)
        call append_field_real_array(json, 'u', field%u, indent + 2, first)
        call append_field_real_array(json, 'v', field%v, indent + 2, first)
        call append_field_real_array(json, 'levels', field%levels, indent + 2, first)
        call append_field_int(json, 'nrows', field%nrows, indent + 2, first)
        call append_field_int(json, 'ncols', field%ncols, indent + 2, first)
        call append_field_string(json, 'colormap', field%colormap, indent + 2, first)
        if (field%show_colorbar_set) then
            call append_field_bool(json, 'showColorbar', field%show_colorbar, &
                                   indent + 2, first)
        end if
        if (field%density >= 0.0_wp) then
            call append_field_real(json, 'density', field%density, indent + 2, first)
        end if
        if (field%vmin_set) then
            call append_field_real(json, 'vmin', field%vmin, indent + 2, first)
        end if
        if (field%vmax_set) then
            call append_field_real(json, 'vmax', field%vmax, indent + 2, first)
        end if
        if (field%linewidths >= 0.0_wp) then
         call append_field_real(json, 'linewidths', field%linewidths, indent + 2, first)
        end if

        json = json//NL//pad//'}'
    end function serialize_field_plot

    subroutine append_field_real_array(json, name, values, indent, first)
        character(len=:), allocatable, intent(inout) :: json
        character(len=*), intent(in) :: name
        real(wp), allocatable, intent(in) :: values(:)
        integer, intent(in) :: indent
        logical, intent(inout) :: first
        integer :: i
        character(len=:), allocatable :: pad

        if (.not. allocated(values)) return
        pad = repeat(' ', indent)
        if (.not. first) json = json//','//NL
        first = .false.
        json = json//pad//Q//name//Q//': ['
        do i = 1, size(values)
            if (i > 1) json = json//', '
            json = json//real_to_str(values(i))
        end do
        json = json//']'
    end subroutine append_field_real_array

    subroutine append_field_string(json, name, value, indent, first)
        character(len=:), allocatable, intent(inout) :: json
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(in) :: value
        integer, intent(in) :: indent
        logical, intent(inout) :: first
        character(len=:), allocatable :: pad

        if (.not. allocated(value)) return
        pad = repeat(' ', indent)
        if (.not. first) json = json//','//NL
        first = .false.
        json = json//pad//Q//name//Q//': '//Q//escape_json_string(value)//Q
    end subroutine append_field_string

    subroutine append_field_bool(json, name, value, indent, first)
        character(len=:), allocatable, intent(inout) :: json
        character(len=*), intent(in) :: name
        logical, intent(in) :: value
        integer, intent(in) :: indent
        logical, intent(inout) :: first
        character(len=:), allocatable :: pad

        pad = repeat(' ', indent)
        if (.not. first) json = json//','//NL
        first = .false.
        json = json//pad//Q//name//Q//': '
        if (value) then
            json = json//'true'
        else
            json = json//'false'
        end if
    end subroutine append_field_bool

    subroutine append_field_int(json, name, value, indent, first)
        character(len=:), allocatable, intent(inout) :: json
        character(len=*), intent(in) :: name
        integer, intent(in) :: value
        integer, intent(in) :: indent
        logical, intent(inout) :: first
        character(len=:), allocatable :: pad

        if (value <= 0) return
        pad = repeat(' ', indent)
        if (.not. first) json = json//','//NL
        first = .false.
        json = json//pad//Q//name//Q//': '//int_to_str(value)
    end subroutine append_field_int

    subroutine append_field_real(json, name, value, indent, first)
        character(len=:), allocatable, intent(inout) :: json
        character(len=*), intent(in) :: name
        real(wp), intent(in) :: value
        integer, intent(in) :: indent
        logical, intent(inout) :: first
        character(len=:), allocatable :: pad

        pad = repeat(' ', indent)
        if (.not. first) json = json//','//NL
        first = .false.
        json = json//pad//Q//name//Q//': '//real_to_str(value)
    end subroutine append_field_real

    pure function int_to_str(n) result(s)
        !! Convert integer to trimmed string
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=20) :: buf

        write (buf, '(i0)') n
        s = trim(buf)
    end function int_to_str

    pure function real_to_str(x) result(s)
        !! Convert real to compact JSON number string.
        !! NaN and Infinity produce "null" (valid JSON).
        real(wp), intent(in) :: x
        character(len=:), allocatable :: s
        character(len=30) :: buf
        integer :: i
        logical :: is_integer

        if (x /= x) then
            s = 'null'
            return
        end if
        if (abs(x) > huge(x)) then
            s = 'null'
            return
        end if

        is_integer = .false.
        if (abs(x) <= real(huge(1), wp)) then
            is_integer = abs(x - nint(x)) < 1.0d-10
        end if

        if (is_integer) then
            write (buf, '(i0)') nint(x)
        else
            write (buf, '(es17.10)') x
            buf = adjustl(buf)
            i = len_trim(buf)
            do while (i > 1 .and. buf(i:i) == '0')
                if (buf(i - 1:i - 1) == '.') exit
                i = i - 1
            end do
            buf(i + 1:) = ' '
        end if
        s = trim(adjustl(buf))
    end function real_to_str

end module fortplot_spec_json
