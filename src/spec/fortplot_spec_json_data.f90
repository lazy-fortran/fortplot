module fortplot_spec_json_data
    !! JSON data parsing: values arrays, row parsing, field scanning.
    !!
    !! Handles parsing of Vega-Lite data objects including row-oriented
    !! JSON arrays with mixed numeric/string column types.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spec_types, only: data_t, field_plot_t, layer_t, spec_t
    use fortplot_spec_json_reader, only: skip_ws, expect_char, &
                                          read_string, read_real, &
                                          read_int, read_bool, skip_value
    use fortplot_spec_json_channels, only: parse_real_array, &
                                          parse_mark, parse_encoding

    implicit none
    private

    public :: parse_data
    public :: parse_values_array
    public :: parse_field_plot
    public :: parse_layers
    public :: parse_one_layer
    public :: scan_row_fields
    public :: parse_row
    public :: find_column

contains

    subroutine parse_data(json, pos, d, status)
        !! Parse data object: {"values": [{...}, ...]}
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(data_t), intent(out) :: d
        integer, intent(out) :: status
        character(len=:), allocatable :: key

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 60
            return
        end if

        do
            call skip_ws(json, pos)
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
                status = 61
                return
            end if
            call skip_ws(json, pos)

            if (key == 'values') then
                call parse_values_array(json, pos, d, status)
            else
                call skip_value(json, pos)
            end if

            if (status /= 0) return
        end do
    end subroutine parse_data

    subroutine parse_values_array(json, pos, d, status)
        !! Parse row-oriented data: [{"x":1,"y":2}, ...]
        !! Two-pass: first count rows and discover columns,
        !! then parse values.
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(data_t), intent(out) :: d
        integer, intent(out) :: status

        integer :: save_pos, nrows, ncols, row, col
        character(len=64) :: field_names(32)
        logical :: field_is_string(32)
        integer :: nfields

        status = 0
        nfields = 0

        if (.not. expect_char(json, pos, '[')) then
            status = 70
            return
        end if

        call skip_ws(json, pos)
        if (json(pos:pos) == ']') then
            pos = pos + 1
            d%nrows = 0
            return
        end if

        save_pos = pos
        nrows = 0
        do
            call skip_ws(json, pos)
            if (json(pos:pos) == ']') exit

            if (nrows > 0) then
                if (.not. expect_char(json, pos, ',')) then
                    status = 71
                    return
                end if
                call skip_ws(json, pos)
            end if

            nrows = nrows + 1
            call scan_row_fields(json, pos, field_names, &
                                 field_is_string, nfields, status)
            if (status /= 0) return
        end do

        ncols = nfields
        if (ncols == 0 .or. nrows == 0) then
            if (.not. expect_char(json, pos, ']')) then
                status = 72
                return
            end if
            d%nrows = 0
            return
        end if

        allocate (d%columns(ncols))
        do col = 1, ncols
            d%columns(col)%field = trim(field_names(col))
            d%columns(col)%is_string = field_is_string(col)
            if (field_is_string(col)) then
                allocate (character(len=256) :: &
                          d%columns(col)%string_values(nrows))
            else
                allocate (d%columns(col)%values(nrows))
                d%columns(col)%values = 0.0_wp
            end if
        end do
        d%nrows = nrows

        pos = save_pos
        do row = 1, nrows
            call skip_ws(json, pos)
            if (row > 1) then
                if (.not. expect_char(json, pos, ',')) then
                    status = 73
                    return
                end if
                call skip_ws(json, pos)
            end if
            call parse_row(json, pos, d, row, field_names, &
                           ncols, status)
            if (status /= 0) return
        end do

        call skip_ws(json, pos)
        if (.not. expect_char(json, pos, ']')) then
            status = 74
            return
        end if
    end subroutine parse_values_array

    subroutine scan_row_fields(json, pos, field_names, &
                               field_is_string, nfields, status)
        !! Scan one row object to discover field names and types.
        !! Advances pos past the entire row object.
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        character(len=64), intent(inout) :: field_names(:)
        logical, intent(inout) :: field_is_string(:)
        integer, intent(inout) :: nfields
        integer, intent(out) :: status
        character(len=:), allocatable :: key
        integer :: i
        logical :: found

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 80
            return
        end if

        do
            call skip_ws(json, pos)
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
                status = 81
                return
            end if
            call skip_ws(json, pos)

            found = .false.
            do i = 1, nfields
                if (trim(field_names(i)) == key) then
                    found = .true.
                    exit
                end if
            end do

            if (.not. found) then
                nfields = nfields + 1
                field_names(nfields) = key
                field_is_string(nfields) = (json(pos:pos) == '"')
            end if

            call skip_value(json, pos)
        end do
    end subroutine scan_row_fields

    subroutine parse_row(json, pos, d, row, field_names, &
                         ncols, status)
        !! Parse one row object and store values into data columns
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(data_t), intent(inout) :: d
        integer, intent(in) :: row, ncols
        character(len=64), intent(in) :: field_names(:)
        integer, intent(out) :: status
        character(len=:), allocatable :: key, sval
        real(wp) :: rval
        integer :: col

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 90
            return
        end if

        do
            call skip_ws(json, pos)
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
                status = 91
                return
            end if
            call skip_ws(json, pos)

            col = find_column(key, field_names, ncols)
            if (col > 0) then
                if (d%columns(col)%is_string) then
                    call read_string(json, pos, sval, status)
                    if (status /= 0) return
                    d%columns(col)%string_values(row) = sval
                else
                    call read_real(json, pos, rval, status)
                    if (status /= 0) return
                    d%columns(col)%values(row) = rval
                end if
            else
                call skip_value(json, pos)
            end if
        end do
    end subroutine parse_row

    pure function find_column(key, field_names, ncols) result(idx)
        !! Find column index by field name
        character(len=*), intent(in) :: key
        character(len=64), intent(in) :: field_names(:)
        integer, intent(in) :: ncols
        integer :: idx, i

        idx = 0
        do i = 1, ncols
            if (trim(field_names(i)) == key) then
                idx = i
                return
            end if
        end do
    end function find_column

    subroutine parse_field_plot(json, pos, field, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(field_plot_t), intent(out) :: field
        integer, intent(out) :: status
        character(len=:), allocatable :: key

        status = 0
        field%defined = .true.
        if (.not. expect_char(json, pos, '{')) then
            status = 95
            return
        end if

        do
            call skip_ws(json, pos)
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
                status = 96
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('x')
                call parse_real_array(json, pos, field%x, status)
            case ('y')
                call parse_real_array(json, pos, field%y, status)
            case ('z')
                call parse_real_array(json, pos, field%z, status)
            case ('u')
                call parse_real_array(json, pos, field%u, status)
            case ('v')
                call parse_real_array(json, pos, field%v, status)
            case ('levels')
                call parse_real_array(json, pos, field%levels, status)
            case ('nrows')
                call read_int(json, pos, field%nrows, status)
            case ('ncols')
                call read_int(json, pos, field%ncols, status)
            case ('colormap')
                call read_string(json, pos, field%colormap, status)
            case ('showColorbar')
                call read_bool(json, pos, field%show_colorbar, status)
                field%show_colorbar_set = .true.
            case ('density')
                call read_real(json, pos, field%density, status)
            case ('vmin')
                call read_real(json, pos, field%vmin, status)
                field%vmin_set = .true.
            case ('vmax')
                call read_real(json, pos, field%vmax, status)
                field%vmax_set = .true.
            case ('linewidths')
                call read_real(json, pos, field%linewidths, status)
            case default
                call skip_value(json, pos)
            end select

            if (status /= 0) return
        end do
    end subroutine parse_field_plot

    subroutine parse_layers(json, pos, spec, status)
        !! Parse layer array
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(spec_t), intent(inout) :: spec
        integer, intent(out) :: status
        type(layer_t) :: tmp_layers(64)
        integer :: nlayers

        status = 0
        spec%is_layered = .true.
        nlayers = 0

        if (.not. expect_char(json, pos, '[')) then
            status = 100
            return
        end if

        do
            call skip_ws(json, pos)
            if (json(pos:pos) == ']') then
                pos = pos + 1
                exit
            end if
            if (nlayers > 0) then
                if (.not. expect_char(json, pos, ',')) then
                    status = 101
                    return
                end if
                call skip_ws(json, pos)
            end if

            nlayers = nlayers + 1
            call parse_one_layer(json, pos, tmp_layers(nlayers), &
                                 status)
            if (status /= 0) return
        end do

        spec%layer_count = nlayers
        if (nlayers > 0) then
            allocate (spec%layers(nlayers))
            spec%layers(1:nlayers) = tmp_layers(1:nlayers)
        end if
    end subroutine parse_layers

    subroutine parse_one_layer(json, pos, lay, status)
        !! Parse a single layer object
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(layer_t), intent(out) :: lay
        integer, intent(out) :: status
        character(len=:), allocatable :: key

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 110
            return
        end if

        do
            call skip_ws(json, pos)
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
                status = 111
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('mark')
                call parse_mark(json, pos, lay%mark, status)
            case ('encoding')
                call parse_encoding(json, pos, lay%encoding, &
                                    status)
            case ('data')
                call parse_data(json, pos, lay%data, status)
                lay%has_data = .true.
            case ('fortplotField')
                call parse_field_plot(json, pos, lay%field, status)
            case default
                call skip_value(json, pos)
            end select

            if (status /= 0) return
        end do
    end subroutine parse_one_layer

end module fortplot_spec_json_data
