module fortplot_spec_json_flatten
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use fortplot_spec_types, only: spec_t, data_t, data_column_t, flatten_t, encoding_t
    use fortplot_spec_json_arrays, only: array_row
    use fortplot_spec_json_reader, only: skip_ws, expect_char, read_string
    implicit none
    private
    public :: parse_transforms, apply_spec_transforms
contains
    subroutine parse_names(json, pos, names, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        character(len=64), allocatable, intent(out) :: names(:)
        integer, intent(out) :: status
        character(len=:), allocatable :: name

        status = 78
        allocate (names(0))
        if (.not. expect_char(json, pos, '[')) return
        do
            call skip_ws(json, pos)
            if (expect_char(json, pos, ']')) exit
            if (size(names) > 0) then
                if (.not. expect_char(json, pos, ',')) return
                call skip_ws(json, pos)
            end if
            call read_string(json, pos, name, status)
            if (status /= 0) return
            if (len(name) > 64 .or. len(name) == 0) then
                status = 78
                return
            end if
            call append_name(names, name)
        end do
        status = 0
    end subroutine parse_names

    subroutine parse_transforms(json, pos, transforms, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(flatten_t), allocatable, intent(out) :: transforms(:)
        integer, intent(out) :: status
        type(flatten_t) :: transform
        character(len=:), allocatable :: key

        status = 78
        allocate (transforms(0))
        if (.not. expect_char(json, pos, '[')) return
        do
            call skip_ws(json, pos)
            if (expect_char(json, pos, ']')) exit
            if (size(transforms) > 0) then
                if (.not. expect_char(json, pos, ',')) return
                call skip_ws(json, pos)
            end if
            if (.not. expect_char(json, pos, '{')) return
            transform = flatten_t()
            do
                call skip_ws(json, pos)
                call read_string(json, pos, key, status)
                if (status /= 0) return
                call skip_ws(json, pos)
                if (.not. expect_char(json, pos, ':')) then
                    status = 78
                    return
                end if
                call skip_ws(json, pos)
                select case (key)
                case ('flatten')
                    call parse_names(json, pos, transform%fields, status)
                case ('as')
                    call parse_names(json, pos, transform%names, status)
                case default
                    status = 79
                end select
                if (status /= 0) return
                call skip_ws(json, pos)
                if (expect_char(json, pos, '}')) exit
                if (.not. expect_char(json, pos, ',')) then
                    status = 78
                    return
                end if
            end do
            if (.not. allocated(transform%fields)) then
                status = 79
                return
            end if
            if (size(transform%fields) == 0) then
                status = 78
                return
            end if
            if (.not. allocated(transform%names)) transform%names = transform%fields
            if (size(transform%names) /= size(transform%fields)) then
                status = 78
                return
            end if
            call append_transform(transforms, transform)
        end do
        status = 0
    end subroutine parse_transforms

    subroutine append_name(names, name)
        character(len=64), allocatable, intent(inout) :: names(:)
        character(len=*), intent(in) :: name
        character(len=64), allocatable :: expanded(:)
        integer :: n
        n = size(names)
        allocate (expanded(n + 1))
        expanded(1:n) = names
        expanded(n + 1) = name
        call move_alloc(expanded, names)
    end subroutine append_name

    subroutine append_transform(transforms, transform)
        type(flatten_t), allocatable, intent(inout) :: transforms(:)
        type(flatten_t), intent(in) :: transform
        type(flatten_t), allocatable :: expanded(:)
        integer :: n
        n = size(transforms)
        allocate (expanded(n + 1))
        expanded(1:n) = transforms
        expanded(n + 1) = transform
        call move_alloc(expanded, transforms)
    end subroutine append_transform

    integer function column_index(data, field) result(col)
        type(data_t), intent(in) :: data
        character(len=*), intent(in) :: field
        integer :: i
        col = 0
        if (.not. allocated(data%columns)) return
        do i = 1, size(data%columns)
            if (data%columns(i)%field /= trim(field)) cycle
            col = i
            return
        end do
    end function column_index

    integer function array_length(data, col, row) result(n)
        type(data_t), intent(in) :: data
        integer, intent(in) :: col, row
        integer :: index
        index = array_row(data%columns(col), row)
        n = 0
        if (allocated(data%columns(col)%arrays(index)%values)) &
            n = size(data%columns(col)%arrays(index)%values)
        if (allocated(data%columns(col)%arrays(index)%strings)) &
            n = size(data%columns(col)%arrays(index)%strings)
    end function array_length

    subroutine flatten_data(data, transform, status)
        type(data_t), intent(inout) :: data
        type(flatten_t), intent(in) :: transform
        integer, intent(out) :: status
        type(data_t) :: output
        integer, allocatable :: cols(:), dest(:), lengths(:)
        integer :: i, j, k, n, nc, row, outrow, col, index

        status = 0
        if (data%nrows == 0) return
        n = size(transform%fields)
        allocate (cols(n), dest(n), lengths(data%nrows))
        lengths = 0
        do i = 1, n
            cols(i) = column_index(data, transform%fields(i))
            if (cols(i) == 0) then
                status = 83
                return
            end if
            if (.not. data%columns(cols(i))%is_array) then
                status = 83
                return
            end if
            do row = 1, data%nrows
                lengths(row) = max(lengths(row), array_length(data, cols(i), row))
            end do
        end do
        output%nrows = sum(lengths)
        nc = size(data%columns)
        allocate (output%columns(nc + n))
        output%columns(1:nc) = data%columns
        do i = 1, n
            dest(i) = column_index(data, transform%names(i))
            if (dest(i) /= 0) cycle
            nc = nc + 1
            dest(i) = nc
        end do
        do j = 1, nc
            col = j
            do i = 1, n
                if (dest(i) == j) col = cols(i)
            end do
            output%columns(j)%field = data%columns(col)%field
            output%columns(j)%is_string = data%columns(col)%is_string
            output%columns(j)%is_array = data%columns(col)%is_array
            do i = 1, n
                if (dest(i) /= j) cycle
                output%columns(j)%field = trim(transform%names(i))
                output%columns(j)%is_array = .false.
            end do
            call allocate_column(output%columns(j), output%nrows)
            if (output%columns(j)%is_array) &
                output%columns(j)%arrays = data%columns(col)%arrays
            outrow = 0
            do row = 1, data%nrows
                index = array_row(data%columns(col), row)
                do k = 1, lengths(row)
                    outrow = outrow + 1
                    if (output%columns(j)%is_array) then
                        output%columns(j)%array_rows(outrow) = index
                    else if (data%columns(col)%is_array) then
                        if (k > array_length(data, col, row)) cycle
                        if (output%columns(j)%is_string) then
                            if (allocated(data%columns(col)%arrays(index)%strings)) then
                                output%columns(j)%string_values(outrow) = &
                                    data%columns(col)%arrays(index)%strings(k)
                                output%columns(j)%string_valid(outrow) = &
                                    data%columns(col)%arrays(index)%valid(k)
                            end if
                        else
                            output%columns(j)%values(outrow) = &
                                data%columns(col)%arrays(index)%values(k)
                        end if
                    else if (output%columns(j)%is_string) then
                        output%columns(j)%string_values(outrow) = &
                            data%columns(col)%string_values(row)
                        output%columns(j)%string_valid(outrow) = .true.
                        if (allocated(data%columns(col)%string_valid)) &
                            output%columns(j)%string_valid(outrow) = &
                            data%columns(col)%string_valid(row)
                    else
                        output%columns(j)%values(outrow) = data%columns(col)%values(row)
                    end if
                end do
            end do
        end do
        data%columns = output%columns(1:nc)
        data%nrows = output%nrows
    end subroutine flatten_data

    subroutine allocate_column(column, nrows)
        type(data_column_t), intent(inout) :: column
        integer, intent(in) :: nrows
        if (allocated(column%values)) deallocate (column%values)
        if (allocated(column%string_values)) deallocate (column%string_values)
        if (allocated(column%string_valid)) deallocate (column%string_valid)
        if (allocated(column%arrays)) deallocate (column%arrays)
        if (allocated(column%array_rows)) deallocate (column%array_rows)
        if (column%is_array) then
            allocate (column%array_rows(nrows))
        else if (column%is_string) then
            allocate (character(len=256) :: column%string_values(nrows))
            column%string_values = ''
            allocate (column%string_valid(nrows))
            column%string_valid = .false.
        else
            allocate (column%values(nrows))
            column%values = ieee_value(0.0_wp, ieee_quiet_nan)
        end if
    end subroutine allocate_column

    subroutine apply_transforms(data, transforms, status)
        type(data_t), intent(inout) :: data
        type(flatten_t), allocatable, intent(inout) :: transforms(:)
        integer, intent(out) :: status
        integer :: i
        status = 0
        if (.not. allocated(transforms)) return
        do i = 1, size(transforms)
            call flatten_data(data, transforms(i), status)
            if (status /= 0) return
        end do
        deallocate (transforms)
    end subroutine apply_transforms

    subroutine validate_channels(data, encoding, status)
        type(data_t), intent(in) :: data
        type(encoding_t), intent(in) :: encoding
        integer, intent(out) :: status
        integer :: col
        status = 0
        if (allocated(encoding%x%field)) then
            col = column_index(data, encoding%x%field)
            if (col > 0) then
                if (data%columns(col)%is_array) status = 84
            end if
        end if
        if (allocated(encoding%y%field)) then
            col = column_index(data, encoding%y%field)
            if (col > 0) then
                if (data%columns(col)%is_array) status = 84
            end if
        end if
    end subroutine validate_channels

    subroutine apply_spec_transforms(spec, status)
        type(spec_t), intent(inout) :: spec
        integer, intent(out) :: status
        integer :: i
        type(encoding_t) :: encoding
        call apply_transforms(spec%data, spec%transforms, status)
        if (status /= 0) return
        if (.not. allocated(spec%layers)) then
            call validate_channels(spec%data, spec%encoding, status)
            return
        end if
        do i = 1, size(spec%layers)
            if (.not. spec%layers(i)%has_data) then
                spec%layers(i)%data = spec%data
                spec%layers(i)%has_data = .true.
            end if
            call apply_transforms(spec%layers(i)%data, spec%layers(i)%transforms, status)
            if (status /= 0) return
            encoding = spec%encoding
            if (allocated(spec%layers(i)%encoding%x%field)) &
                encoding%x = spec%layers(i)%encoding%x
            if (allocated(spec%layers(i)%encoding%y%field)) &
                encoding%y = spec%layers(i)%encoding%y
            call validate_channels(spec%layers(i)%data, encoding, status)
            if (status /= 0) return
        end do
    end subroutine apply_spec_transforms
end module fortplot_spec_json_flatten
