module fortplot_spec_json_channels
    !! JSON channel, scale, domain, and axis parsing.
    !!
    !! Handles encoding channels (x, y, color, etc.), scale configuration,
    !! axis properties, and supported domain parsing.

    use fortplot_spec_types, only: channel_t, scale_t, axis_t
    use fortplot_spec_json_reader, only: skip_ws, expect_char, &
                                          read_string, read_real, &
                                          read_bool, read_literal, skip_value

    implicit none
    private

    public :: parse_channel
    public :: parse_scale
    public :: parse_axis
    public :: parse_supported_domain
    public :: parse_domain
    public :: parse_real_array

contains

    subroutine parse_channel(json, pos, ch, status)
        !! Parse a single encoding channel object
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(channel_t), intent(out) :: ch
        integer, intent(out) :: status
        character(len=:), allocatable :: key

        status = 0
        ch%defined = .true.

        if (.not. expect_char(json, pos, '{')) then
            status = 30
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
                status = 31
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('field')
                call read_string(json, pos, ch%field, status)
            case ('type')
                call read_string(json, pos, ch%type, status)
            case ('value')
                call read_literal(json, pos, ch%value, status)
            case ('scale')
                call parse_scale(json, pos, ch%scale, status)
            case ('axis')
                call parse_axis(json, pos, ch%axis, status)
            case default
                call skip_value(json, pos)
            end select

            if (status /= 0) return
        end do
    end subroutine parse_channel

    subroutine parse_scale(json, pos, sc, status)
        !! Parse scale object
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(scale_t), intent(out) :: sc
        integer, intent(out) :: status
        character(len=:), allocatable :: key

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 40
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
                status = 41
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('type')
                call read_string(json, pos, sc%type, status)
            case ('domain')
                call parse_supported_domain(json, pos, sc, status)
            case ('zero')
                call read_bool(json, pos, sc%zero, status)
            case ('exponent')
                call read_real(json, pos, sc%exponent, status)
            case default
                call skip_value(json, pos)
            end select

            if (status /= 0) return
        end do
    end subroutine parse_scale

    subroutine parse_supported_domain(json, pos, sc, status)
        !! Parse only numeric domains and skip categorical domains.
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(scale_t), intent(inout) :: sc
        integer, intent(out) :: status

        integer :: probe

        status = 0
        probe = pos
        call skip_ws(json, probe)
        if (.not. expect_char(json, probe, '[')) then
            call skip_value(json, pos)
            return
        end if
        call skip_ws(json, probe)
        if (probe > len(json)) then
            status = 42
            return
        end if

        select case (json(probe:probe))
        case ('-', '0':'9')
            call parse_domain(json, pos, sc, status)
        case default
            call skip_value(json, pos)
        end select
    end subroutine parse_supported_domain

    subroutine parse_domain(json, pos, sc, status)
        !! Parse domain array [min, max]
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(scale_t), intent(inout) :: sc
        integer, intent(out) :: status

        status = 0
        if (.not. expect_char(json, pos, '[')) then
            status = 42
            return
        end if
        call skip_ws(json, pos)
        call read_real(json, pos, sc%domain_min, status)
        if (status /= 0) return
        call skip_ws(json, pos)
        if (.not. expect_char(json, pos, ',')) then
            status = 43
            return
        end if
        call skip_ws(json, pos)
        call read_real(json, pos, sc%domain_max, status)
        if (status /= 0) return
        call skip_ws(json, pos)
        if (.not. expect_char(json, pos, ']')) then
            status = 44
            return
        end if
        sc%domain_set = .true.
    end subroutine parse_domain

    subroutine parse_axis(json, pos, ax, status)
        !! Parse axis object
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        type(axis_t), intent(out) :: ax
        integer, intent(out) :: status
        character(len=:), allocatable :: key

        status = 0
        if (.not. expect_char(json, pos, '{')) then
            status = 50
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
                status = 51
                return
            end if
            call skip_ws(json, pos)

            select case (key)
            case ('title')
                call read_string(json, pos, ax%title, status)
                ax%title_set = .true.
            case ('grid')
                call read_bool(json, pos, ax%grid, status)
            case ('labelAngle')
                call read_real(json, pos, ax%label_angle, status)
            case ('values')
                call parse_real_array(json, pos, ax%tick_values, &
                                     status)
            case ('format')
                call read_string(json, pos, ax%format, status)
            case ('gridOpacity')
                call read_real(json, pos, ax%grid_opacity, status)
            case ('gridDash')
                call skip_value(json, pos)
            case default
                call skip_value(json, pos)
            end select

            if (status /= 0) return
        end do
    end subroutine parse_axis

    subroutine parse_real_array(json, pos, values, status)
        character(len=*), intent(in) :: json
        integer, intent(inout) :: pos
        real(wp), allocatable, intent(out) :: values(:)
        integer, intent(out) :: status
        integer :: count, save_pos, i
        real(wp) :: tmp

        status = 0
        if (.not. expect_char(json, pos, '[')) then
            status = 97
            return
        end if

        call skip_ws(json, pos)
        if (json(pos:pos) == ']') then
            pos = pos + 1
            allocate (values(0))
            return
        end if

        save_pos = pos
        count = 0
        do
            if (count > 0) then
                if (.not. expect_char(json, pos, ',')) then
                    status = 98
                    return
                end if
                call skip_ws(json, pos)
            end if
            call read_real(json, pos, tmp, status)
            if (status /= 0) return
            count = count + 1
            call skip_ws(json, pos)
            if (json(pos:pos) == ']') exit
        end do

        allocate (values(count))
        pos = save_pos
        do i = 1, count
            if (i > 1) then
                if (.not. expect_char(json, pos, ',')) then
                    status = 99
                    return
                end if
                call skip_ws(json, pos)
            end if
            call read_real(json, pos, values(i), status)
            if (status /= 0) return
            call skip_ws(json, pos)
        end do
        if (.not. expect_char(json, pos, ']')) then
            status = 102
        end if
    end subroutine parse_real_array

end module fortplot_spec_json_channels
