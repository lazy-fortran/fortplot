module fortplot_spec_builder
    !! Builder API for constructing spec_t from data arrays
    !!
    !! Provides high-level functions (vl_line, vl_point, vl_bar, etc.)
    !! that build Vega-Lite-shaped spec_t from Fortran arrays.
    !! Also provides spec_savefig for rendering or JSON export.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spec_types, only: spec_t, mark_t, encoding_t, &
                              channel_t, data_t, data_column_t, scale_t, axis_t, layer_t
    use fortplot_spec_json, only: spec_to_json_file
    use fortplot_plot_bars, only: bar_impl
    implicit none

    private
    public :: vl_line, vl_point, vl_bar, vl_area
    public :: vl_layer_add, vl_channel
    public :: spec_savefig, spec_to_figure

contains

    function vl_line(x, y, title, xlabel, ylabel, width, height, &
                     interpolate) result(spec)
        !! Build a line mark spec from x/y arrays
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: title
        character(len=*), intent(in), optional :: xlabel, ylabel
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: interpolate
        type(spec_t) :: spec

        call init_xy_spec(spec, 'line', x, y, title, xlabel, &
                          ylabel, width, height)
        if (present(interpolate)) then
            spec%mark%interpolate = interpolate
        end if
    end function vl_line

    function vl_point(x, y, title, xlabel, ylabel, &
                      width, height) result(spec)
        !! Build a point (scatter) mark spec from x/y arrays
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: title
        character(len=*), intent(in), optional :: xlabel, ylabel
        integer, intent(in), optional :: width, height
        type(spec_t) :: spec

        call init_xy_spec(spec, 'point', x, y, title, xlabel, &
                          ylabel, width, height)
    end function vl_point

    function vl_bar(x, y, title, xlabel, ylabel, &
                    width, height) result(spec)
        !! Build a bar mark spec from x/y arrays
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: title
        character(len=*), intent(in), optional :: xlabel, ylabel
        integer, intent(in), optional :: width, height
        type(spec_t) :: spec

        call init_xy_spec(spec, 'bar', x, y, title, xlabel, &
                          ylabel, width, height)
        spec%encoding%x%type = 'ordinal'
    end function vl_bar

    function vl_area(x, y, title, xlabel, ylabel, &
                     width, height) result(spec)
        !! Build an area mark spec from x/y arrays
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: title
        character(len=*), intent(in), optional :: xlabel, ylabel
        integer, intent(in), optional :: width, height
        type(spec_t) :: spec

        call init_xy_spec(spec, 'area', x, y, title, xlabel, &
                          ylabel, width, height)
    end function vl_area

    subroutine init_xy_spec(spec, mark_type, x, y, title, &
                            xlabel, ylabel, width, height)
        !! Common initialization for x/y mark specs
        type(spec_t), intent(out) :: spec
        character(len=*), intent(in) :: mark_type
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: title
        character(len=*), intent(in), optional :: xlabel, ylabel
        integer, intent(in), optional :: width, height
        integer :: n

        n = min(size(x), size(y))

        spec%mark%type = mark_type

        if (present(title)) spec%title = title
        if (present(width)) spec%width = width
        if (present(height)) spec%height = height

        spec%encoding%x = vl_channel('x', 'quantitative')
        spec%encoding%y = vl_channel('y', 'quantitative')
        if (present(xlabel)) then
            spec%encoding%x%axis%title = xlabel
            spec%encoding%x%axis%title_set = .true.
        end if
        if (present(ylabel)) then
            spec%encoding%y%axis%title = ylabel
            spec%encoding%y%axis%title_set = .true.
        end if

        allocate (spec%data%columns(2))
        spec%data%columns(1)%field = 'x'
        spec%data%columns(1)%values = x(1:n)
        spec%data%columns(2)%field = 'y'
        spec%data%columns(2)%values = y(1:n)
        spec%data%nrows = n
    end subroutine init_xy_spec

    function vl_channel(field, type) result(ch)
        !! Create a channel with field and type
        character(len=*), intent(in) :: field, type
        type(channel_t) :: ch

        ch%field = field
        ch%type = type
        ch%defined = .true.
    end function vl_channel

    subroutine vl_layer_add(spec, mark_type, x, y, &
                            label, interpolate)
        !! Add a layer to a spec (converts to layered if needed)
        type(spec_t), intent(inout) :: spec
        character(len=*), intent(in) :: mark_type
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: interpolate
        type(layer_t), allocatable :: new_layers(:)
        type(layer_t) :: new_layer
        integer :: n, old_count

        n = min(size(x), size(y))

        if (.not. spec%is_layered) then
            call promote_to_layered(spec)
        end if

        new_layer%mark%type = mark_type
        new_layer%encoding%x = vl_channel('x', 'quantitative')
        new_layer%encoding%y = vl_channel('y', 'quantitative')
        if (present(label)) then
            new_layer%encoding%color%value = &
                '"'//label//'"'
            new_layer%encoding%color%defined = .true.
        end if
        if (present(interpolate)) then
            new_layer%mark%interpolate = interpolate
        end if

        allocate (new_layer%data%columns(2))
        new_layer%data%columns(1)%field = 'x'
        new_layer%data%columns(1)%values = x(1:n)
        new_layer%data%columns(2)%field = 'y'
        new_layer%data%columns(2)%values = y(1:n)
        new_layer%data%nrows = n
        new_layer%has_data = .true.

        old_count = spec%layer_count
        allocate (new_layers(old_count + 1))
        if (old_count > 0) then
            new_layers(1:old_count) = spec%layers(1:old_count)
        end if
        new_layers(old_count + 1) = new_layer
        call move_alloc(new_layers, spec%layers)
        spec%layer_count = old_count + 1
    end subroutine vl_layer_add

    subroutine promote_to_layered(spec)
        !! Convert a single-view spec to layered, moving its
        !! mark/encoding into the first layer
        type(spec_t), intent(inout) :: spec

        spec%is_layered = .true.
        allocate (spec%layers(1))
        spec%layers(1)%mark = spec%mark
        spec%layers(1)%encoding = spec%encoding
        spec%layers(1)%has_data = .false.
        spec%layer_count = 1
    end subroutine promote_to_layered

    subroutine spec_savefig(spec, filename, status)
        !! Save spec as JSON (.vl.json/.json) or render to image
        type(spec_t), intent(in) :: spec
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: status
        integer :: ios, dot_pos
        character(len=20) :: ext

        ios = 0
        dot_pos = index(filename, '.vl.json')
        if (dot_pos > 0) then
            call spec_to_json_file(spec, filename, ios)
            if (present(status)) status = ios
            return
        end if

        dot_pos = index(filename, '.json')
        if (dot_pos > 0 .and. &
            dot_pos + 4 == len_trim(filename)) then
            call spec_to_json_file(spec, filename, ios)
            if (present(status)) status = ios
            return
        end if

        call render_spec_to_file(spec, filename, ios)
        if (present(status)) status = ios
    end subroutine spec_savefig

    subroutine spec_to_figure(spec, fig)
        !! Convert spec_t to figure_t for rendering via existing
        !! backends. Phase 1 bridge: maps spec fields to figure_t
        !! state so existing rendering pipeline can consume it.
        use fortplot_figure_core, only: figure_t
        type(spec_t), intent(in) :: spec
        type(figure_t), intent(out) :: fig
        real(wp), allocatable :: x(:), y(:)
        integer :: i

        call fig%initialize(spec%width, spec%height)

        if (allocated(spec%title)) then
            call fig%set_title(spec%title)
        end if

        if (spec%is_layered .and. spec%layer_count > 0) then
            do i = 1, spec%layer_count
                call add_layer_to_figure(fig, spec%layers(i), &
                                         spec%data)
            end do
        else
            call extract_xy(spec%data, spec%encoding, x, y)
            if (allocated(x) .and. allocated(y)) then
                call add_mark_to_figure(fig, spec%mark, x, y, &
                                        spec%encoding)
            end if
        end if

        call apply_encoding_axes(fig, spec%encoding)
    end subroutine spec_to_figure

    subroutine render_spec_to_file(spec, filename, status)
        !! Render spec via figure_t and existing backend pipeline
        use fortplot_figure_core, only: figure_t
        type(spec_t), intent(in) :: spec
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        type(figure_t) :: fig

        call spec_to_figure(spec, fig)
        call fig%savefig_with_status(filename, status)
    end subroutine render_spec_to_file

    subroutine extract_xy(data, enc, x, y)
        !! Extract x and y arrays from data based on encoding fields
        type(data_t), intent(in) :: data
        type(encoding_t), intent(in) :: enc
        real(wp), allocatable, intent(out) :: x(:), y(:)
        integer :: j
        character(len=:), allocatable :: xfield, yfield

        if (.not. allocated(data%columns) .or. &
            data%nrows == 0) return

        xfield = 'x'
        yfield = 'y'
        if (enc%x%defined .and. allocated(enc%x%field)) then
            xfield = enc%x%field
        end if
        if (enc%y%defined .and. allocated(enc%y%field)) then
            yfield = enc%y%field
        end if

        do j = 1, size(data%columns)
            if (data%columns(j)%field == xfield) then
                x = data%columns(j)%values
            end if
            if (data%columns(j)%field == yfield) then
                y = data%columns(j)%values
            end if
        end do
    end subroutine extract_xy

    subroutine add_mark_to_figure(fig, m, x, y, enc)
        !! Map a mark + data to a figure_t plot call
        use fortplot_figure_core, only: figure_t
        type(figure_t), intent(inout) :: fig
        type(mark_t), intent(in) :: m
        real(wp), intent(in) :: x(:), y(:)
        type(encoding_t), intent(in) :: enc
        real(wp), allocatable :: zeros(:)
        character(len=:), allocatable :: label
        integer :: vlen

        ! Extract label from color encoding value (strip JSON quotes)
        if (enc%color%defined .and. allocated(enc%color%value)) then
            vlen = len(enc%color%value)
            if (vlen >= 2 .and. &
                enc%color%value(1:1) == '"' .and. &
                enc%color%value(vlen:vlen) == '"') then
                label = enc%color%value(2:vlen - 1)
            else
                label = enc%color%value
            end if
        end if

        select case (m%type)
        case ('line')
            if (allocated(label)) then
                call fig%add_plot(x, y, label=label)
            else
                call fig%add_plot(x, y)
            end if
        case ('point')
            if (allocated(label)) then
                call fig%scatter(x, y, label=label)
            else
                call fig%scatter(x, y)
            end if
        case ('bar')
            call bar_impl(fig, x, y)
        case ('area')
            if (size(x) >= 2) then
                allocate (zeros(size(y)))
                zeros = 0.0_wp
                call fig%add_fill_between(x, y1=y, y2=zeros)
            end if
        case default
            if (allocated(label)) then
                call fig%add_plot(x, y, label=label)
            else
                call fig%add_plot(x, y)
            end if
        end select
    end subroutine add_mark_to_figure

    subroutine add_layer_to_figure(fig, lay, shared_data)
        !! Add a single layer to figure_t
        use fortplot_figure_core, only: figure_t
        type(figure_t), intent(inout) :: fig
        type(layer_t), intent(in) :: lay
        type(data_t), intent(in) :: shared_data
        real(wp), allocatable :: x(:), y(:)

        if (lay%has_data) then
            call extract_xy(lay%data, lay%encoding, x, y)
        else
            call extract_xy(shared_data, lay%encoding, x, y)
        end if

        if (allocated(x) .and. allocated(y)) then
            call add_mark_to_figure(fig, lay%mark, x, y, &
                                    lay%encoding)
        end if
    end subroutine add_layer_to_figure

    subroutine apply_encoding_axes(fig, enc)
        !! Apply axis titles from encoding to figure
        use fortplot_figure_core, only: figure_t
        type(figure_t), intent(inout) :: fig
        type(encoding_t), intent(in) :: enc

        if (enc%x%defined .and. enc%x%axis%title_set) then
            call fig%set_xlabel(enc%x%axis%title)
        end if
        if (enc%y%defined .and. enc%y%axis%title_set) then
            call fig%set_ylabel(enc%y%axis%title)
        end if
    end subroutine apply_encoding_axes

end module fortplot_spec_builder
