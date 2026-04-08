module fortplot_spec_builder
    !! Builder API for constructing spec_t from data arrays.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spec_json, only: spec_to_json_file
    use fortplot_spec_rendering, only: render_spec_to_file, spec_target_is_json
    use fortplot_spec_types, only: spec_t, channel_t, layer_t
    implicit none

    private
    public :: vl_line, vl_point, vl_bar, vl_area
    public :: vl_layer_add, vl_channel
    public :: spec_savefig

contains

    function vl_line(x, y, title, xlabel, ylabel, width, height, &
                     interpolate) result(spec)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: title
        character(len=*), intent(in), optional :: xlabel, ylabel
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: interpolate
        type(spec_t) :: spec

        call init_xy_spec(spec, 'line', x, y, title, xlabel, ylabel, width, height)
        if (present(interpolate)) spec%mark%interpolate = interpolate
    end function vl_line

    function vl_point(x, y, title, xlabel, ylabel, width, height) result(spec)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: title
        character(len=*), intent(in), optional :: xlabel, ylabel
        integer, intent(in), optional :: width, height
        type(spec_t) :: spec

        call init_xy_spec(spec, 'point', x, y, title, xlabel, ylabel, width, height)
    end function vl_point

    function vl_bar(x, y, title, xlabel, ylabel, width, height) result(spec)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: title
        character(len=*), intent(in), optional :: xlabel, ylabel
        integer, intent(in), optional :: width, height
        type(spec_t) :: spec

        call init_xy_spec(spec, 'bar', x, y, title, xlabel, ylabel, width, height)
        spec%encoding%x%type = 'ordinal'
    end function vl_bar

    function vl_area(x, y, title, xlabel, ylabel, width, height) result(spec)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: title
        character(len=*), intent(in), optional :: xlabel, ylabel
        integer, intent(in), optional :: width, height
        type(spec_t) :: spec

        call init_xy_spec(spec, 'area', x, y, title, xlabel, ylabel, width, height)
    end function vl_area

    subroutine init_xy_spec(spec, mark_type, x, y, title, xlabel, ylabel, width, height)
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
        character(len=*), intent(in) :: field, type
        type(channel_t) :: ch

        ch%field = field
        ch%type = type
        ch%defined = .true.
    end function vl_channel

    subroutine vl_layer_add(spec, mark_type, x, y, label, interpolate)
        type(spec_t), intent(inout) :: spec
        character(len=*), intent(in) :: mark_type
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: interpolate
        type(layer_t), allocatable :: new_layers(:)
        type(layer_t) :: new_layer
        integer :: n
        integer :: old_count

        n = min(size(x), size(y))
        if (.not. spec%is_layered) call promote_to_layered(spec)

        new_layer%mark%type = mark_type
        new_layer%encoding%x = vl_channel('x', 'quantitative')
        new_layer%encoding%y = vl_channel('y', 'quantitative')
        if (present(label)) then
            new_layer%encoding%color%value = '"'//label//'"'
            new_layer%encoding%color%defined = .true.
        end if
        if (present(interpolate)) new_layer%mark%interpolate = interpolate

        allocate (new_layer%data%columns(2))
        new_layer%data%columns(1)%field = 'x'
        new_layer%data%columns(1)%values = x(1:n)
        new_layer%data%columns(2)%field = 'y'
        new_layer%data%columns(2)%values = y(1:n)
        new_layer%data%nrows = n
        new_layer%has_data = .true.

        old_count = spec%layer_count
        allocate (new_layers(old_count + 1))
        if (old_count > 0) new_layers(1:old_count) = spec%layers(1:old_count)
        new_layers(old_count + 1) = new_layer
        call move_alloc(new_layers, spec%layers)
        spec%layer_count = old_count + 1
    end subroutine vl_layer_add

    subroutine promote_to_layered(spec)
        type(spec_t), intent(inout) :: spec

        spec%is_layered = .true.
        allocate (spec%layers(1))
        spec%layers(1)%mark = spec%mark
        spec%layers(1)%encoding = spec%encoding
        spec%layers(1)%has_data = .false.
        spec%layer_count = 1
    end subroutine promote_to_layered

    subroutine spec_savefig(spec, filename, status)
        type(spec_t), intent(in) :: spec
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: status
        integer :: ios

        ios = 0
        if (spec_target_is_json(filename)) then
            call spec_to_json_file(spec, filename, ios)
        else
            call render_spec_to_file(spec, filename, ios)
        end if
        if (present(status)) status = ios
    end subroutine spec_savefig

end module fortplot_spec_builder
