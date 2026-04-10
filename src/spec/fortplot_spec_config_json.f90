module fortplot_spec_config_json
    !! JSON serialization for config_t and padding_t.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spec_config_types, only: config_t, padding_t
    implicit none

    private
    public :: serialize_config, serialize_padding

    character(len=*), parameter :: NL = new_line('a')
    character(len=*), parameter :: Q = '"'

contains

    function serialize_config(cfg) result(json)
        type(config_t), intent(in) :: cfg
        character(len=:), allocatable :: json

        logical :: need_comma

        json = '  "config": {'//NL
        need_comma = .false.

        if (cfg%background_set) then
            json = json//'    "background": '//Q// &
                trim(cfg%background)//Q
            need_comma = .true.
        end if

        if (cfg%category_color_count > 0) then
            if (need_comma) json = json//','//NL
            json = json//serialize_range(cfg)
            need_comma = .true.
        end if

        if (cfg%view%defined) then
            if (need_comma) json = json//','//NL
            json = json//serialize_view(cfg%view)
            need_comma = .true.
        end if

        if (cfg%axis%defined) then
            if (need_comma) json = json//','//NL
            json = json//serialize_axis(cfg%axis)
            need_comma = .true.
        end if

        if (cfg%mark%defined) then
            if (need_comma) json = json//','//NL
            json = json//serialize_marks(cfg%mark)
            need_comma = .true.
        end if

        if (cfg%title_config%defined) then
            if (need_comma) json = json//','//NL
            json = json//serialize_title(cfg%title_config)
            need_comma = .true.
        end if

        if (cfg%legend%defined) then
            if (need_comma) json = json//','//NL
            json = json//serialize_legend(cfg%legend)
        end if

        json = json//NL//'  }'
    end function serialize_config

    function serialize_padding(pad) result(json)
        type(padding_t), intent(in) :: pad
        character(len=:), allocatable :: json

        json = '  "padding": {'
        json = json//'"left": '//itoa(pad%left)
        json = json//', "right": '//itoa(pad%right)
        json = json//', "top": '//itoa(pad%top)
        json = json//', "bottom": '//itoa(pad%bottom)
        json = json//'}'
    end function serialize_padding

    function serialize_range(cfg) result(json)
        type(config_t), intent(in) :: cfg
        character(len=:), allocatable :: json
        integer :: i

        json = '    "range": {"category": ['
        do i = 1, cfg%category_color_count
            if (i > 1) json = json//', '
            json = json//Q//trim(cfg%category_colors(i))//Q
        end do
        json = json//']}'
    end function serialize_range

    function serialize_view(vw) result(json)
        use fortplot_spec_config_types, only: config_view_t
        type(config_view_t), intent(in) :: vw
        character(len=:), allocatable :: json
        logical :: nc

        json = '    "view": {'
        nc = .false.
        if (vw%stroke_set) then
            json = json//Q//'stroke'//Q//': '//Q// &
                trim(vw%stroke)//Q
            nc = .true.
        end if
        if (vw%stroke_width >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'strokeWidth'//Q//': '// &
                ftoa(vw%stroke_width)
        end if
        json = json//'}'
    end function serialize_view

    function serialize_axis(ax) result(json)
        use fortplot_spec_config_types, only: config_axis_t
        type(config_axis_t), intent(in) :: ax
        character(len=:), allocatable :: json
        logical :: nc

        json = '    "axis": {'
        nc = .false.

        if (ax%domain_set) then
            json = json//Q//'domain'//Q//': '//ltoa(ax%domain)
            nc = .true.
        end if
        if (ax%grid_set) then
            if (nc) json = json//', '
            json = json//Q//'grid'//Q//': '//ltoa(ax%grid)
            nc = .true.
        end if
        if (ax%grid_opacity >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'gridOpacity'//Q//': '// &
                ftoa(ax%grid_opacity)
            nc = .true.
        end if
        if (ax%grid_width >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'gridWidth'//Q//': '// &
                ftoa(ax%grid_width)
            nc = .true.
        end if
        if (ax%grid_color_set) then
            if (nc) json = json//', '
            json = json//Q//'gridColor'//Q//': '//Q// &
                trim(ax%grid_color)//Q
            nc = .true.
        end if
        if (ax%label_font_size >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'labelFontSize'//Q//': '// &
                ftoa(ax%label_font_size)
            nc = .true.
        end if
        if (ax%title_font_size >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'titleFontSize'//Q//': '// &
                ftoa(ax%title_font_size)
            nc = .true.
        end if
        if (ax%tick_size >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'tickSize'//Q//': '// &
                ftoa(ax%tick_size)
            nc = .true.
        end if
        if (ax%tick_width >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'tickWidth'//Q//': '// &
                ftoa(ax%tick_width)
            nc = .true.
        end if
        if (ax%tick_color_set) then
            if (nc) json = json//', '
            json = json//Q//'tickColor'//Q//': '//Q// &
                trim(ax%tick_color)//Q
            nc = .true.
        end if
        if (ax%label_font_set) then
            if (nc) json = json//', '
            json = json//Q//'labelFont'//Q//': '//Q// &
                trim(ax%label_font)//Q
            nc = .true.
        end if
        if (ax%title_font_set) then
            if (nc) json = json//', '
            json = json//Q//'titleFont'//Q//': '//Q// &
                trim(ax%title_font)//Q
            nc = .true.
        end if
        if (ax%title_font_weight_set) then
            if (nc) json = json//', '
            json = json//Q//'titleFontWeight'//Q//': '//Q// &
                trim(ax%title_font_weight)//Q
        end if
        json = json//'}'
    end function serialize_axis

    function serialize_marks(mk) result(json)
        use fortplot_spec_config_types, only: config_mark_defaults_t
        type(config_mark_defaults_t), intent(in) :: mk
        character(len=:), allocatable :: json

        json = ''
        if (mk%line_stroke_width >= 0.0_wp) then
            json = json//'    "line": {"strokeWidth": '// &
                ftoa(mk%line_stroke_width)//'}'
        end if
        if (mk%point_filled_set .or. mk%point_size >= 0.0_wp) then
            if (len(json) > 0) json = json//','//NL
            json = json//'    "point": {'
            if (mk%point_filled_set) then
                json = json//Q//'filled'//Q//': '// &
                    ltoa(mk%point_filled)
                if (mk%point_size >= 0.0_wp) json = json//', '
            end if
            if (mk%point_size >= 0.0_wp) then
                json = json//Q//'size'//Q//': '// &
                    ftoa(mk%point_size)
            end if
            json = json//'}'
        end if
        if (mk%bar_fill_set) then
            if (len(json) > 0) json = json//','//NL
            json = json//'    "bar": {"fill": '//Q// &
                trim(mk%bar_fill)//Q//'}'
        end if
    end function serialize_marks

    function serialize_title(tc) result(json)
        use fortplot_spec_config_types, only: config_title_t
        type(config_title_t), intent(in) :: tc
        character(len=:), allocatable :: json
        logical :: nc

        json = '    "title": {'
        nc = .false.

        if (tc%anchor_set) then
            json = json//Q//'anchor'//Q//': '//Q// &
                trim(tc%anchor)//Q
            nc = .true.
        end if
        if (tc%color_set) then
            if (nc) json = json//', '
            json = json//Q//'color'//Q//': '//Q// &
                trim(tc%color)//Q
            nc = .true.
        end if
        if (tc%font_set) then
            if (nc) json = json//', '
            json = json//Q//'font'//Q//': '//Q// &
                trim(tc%font)//Q
            nc = .true.
        end if
        if (tc%font_size >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'fontSize'//Q//': '// &
                ftoa(tc%font_size)
            nc = .true.
        end if
        if (tc%font_weight_set) then
            if (nc) json = json//', '
            json = json//Q//'fontWeight'//Q//': '//Q// &
                trim(tc%font_weight)//Q
            nc = .true.
        end if
        if (tc%offset >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'offset'//Q//': '//ftoa(tc%offset)
        end if
        json = json//'}'
    end function serialize_title

    function serialize_legend(lg) result(json)
        use fortplot_spec_config_types, only: config_legend_t
        type(config_legend_t), intent(in) :: lg
        character(len=:), allocatable :: json
        logical :: nc

        json = '    "legend": {'
        nc = .false.

        if (lg%orient_set) then
            json = json//Q//'orient'//Q//': '//Q// &
                trim(lg%orient)//Q
            nc = .true.
        end if
        if (lg%label_font_size >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'labelFontSize'//Q//': '// &
                ftoa(lg%label_font_size)
            nc = .true.
        end if
        if (lg%symbol_stroke_width >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'symbolStrokeWidth'//Q//': '// &
                ftoa(lg%symbol_stroke_width)
            nc = .true.
        end if
        if (lg%fill_color_set) then
            if (nc) json = json//', '
            json = json//Q//'fillColor'//Q//': '//Q// &
                trim(lg%fill_color)//Q
            nc = .true.
        end if
        if (lg%stroke_color_set) then
            if (nc) json = json//', '
            json = json//Q//'strokeColor'//Q//': '//Q// &
                trim(lg%stroke_color)//Q
            nc = .true.
        end if
        if (lg%corner_radius >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'cornerRadius'//Q//': '// &
                ftoa(lg%corner_radius)
            nc = .true.
        end if
        if (lg%padding >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'padding'//Q//': '// &
                ftoa(lg%padding)
            nc = .true.
        end if
        if (lg%frame_alpha >= 0.0_wp) then
            if (nc) json = json//', '
            json = json//Q//'frameAlpha'//Q//': '// &
                ftoa(lg%frame_alpha)
            nc = .true.
        end if
        json = json//'}'
    end function serialize_legend

    pure function ftoa(val) result(s)
        real(wp), intent(in) :: val
        character(len=:), allocatable :: s
        character(len=20) :: buf
        write (buf, '(g0)') val
        s = trim(adjustl(buf))
    end function ftoa

    pure function itoa(val) result(s)
        integer, intent(in) :: val
        character(len=:), allocatable :: s
        character(len=12) :: buf
        write (buf, '(i0)') val
        s = trim(adjustl(buf))
    end function itoa

    pure function ltoa(val) result(s)
        logical, intent(in) :: val
        character(len=:), allocatable :: s
        if (val) then
            s = 'true'
        else
            s = 'false'
        end if
    end function ltoa

end module fortplot_spec_config_json
