module fortplot_spec_config_apply
    !! Applies config_t values to figure_state_t for rendering.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spec_config_types, only: config_t, padding_t
    use fortplot_spec_config_defaults, only: mpl_default_config, &
        vegalite_default_config
    use fortplot_spec_types, only: spec_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_colors, only: parse_color
    use fortplot_text_fonts, only: set_preferred_font
    implicit none

    private
    public :: apply_config_to_state, apply_padding_to_margins
    public :: apply_style_defaults

contains

    subroutine apply_style_defaults(style_name, spec, dpi, force)
        !! Fill spec%config from built-in style.
        !! When force=.true., overrides any JSON-provided config.
        !! When force=.false. (default), only fills when absent.
        character(len=*), intent(in) :: style_name
        type(spec_t), intent(inout) :: spec
        real(wp), intent(in) :: dpi
        logical, intent(in), optional :: force

        logical :: do_force

        do_force = .false.
        if (present(force)) do_force = force

        if (spec%config%defined .and. .not. do_force) return

        select case (trim(style_name))
        case ('mpl', 'matplotlib')
            spec%config = mpl_default_config(dpi)
        case ('vegalite', 'vega-lite', 'vega')
            spec%config = vegalite_default_config(dpi)
        end select
    end subroutine apply_style_defaults

    subroutine apply_config_to_state(cfg, state)
        !! Translate config_t into figure_state_t fields.
        type(config_t), intent(in) :: cfg
        type(figure_state_t), intent(inout) :: state

        if (.not. cfg%defined) return

        call apply_color_palette(cfg, state)
        call apply_mark_defaults(cfg, state)
        call apply_axis_config(cfg, state)
        call apply_title_config(cfg, state)
        call apply_font_preference(cfg)
    end subroutine apply_config_to_state

    subroutine apply_color_palette(cfg, state)
        type(config_t), intent(in) :: cfg
        type(figure_state_t), intent(inout) :: state

        real(wp) :: rgb(3)
        logical :: ok
        integer :: i, n

        n = cfg%category_color_count
        if (n <= 0) return

        do i = 1, min(n, size(state%colors, 2))
            call parse_color(trim(cfg%category_colors(i)), rgb, ok)
            if (ok) state%colors(:, i) = rgb
        end do
    end subroutine apply_color_palette

    subroutine apply_mark_defaults(cfg, state)
        type(config_t), intent(in) :: cfg
        type(figure_state_t), intent(inout) :: state

        if (.not. cfg%mark%defined) return

        if (cfg%mark%line_stroke_width >= 0.0_wp) then
            state%current_line_width = &
                cfg%mark%line_stroke_width
        end if
    end subroutine apply_mark_defaults

    subroutine apply_axis_config(cfg, state)
        type(config_t), intent(in) :: cfg
        type(figure_state_t), intent(inout) :: state

        if (.not. cfg%axis%defined) return

        if (cfg%axis%grid_set) then
            state%grid_enabled = cfg%axis%grid
        end if
        if (cfg%axis%grid_opacity >= 0.0_wp) then
            state%grid_alpha = cfg%axis%grid_opacity
        end if
        if (cfg%axis%grid_color_set) then
            state%grid_color = cfg%axis%grid_color
        end if
        if (cfg%axis%label_font_size >= 0.0_wp) then
            state%label_font_size = cfg%axis%label_font_size
            state%tick_font_size = cfg%axis%label_font_size
        end if
        if (cfg%axis%title_font_size >= 0.0_wp) then
            state%label_font_size = cfg%axis%title_font_size
        end if
    end subroutine apply_axis_config

    subroutine apply_title_config(cfg, state)
        type(config_t), intent(in) :: cfg
        type(figure_state_t), intent(inout) :: state

        if (.not. cfg%title_config%defined) return

        if (cfg%title_config%font_size >= 0.0_wp) then
            state%title_font_size = cfg%title_config%font_size
        end if
    end subroutine apply_title_config

    subroutine apply_font_preference(cfg)
        !! Set the preferred font based on config axis label font.
        !! Maps common CSS font families to system font names.
        type(config_t), intent(in) :: cfg

        character(len=40) :: font_name

        if (.not. cfg%axis%defined) return
        if (.not. cfg%axis%label_font_set) return

        font_name = cfg%axis%label_font
        ! Map CSS font family strings to font discovery names
        if (index(font_name, 'DejaVu') > 0) then
            call set_preferred_font('DejaVu Sans')
        else if (index(font_name, 'Arial') > 0) then
            call set_preferred_font('Arial')
        else if (index(font_name, 'Helvetica') > 0) then
            call set_preferred_font('Helvetica')
        else if (index(font_name, 'Liberation') > 0) then
            call set_preferred_font('Liberation Sans')
        end if
    end subroutine apply_font_preference

    subroutine apply_padding_to_margins(pad, state, autosize_type)
        !! Convert Vega-Lite padding to figure_state_t margins.
        !!
        !! When autosize is "none" with "contains": "padding", the spec
        !! width/height is the data rectangle and padding is added around
        !! it. We expand the canvas to total = data + padding and compute
        !! margins as fractions of that total.
        type(padding_t), intent(in) :: pad
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in), optional :: autosize_type

        integer :: pl, pr, pt, pb
        real(wp) :: tw, th

        if (.not. pad%defined) return

        pl = max(pad%left, 0)
        pr = max(pad%right, 0)
        pt = max(pad%top, 0)
        pb = max(pad%bottom, 0)

        ! Expand canvas: total = data_rect + padding
        tw = real(state%width + pl + pr, wp)
        th = real(state%height + pt + pb, wp)

        if (tw <= 0.0_wp .or. th <= 0.0_wp) return

        state%width = nint(tw)
        state%height = nint(th)
        state%margin_left = real(pl, wp) / tw
        state%margin_right = real(pr, wp) / tw
        state%margin_top = real(pt, wp) / th
        state%margin_bottom = real(pb, wp) / th

        ! Reinitialize backend with new dimensions
        call reinit_backend_dimensions(state)
    end subroutine apply_padding_to_margins

    subroutine reinit_backend_dimensions(state)
        use fortplot_utils, only: initialize_backend
        type(figure_state_t), intent(inout) :: state

        call initialize_backend(state%backend, &
            trim(state%backend_name), &
            state%width, state%height, state%dpi)
    end subroutine reinit_backend_dimensions

end module fortplot_spec_config_apply
