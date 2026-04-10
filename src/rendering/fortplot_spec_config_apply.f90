module fortplot_spec_config_apply
    !! Applies config_t values to figure_state_t for rendering.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_spec_config_types, only: config_t, padding_t
    use fortplot_spec_config_defaults, only: mpl_default_config, &
        vegalite_default_config
    use fortplot_spec_types, only: spec_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_colors, only: parse_color
    implicit none

    private
    public :: apply_config_to_state, apply_padding_to_margins
    public :: apply_style_defaults

contains

    subroutine apply_style_defaults(style_name, spec, dpi)
        !! Fill spec%config from built-in style if not already set.
        character(len=*), intent(in) :: style_name
        type(spec_t), intent(inout) :: spec
        real(wp), intent(in) :: dpi

        if (spec%config%defined) return

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
    end subroutine apply_axis_config

    subroutine apply_padding_to_margins(pad, state)
        !! Convert pixel padding to fractional margins.
        type(padding_t), intent(in) :: pad
        type(figure_state_t), intent(inout) :: state

        real(wp) :: w, h

        if (.not. pad%defined) return

        w = real(state%width, wp)
        h = real(state%height, wp)

        if (w <= 0.0_wp .or. h <= 0.0_wp) return

        if (pad%left >= 0) then
            state%margin_left = real(pad%left, wp) / w
        end if
        if (pad%right >= 0) then
            state%margin_right = real(pad%right, wp) / w
        end if
        if (pad%top >= 0) then
            state%margin_top = real(pad%top, wp) / h
        end if
        if (pad%bottom >= 0) then
            state%margin_bottom = real(pad%bottom, wp) / h
        end if
    end subroutine apply_padding_to_margins

end module fortplot_spec_config_apply
