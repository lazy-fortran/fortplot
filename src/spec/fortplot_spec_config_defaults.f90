module fortplot_spec_config_defaults
    !! Built-in style presets for MPL and Vega-Lite rendering modes.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: TAB10_HEX, TAB10_COUNT
    use fortplot_spec_config_types, only: config_t, config_axis_t, &
        config_view_t, config_mark_defaults_t, config_title_t, &
        config_legend_t, padding_t
    implicit none

    private
    public :: mpl_default_config, vegalite_default_config

contains

    pure function pts_to_px(points, dpi) result(px)
        real(wp), intent(in) :: points, dpi
        real(wp) :: px
        px = points * dpi / 72.0_wp
    end function pts_to_px

    pure function mpl_default_config(dpi) result(cfg)
        !! Return config_t matching matplotlib visual defaults.
        real(wp), intent(in) :: dpi
        type(config_t) :: cfg

        cfg%defined = .true.
        cfg%background = '#ffffff'
        cfg%background_set = .true.

        cfg%category_color_count = TAB10_COUNT
        cfg%category_colors(1:TAB10_COUNT) = TAB10_HEX

        ! Axis: MPL style (no Vega domain line, use view stroke)
        cfg%axis%defined = .true.
        cfg%axis%domain = .false.
        cfg%axis%domain_set = .true.
        cfg%axis%grid = .false.
        cfg%axis%grid_set = .true.
        cfg%axis%grid_color = '#b0b0b0'
        cfg%axis%grid_color_set = .true.
        cfg%axis%grid_opacity = 1.0_wp
        cfg%axis%grid_width = pts_to_px(0.8_wp, dpi)
        cfg%axis%label_font_size = pts_to_px(10.0_wp, dpi)
        cfg%axis%title_font_size = pts_to_px(10.0_wp, dpi)
        cfg%axis%tick_size = pts_to_px(3.5_wp, dpi)
        cfg%axis%tick_width = pts_to_px(0.8_wp, dpi)
        cfg%axis%tick_color = '#000000'
        cfg%axis%tick_color_set = .true.
        cfg%axis%title_font_weight = 'normal'
        cfg%axis%title_font_weight_set = .true.

        ! View: MPL-style black frame
        cfg%view%defined = .true.
        cfg%view%stroke = '#000000'
        cfg%view%stroke_set = .true.
        cfg%view%stroke_width = pts_to_px(0.8_wp, dpi)

        ! Mark defaults
        cfg%mark%defined = .true.
        cfg%mark%line_stroke_width = pts_to_px(1.5_wp, dpi)
        cfg%mark%point_size = pts_to_px(6.0_wp, dpi)**2
        cfg%mark%point_filled = .true.
        cfg%mark%point_filled_set = .true.
        cfg%mark%bar_fill = cfg%category_colors(1)
        cfg%mark%bar_fill_set = .true.

        ! Title: MPL centered, 12pt, normal weight
        cfg%title_config%defined = .true.
        cfg%title_config%font_size = pts_to_px(12.0_wp, dpi)
        cfg%title_config%font_weight = 'normal'
        cfg%title_config%font_weight_set = .true.
        cfg%title_config%anchor = 'middle'
        cfg%title_config%anchor_set = .true.
        cfg%title_config%offset = pts_to_px(6.0_wp, dpi)
        cfg%title_config%color = '#000000'
        cfg%title_config%color_set = .true.

        ! Legend
        cfg%legend%defined = .true.
        cfg%legend%orient = 'top-right'
        cfg%legend%orient_set = .true.
        cfg%legend%fill_color = '#ffffff'
        cfg%legend%fill_color_set = .true.
        cfg%legend%stroke_color = '#cccccc'
        cfg%legend%stroke_color_set = .true.
        cfg%legend%corner_radius = 6.0_wp
        cfg%legend%padding = 4.0_wp
        cfg%legend%frame_alpha = 0.8_wp
        cfg%legend%symbol_stroke_width = pts_to_px(1.5_wp, dpi)
    end function mpl_default_config

    pure function vegalite_default_config(dpi) result(cfg)
        !! Return config_t matching Vega-Lite visual defaults.
        real(wp), intent(in) :: dpi
        type(config_t) :: cfg

        cfg%defined = .true.
        cfg%background = '#ffffff'
        cfg%background_set = .true.

        ! Tableau-10 (same hex values as tab10)
        cfg%category_color_count = 10
        cfg%category_colors(1) = '#4c78a8'
        cfg%category_colors(2) = '#f58518'
        cfg%category_colors(3) = '#e45756'
        cfg%category_colors(4) = '#72b7b2'
        cfg%category_colors(5) = '#54a24b'
        cfg%category_colors(6) = '#eeca3b'
        cfg%category_colors(7) = '#b279a2'
        cfg%category_colors(8) = '#ff9da6'
        cfg%category_colors(9) = '#9d755d'
        cfg%category_colors(10) = '#bab0ac'

        ! Axis: verified from vega-lite src/config.ts + src/compile/axis
        ! Domain ON for main axis, grid ON for continuous scales,
        ! ticks inward (Vega default), gridColor=#888, domainColor=#ddd
        cfg%axis%defined = .true.
        cfg%axis%domain = .true.
        cfg%axis%domain_set = .true.
        cfg%axis%grid = .true.
        cfg%axis%grid_set = .true.
        cfg%axis%grid_color = '#888888'
        cfg%axis%grid_color_set = .true.
        cfg%axis%grid_opacity = 1.0_wp
        cfg%axis%label_font_size = 10.0_wp
        cfg%axis%title_font_size = 11.0_wp
        cfg%axis%tick_size = 5.0_wp
        cfg%axis%tick_width = 1.0_wp
        cfg%axis%tick_color = '#dddddd'
        cfg%axis%tick_color_set = .true.

        ! View: stroke=#ddd (from src/spec/base.ts line 165)
        cfg%view%defined = .true.
        cfg%view%stroke = '#dddddd'
        cfg%view%stroke_set = .true.
        cfg%view%stroke_width = 1.0_wp

        ! Mark: default color=#4c78a8, line 2px (from src/mark.ts)
        cfg%mark%defined = .true.
        cfg%mark%line_stroke_width = 2.0_wp
        cfg%mark%point_size = 30.0_wp
        cfg%mark%point_filled = .true.
        cfg%mark%point_filled_set = .true.
        cfg%mark%bar_fill = cfg%category_colors(1)
        cfg%mark%bar_fill_set = .true.

        ! Title: groupTitle=13, bold (from src/config.ts)
        cfg%title_config%defined = .true.
        cfg%title_config%font_size = 13.0_wp
        cfg%title_config%font_weight = 'bold'
        cfg%title_config%font_weight_set = .true.
        cfg%title_config%anchor = 'start'
        cfg%title_config%anchor_set = .true.
        cfg%title_config%offset = 10.0_wp
        cfg%title_config%color = '#000000'
        cfg%title_config%color_set = .true.

        ! Legend: orient right (from src/legend.ts)
        cfg%legend%defined = .true.
        cfg%legend%orient = 'right'
        cfg%legend%orient_set = .true.
        cfg%legend%symbol_stroke_width = 2.0_wp
    end function vegalite_default_config

end module fortplot_spec_config_defaults
