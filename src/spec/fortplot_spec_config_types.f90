module fortplot_spec_config_types
    !! Vega-Lite config types for style theming.
    !!
    !! These types mirror the Vega-Lite config object structure and allow
    !! fortplot to carry visual style information parsed from JSON or
    !! supplied by built-in style presets (MPL, Vega-Lite).
    !!
    !! Sentinel convention: -1.0 means "unset / use default".

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: config_t, config_axis_t, config_view_t
    public :: config_mark_defaults_t, config_title_t
    public :: config_legend_t, padding_t

    type :: padding_t
        !! Pixel padding around the plot area (maps to Vega-Lite padding).
        integer :: left = -1
        integer :: right = -1
        integer :: top = -1
        integer :: bottom = -1
        logical :: defined = .false.
    end type padding_t

    type :: config_axis_t
        !! Axis configuration (maps to Vega-Lite config.axis).
        logical :: domain = .true.
        logical :: domain_set = .false.
        logical :: grid = .false.
        logical :: grid_set = .false.
        real(wp) :: grid_opacity = -1.0_wp
        real(wp) :: grid_width = -1.0_wp
        character(len=7) :: grid_color = ''
        logical :: grid_color_set = .false.
        real(wp) :: label_font_size = -1.0_wp
        real(wp) :: title_font_size = -1.0_wp
        real(wp) :: tick_size = -1.0_wp
        real(wp) :: tick_width = -1.0_wp
        character(len=7) :: tick_color = ''
        logical :: tick_color_set = .false.
        character(len=40) :: label_font = ''
        logical :: label_font_set = .false.
        character(len=40) :: title_font = ''
        logical :: title_font_set = .false.
        character(len=20) :: title_font_weight = ''
        logical :: title_font_weight_set = .false.
        logical :: defined = .false.
    end type config_axis_t

    type :: config_view_t
        !! View rectangle (maps to Vega-Lite config.view).
        character(len=7) :: stroke = ''
        logical :: stroke_set = .false.
        real(wp) :: stroke_width = -1.0_wp
        logical :: defined = .false.
    end type config_view_t

    type :: config_mark_defaults_t
        !! Mark defaults (maps to Vega-Lite config.line/point/bar).
        real(wp) :: line_stroke_width = -1.0_wp
        real(wp) :: point_size = -1.0_wp
        logical :: point_filled = .true.
        logical :: point_filled_set = .false.
        character(len=7) :: bar_fill = ''
        logical :: bar_fill_set = .false.
        logical :: defined = .false.
    end type config_mark_defaults_t

    type :: config_title_t
        !! Title configuration (maps to Vega-Lite config.title).
        real(wp) :: font_size = -1.0_wp
        character(len=40) :: font = ''
        logical :: font_set = .false.
        character(len=20) :: font_weight = ''
        logical :: font_weight_set = .false.
        character(len=10) :: anchor = ''
        logical :: anchor_set = .false.
        real(wp) :: offset = -1.0_wp
        character(len=7) :: color = ''
        logical :: color_set = .false.
        logical :: defined = .false.
    end type config_title_t

    type :: config_legend_t
        !! Legend configuration (maps to Vega-Lite config.legend).
        character(len=20) :: orient = ''
        logical :: orient_set = .false.
        real(wp) :: label_font_size = -1.0_wp
        real(wp) :: symbol_stroke_width = -1.0_wp
        character(len=7) :: fill_color = ''
        logical :: fill_color_set = .false.
        character(len=7) :: stroke_color = ''
        logical :: stroke_color_set = .false.
        real(wp) :: corner_radius = -1.0_wp
        real(wp) :: padding = -1.0_wp
        logical :: defined = .false.
    end type config_legend_t

    type :: config_t
        !! Top-level Vega-Lite config object.
        character(len=7) :: background = ''
        logical :: background_set = .false.
        character(len=7) :: category_colors(10) = ''
        integer :: category_color_count = 0
        type(config_axis_t) :: axis
        type(config_view_t) :: view
        type(config_mark_defaults_t) :: mark
        type(config_title_t) :: title_config
        type(config_legend_t) :: legend
        logical :: defined = .false.
    end type config_t

end module fortplot_spec_config_types
