module fortplot_spec_types
    !! Vega-Lite-shaped specification types for fortplot
    !!
    !! Defines Fortran derived types that map 1:1 to Vega-Lite schema
    !! fields. These types serve as the single source of truth for plot
    !! specifications, decoupling frontend API from backend rendering.
    !!
    !! The Fortran API builds spec_t directly (no JSON overhead).
    !! JSON is a serialization layer for interop with Vega ecosystem.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_annotations, only: text_annotation_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t, subplot_data_t
    implicit none

    private
    public :: spec_t, mark_t, encoding_t, channel_t
    public :: data_t, data_column_t, scale_t, axis_t
    public :: field_plot_t, layer_t, scene_t

    type :: axis_t
        !! Axis configuration (maps to Vega-Lite axis object)
        character(len=:), allocatable :: title
        logical :: grid = .false.
        real(wp) :: label_angle = 0.0_wp
        logical :: title_set = .false.
    end type axis_t

    type :: scale_t
        !! Scale configuration (maps to Vega-Lite scale object)
        !! Supported types: linear, log, pow, sqrt, symlog,
        !! ordinal, band, point
        character(len=:), allocatable :: type
        real(wp) :: domain_min = 0.0_wp
        real(wp) :: domain_max = 0.0_wp
        logical :: domain_set = .false.
        logical :: zero = .false.
        real(wp) :: exponent = 1.0_wp
    end type scale_t

    type :: channel_t
        !! Encoding channel (maps to Vega-Lite channel object)
        !! Connects a data field to a visual property.
        character(len=:), allocatable :: field
        character(len=:), allocatable :: type
        type(scale_t) :: scale
        type(axis_t) :: axis
        character(len=:), allocatable :: value
        logical :: defined = .false.
    end type channel_t

    type :: encoding_t
        !! Encoding specification (maps to Vega-Lite encoding object)
        !! Maps data fields to visual channels.
        type(channel_t) :: x, y, x2, y2
        type(channel_t) :: color, size, shape, opacity, text
    end type encoding_t

    type :: mark_t
        !! Mark specification (maps to Vega-Lite mark object)
        !! Defines the visual representation type and properties.
        character(len=:), allocatable :: type
        real(wp) :: size = -1.0_wp
        real(wp) :: opacity = 1.0_wp
        real(wp) :: stroke_width = -1.0_wp
        character(len=:), allocatable :: stroke
        character(len=:), allocatable :: fill
        logical :: filled = .true.
        character(len=:), allocatable :: interpolate
        character(len=:), allocatable :: point
    end type mark_t

    type :: data_column_t
        !! Single named column of data values
        character(len=:), allocatable :: field
        real(wp), allocatable :: values(:)
        character(len=:), allocatable :: string_values(:)
        logical :: is_string = .false.
    end type data_column_t

    type :: data_t
        !! Data specification (maps to Vega-Lite data object)
        !! Stores inline values as named columns (Fortran-native layout).
        !! Serialized to row-oriented JSON for Vega-Lite compatibility.
        type(data_column_t), allocatable :: columns(:)
        integer :: nrows = 0
    end type data_t

    type :: field_plot_t
        !! fortplot-specific field-plot metadata carried alongside the
        !! Vega-Lite-shaped spec for contour, pcolormesh, and streamplot data.
        real(wp), allocatable :: x(:), y(:)
        real(wp), allocatable :: z(:), u(:), v(:)
        real(wp), allocatable :: levels(:)
        integer :: nrows = 0
        integer :: ncols = 0
        logical :: defined = .false.
        character(len=:), allocatable :: colormap
        logical :: show_colorbar = .false.
        logical :: show_colorbar_set = .false.
        real(wp) :: density = -1.0_wp
        real(wp) :: vmin = 0.0_wp
        real(wp) :: vmax = 0.0_wp
        logical :: vmin_set = .false.
        logical :: vmax_set = .false.
        real(wp) :: linewidths = -1.0_wp
    end type field_plot_t

    type :: layer_t
        !! Single layer within a layered spec
        type(mark_t) :: mark
        type(encoding_t) :: encoding
        type(data_t) :: data
        type(field_plot_t) :: field
        logical :: has_data = .false.
    end type layer_t

    type :: scene_t
        !! fortplot-native scene extension used for strict frontend cutover.
        !! This keeps spec_t as the only render contract while preserving
        !! existing OO and pyplot semantics that are not plain Vega-Lite.
        type(figure_state_t) :: state
        type(plot_data_t), allocatable :: plots(:)
        type(text_annotation_t), allocatable :: annotations(:)
        type(subplot_data_t), allocatable :: subplots_array(:, :)
        integer :: plot_count = 0
        integer :: annotation_count = 0
        integer :: subplot_rows = 0
        integer :: subplot_cols = 0
        logical :: defined = .false.
    end type scene_t

    type :: spec_t
        !! Top-level Vega-Lite specification
        !! Single source of truth for a plot. The Fortran plot() API
        !! is syntactic sugar that builds a spec_t.
        character(len=:), allocatable :: schema
        type(mark_t) :: mark
        type(encoding_t) :: encoding
        type(data_t) :: data
        type(field_plot_t) :: field
        character(len=:), allocatable :: title
        integer :: width = 400
        integer :: height = 300
        type(layer_t), allocatable :: layers(:)
        integer :: layer_count = 0
        logical :: is_layered = .false.
        type(scene_t) :: scene
    end type spec_t

end module fortplot_spec_types
