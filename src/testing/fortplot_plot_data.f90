module fortplot_plot_data
    !! Core plot data structures - extracted from fortplot_figure_core
    !! 
    !! This module contains only data structure definitions following the
    !! Single Responsibility Principle. Contains plot_data_t, arrow_data_t,
    !! and subplot_t type definitions.
    !! 
    !! SOLID: Single responsibility for data modeling only
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_pcolormesh, only: pcolormesh_t
    implicit none

    private
    public :: plot_data_t, arrow_data_t, subplot_t, subplot_data_t
    public :: AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    public :: PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
              PLOT_TYPE_ERRORBAR, PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, &
              PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
              PLOT_TYPE_SURFACE, PLOT_TYPE_PIE
    public :: HALF_WIDTH, IQR_WHISKER_MULTIPLIER

    ! Plot type constants
    integer, parameter :: PLOT_TYPE_LINE = 1
    integer, parameter :: PLOT_TYPE_CONTOUR = 2
    integer, parameter :: PLOT_TYPE_PCOLORMESH = 3
    integer, parameter :: PLOT_TYPE_ERRORBAR = 4
    integer, parameter :: PLOT_TYPE_BAR = 5
    integer, parameter :: PLOT_TYPE_HISTOGRAM = 6
    integer, parameter :: PLOT_TYPE_BOXPLOT = 7
    integer, parameter :: PLOT_TYPE_SCATTER = 8
    integer, parameter :: PLOT_TYPE_FILL = 9
    integer, parameter :: PLOT_TYPE_SURFACE = 10
    integer, parameter :: PLOT_TYPE_PIE = 11

    ! Constants for calculations
    real(wp), parameter :: HALF_WIDTH = 0.5_wp
    real(wp), parameter :: IQR_WHISKER_MULTIPLIER = 1.5_wp

    ! Axis selection identifiers for multi-axis figures
    integer, parameter :: AXIS_PRIMARY = 0
    integer, parameter :: AXIS_TWINX  = 1
    integer, parameter :: AXIS_TWINY  = 2

    type :: arrow_data_t
        !! Data container for streamplot arrows
        !! Stores position, direction, size and style for arrow rendering
        real(wp) :: x = 0.0_wp          ! Arrow position x-coordinate
        real(wp) :: y = 0.0_wp          ! Arrow position y-coordinate  
        real(wp) :: dx = 0.0_wp         ! Arrow direction x-component (normalized)
        real(wp) :: dy = 0.0_wp         ! Arrow direction y-component (normalized)
        real(wp) :: size = 1.0_wp       ! Arrow size scaling factor
        character(len=10) :: style = '->' ! Arrow style (matplotlib compatible)
    end type arrow_data_t

    type :: fill_between_data_t
        !! Storage for fill_between polygon samples
        real(wp), allocatable :: x(:)
        real(wp), allocatable :: upper(:)
        real(wp), allocatable :: lower(:)
        logical, allocatable :: mask(:)
        logical :: has_mask = .false.
    end type fill_between_data_t

    type :: plot_data_t
        !! Data container for individual plots
        !! Separated from figure to follow Single Responsibility Principle
        integer :: plot_type = PLOT_TYPE_LINE
        ! Line plot data
        real(wp), allocatable :: x(:), y(:), z(:)  ! z optional for 3D plots
        ! Contour plot data
        real(wp), allocatable :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), allocatable :: contour_levels(:)
        ! Color contour properties
        logical :: use_color_levels = .false.
        logical :: fill_contours = .false.
        character(len=20) :: colormap = 'crest'
        logical :: show_colorbar = .true.
        ! Surface plot properties
        logical :: surface_show_colorbar = .false.
        real(wp) :: surface_alpha = 1.0_wp
        real(wp) :: surface_linewidth = 1.0_wp
        logical :: surface_use_colormap = .false.
        real(wp) :: surface_edgecolor(3) = [0.0_wp, 0.447_wp, 0.698_wp]
        ! Store requested colormap name for future renderer parity
        character(len=:), allocatable :: surface_colormap
        ! Pcolormesh data
        type(pcolormesh_t) :: pcolormesh_data
        ! Bar chart data
        real(wp), allocatable :: bar_x(:), bar_heights(:)
        real(wp) :: bar_width = 0.8_wp
        logical :: bar_horizontal = .false.
        ! Histogram data
        real(wp), allocatable :: hist_bin_edges(:)
        real(wp), allocatable :: hist_counts(:)
        logical :: hist_density = .false.
        ! Box plot data
        real(wp), allocatable :: box_data(:)
        real(wp) :: position = 1.0_wp
        real(wp) :: width = 0.6_wp
        logical :: show_outliers = .true.
        logical :: horizontal = .false.
        real(wp) :: q1, q2, q3  ! Quartiles
        real(wp) :: whisker_low, whisker_high
        real(wp), allocatable :: outliers(:)
        ! Error bar data
        real(wp), allocatable :: xerr(:), yerr(:)
        real(wp), allocatable :: xerr_lower(:), xerr_upper(:)
        real(wp), allocatable :: yerr_lower(:), yerr_upper(:)
        real(wp) :: capsize = 5.0_wp
        real(wp) :: elinewidth = 1.0_wp
        logical :: has_xerr = .false., has_yerr = .false.
        logical :: asymmetric_xerr = .false., asymmetric_yerr = .false.
        ! Scatter plot data
        real(wp), allocatable :: scatter_sizes(:)     ! Variable marker sizes
        real(wp), allocatable :: scatter_colors(:)    ! Variable marker colors
        real(wp) :: scatter_size_default = 20.0_wp    ! Default marker size
        character(len=20) :: scatter_colormap = 'viridis'  ! Colormap for color mapping
        logical :: scatter_colorbar = .false.         ! Show colorbar for color mapping
        real(wp) :: scatter_vmin = 0.0_wp            ! Color scale minimum
        real(wp) :: scatter_vmax = 1.0_wp            ! Color scale maximum
        logical :: scatter_vrange_set = .false.      ! Whether vmin/vmax are manually set
        ! Common properties
        real(wp), dimension(3) :: color = [0.0_wp, 0.447_wp, 0.698_wp]  ! Default to blue
        character(len=:), allocatable :: label
        character(len=:), allocatable :: linestyle
        character(len=:), allocatable :: marker
        ! Filled polygon data (fill_between)
        type(fill_between_data_t) :: fill_between_data
        real(wp) :: fill_alpha = 1.0_wp
        ! Pie chart data
        integer :: pie_slice_count = 0
        real(wp), allocatable :: pie_start(:)
        real(wp), allocatable :: pie_end(:)
        real(wp), allocatable :: pie_offsets(:)
        real(wp), allocatable :: pie_colors(:,:)
        real(wp), allocatable :: pie_label_pos(:,:)
        real(wp), allocatable :: pie_values(:)
        integer, allocatable :: pie_source_index(:)
        character(len=:), allocatable :: pie_labels(:)
        character(len=:), allocatable :: pie_autopct
        real(wp) :: pie_radius = 1.0_wp
        real(wp) :: pie_center(2) = [0.0_wp, 0.0_wp]
        ! Axis assignment (primary by default)
        integer :: axis = AXIS_PRIMARY
    contains
        procedure :: is_3d
    end type plot_data_t

    type :: subplot_t
        !! Individual subplot container
        type(plot_data_t), allocatable :: plots(:)
        integer :: plot_count = 0
        integer :: max_plots = 10
        ! Subplot-specific properties
        character(len=:), allocatable :: title
        character(len=:), allocatable :: xlabel
        character(len=:), allocatable :: ylabel
        character(len=10) :: xscale = 'linear'
        character(len=10) :: yscale = 'linear'
    end type subplot_t
    
    ! Subplot data storage to avoid recursive type
    type :: subplot_data_t
        !! Subplot data container (extracted from fortplot_figure_core)
        type(plot_data_t), allocatable :: plots(:)
        integer :: plot_count = 0
        character(len=:), allocatable :: title
        character(len=:), allocatable :: xlabel
        character(len=:), allocatable :: ylabel
        real(wp) :: x_min, x_max, y_min, y_max
        logical :: xlim_set = .false., ylim_set = .false.
        integer :: max_plots = 100
    end type subplot_data_t

contains

    logical function is_3d(self)
        !! Check if plot represents true 3D data
        !! A plot is 3D only when explicit 3D samples (x,y,z) are provided.
        !! Contour/pcolormesh (z_grid over x/y grid) are 2D renderings and must not
        !! trigger 3D axes.
        class(plot_data_t), intent(in) :: self
        if (allocated(self%z)) then
            is_3d = size(self%z) > 0
            return
        end if

        if (self%plot_type == PLOT_TYPE_SURFACE) then
            if (allocated(self%z_grid)) then
                is_3d = size(self%z_grid) > 0
            else
                is_3d = .false.
            end if
            return
        end if

        is_3d = .false.
    end function is_3d

end module fortplot_plot_data
