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
    public :: plot_data_t, arrow_data_t, subplot_t
    public :: PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
              PLOT_TYPE_ERRORBAR, PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, PLOT_TYPE_BOXPLOT, &
              PLOT_TYPE_SCATTER
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

    ! Constants for calculations
    real(wp), parameter :: HALF_WIDTH = 0.5_wp
    real(wp), parameter :: IQR_WHISKER_MULTIPLIER = 1.5_wp

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
        character(len=20) :: colormap = 'crest'
        logical :: show_colorbar = .true.
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
        real(wp), dimension(3) :: color
        character(len=:), allocatable :: label
        character(len=:), allocatable :: linestyle
        character(len=:), allocatable :: marker
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

contains

    logical function is_3d(self)
        !! Check if plot data contains 3D information
        !! Following KISS principle - simple check for z allocation
        class(plot_data_t), intent(in) :: self
        is_3d = allocated(self%z) .or. allocated(self%z_grid)
    end function is_3d

end module fortplot_plot_data