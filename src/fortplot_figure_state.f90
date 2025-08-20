module fortplot_figure_state
    !! Figure state management module - extracted from fortplot_figure_core
    !! 
    !! This module contains procedures for figure initialization, configuration,
    !! and property management following the Single Responsibility Principle.
    !! 
    !! SOLID: Single responsibility for figure state management only
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_utils, only: initialize_backend
    use fortplot_plot_data, only: plot_data_t, arrow_data_t
    use fortplot_legend, only: legend_t
    use fortplot_annotations, only: text_annotation_t
    implicit none

    private
    public :: figure_state_t
    public :: initialize_figure_state, configure_figure_dimensions
    public :: configure_figure_margins, configure_figure_scales
    public :: configure_figure_limits, configure_figure_labels

    type :: figure_state_t
        !! Figure state container
        !! Contains only state information, no behavior
        
        ! Backend and rendering state
        class(plot_context), allocatable :: backend
        integer :: plot_count = 0
        logical :: rendered = .false.
        
        ! Figure dimensions
        integer :: width = 640
        integer :: height = 480

        ! Plot area settings
        real(wp) :: margin_left = 0.15_wp
        real(wp) :: margin_right = 0.05_wp
        real(wp) :: margin_bottom = 0.15_wp
        real(wp) :: margin_top = 0.05_wp
        
        ! Scale settings
        character(len=10) :: xscale = 'linear'
        character(len=10) :: yscale = 'linear'
        real(wp) :: symlog_threshold = 1.0_wp
        
        ! Axis limits - separate original and transformed ranges
        real(wp) :: x_min, x_max, y_min, y_max  ! Original data ranges for tick generation
        real(wp) :: z_min, z_max  ! Z-axis limits for 3D plots
        real(wp) :: x_min_transformed, x_max_transformed, y_min_transformed, y_max_transformed  ! Transformed for rendering
        logical :: xlim_set = .false., ylim_set = .false.
        
        ! Figure and axis labels
        character(len=:), allocatable :: title
        character(len=:), allocatable :: xlabel
        character(len=:), allocatable :: ylabel

        ! Color palette: seaborn colorblind palette
        real(wp), dimension(3,6) :: colors = reshape([ &
            0.0_wp,   0.447_wp, 0.698_wp,  & ! #0072B2 (blue)
            0.0_wp,   0.619_wp, 0.451_wp,  & ! #009E73 (green)
            0.835_wp, 0.369_wp, 0.0_wp,    & ! #D55E00 (orange)
            0.8_wp,   0.475_wp, 0.655_wp,  & ! #CC79A7 (purple)
            0.941_wp, 0.894_wp, 0.259_wp,  & ! #F0E442 (yellow)
            0.337_wp, 0.702_wp, 0.914_wp], & ! #56B4E9 (cyan)
            [3,6])

        ! Data storage
        type(plot_data_t), allocatable :: plots(:)
        type(plot_data_t), allocatable :: streamlines(:)
        type(arrow_data_t), allocatable :: arrow_data(:)
        
        ! Legend support
        type(legend_t) :: legend_data
        logical :: show_legend = .false.
        integer :: max_plots = 500
        
        ! Line drawing properties
        real(wp) :: current_line_width = 1.0_wp
        
        ! Error state
        logical :: has_error = .false.
        
        ! Text annotation support
        type(text_annotation_t), allocatable :: annotations(:)
        integer :: annotation_count = 0
        integer :: max_annotations = 100
    end type figure_state_t

contains

    subroutine initialize_figure_state(state, width, height, backend_type)
        !! Initialize figure state with specified dimensions and optional backend
        !! Following SOLID principles - focused only on state initialization
        type(figure_state_t), intent(inout) :: state
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend_type
        
        call configure_figure_dimensions(state, width, height)
        
        if (.not. allocated(state%plots)) then
            allocate(state%plots(state%max_plots))
        end if
        state%plot_count = 0
        state%rendered = .false.
        
        ! Initialize annotations
        if (.not. allocated(state%annotations)) then
            allocate(state%annotations(state%max_annotations))
        end if
        state%annotation_count = 0
        
        ! Initialize backend if specified
        if (present(backend_type)) then
            call initialize_backend(state%backend, backend_type, state%width, state%height)
        end if
        
    end subroutine initialize_figure_state

    subroutine configure_figure_dimensions(state, width, height)
        !! Configure figure dimensions
        !! Following SRP - handles only dimension configuration
        type(figure_state_t), intent(inout) :: state
        integer, intent(in), optional :: width, height
        
        if (present(width)) state%width = width
        if (present(height)) state%height = height
        
    end subroutine configure_figure_dimensions

    subroutine configure_figure_margins(state, left, right, bottom, top)
        !! Configure figure margins
        !! Following SRP - handles only margin configuration
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in), optional :: left, right, bottom, top
        
        if (present(left)) state%margin_left = left
        if (present(right)) state%margin_right = right
        if (present(bottom)) state%margin_bottom = bottom
        if (present(top)) state%margin_top = top
        
    end subroutine configure_figure_margins

    subroutine configure_figure_scales(state, xscale, yscale, symlog_threshold)
        !! Configure figure scale settings
        !! Following SRP - handles only scale configuration
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: symlog_threshold
        
        if (present(xscale)) state%xscale = xscale
        if (present(yscale)) state%yscale = yscale
        if (present(symlog_threshold)) state%symlog_threshold = symlog_threshold
        
    end subroutine configure_figure_scales

    subroutine configure_figure_limits(state, xlim, ylim)
        !! Configure figure axis limits
        !! Following SRP - handles only limit configuration
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in), optional :: xlim(2), ylim(2)
        
        if (present(xlim)) then
            state%x_min = xlim(1)
            state%x_max = xlim(2)
            state%xlim_set = .true.
        end if
        
        if (present(ylim)) then
            state%y_min = ylim(1)
            state%y_max = ylim(2)
            state%ylim_set = .true.
        end if
        
    end subroutine configure_figure_limits

    subroutine configure_figure_labels(state, title, xlabel, ylabel)
        !! Configure figure labels
        !! Following SRP - handles only label configuration
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        
        if (present(title)) then
            if (allocated(state%title)) deallocate(state%title)
            allocate(character(len=len_trim(title)) :: state%title)
            state%title = trim(title)
        end if
        
        if (present(xlabel)) then
            if (allocated(state%xlabel)) deallocate(state%xlabel)
            allocate(character(len=len_trim(xlabel)) :: state%xlabel)
            state%xlabel = trim(xlabel)
        end if
        
        if (present(ylabel)) then
            if (allocated(state%ylabel)) deallocate(state%ylabel)
            allocate(character(len=len_trim(ylabel)) :: state%ylabel)
            state%ylabel = trim(ylabel)
        end if
        
    end subroutine configure_figure_labels

end module fortplot_figure_state