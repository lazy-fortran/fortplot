module fortplot_figure_management
    !! Figure lifecycle management module for fortplot_figure_core
    !! 
    !! This module handles figure lifecycle, initialization, I/O operations,
    !! animation support, and cleanup. Extracted from fortplot_figure_core.f90 
    !! for better organization following Single Responsibility Principle.
    !!
    !! Responsibilities:
    !! - Figure initialization and destruction
    !! - File I/O operations (savefig, show)
    !! - Animation support (PNG backend setup, data extraction)
    !! - Streamline data management
    !! - Subplot management

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_annotations, only: text_annotation_t
    use fortplot_plot_data, only: arrow_data_t, plot_data_t, subplot_data_t
    use fortplot_figure_initialization, only: figure_state_t, initialize_figure_state
    use fortplot_figure_core_io
    use fortplot_figure_streamlines, only: clear_streamline_data
    use fortplot_figure_subplots, only: create_subplots, add_subplot_plot, &
                                        get_subplot_plot_count, set_subplot_title, &
                                        set_subplot_xlabel, set_subplot_ylabel, &
                                        get_subplot_title
    use fortplot_figure_animation, only: setup_figure_png_backend_for_animation, &
                                        extract_figure_rgb_data_for_animation, &
                                        extract_figure_png_data_for_animation
    implicit none

    private
    public :: figure_initialize, figure_destroy, figure_clear, figure_savefig, figure_savefig_with_status
    public :: figure_show, figure_clear_streamlines, figure_setup_png_backend_for_animation
    public :: figure_extract_rgb_data_for_animation, figure_extract_png_data_for_animation
    public :: figure_subplots, figure_subplot_plot, figure_subplot_plot_count
    public :: figure_subplot_set_title, figure_subplot_set_xlabel, figure_subplot_set_ylabel
    public :: figure_subplot_title
    ! Core delegation wrappers
    public :: core_clear, core_clear_streamlines, core_destroy

contains

    subroutine figure_initialize(state, plots, streamlines, subplots_array, &
                                subplot_rows, subplot_cols, current_subplot, &
                                title_target, xlabel_target, ylabel_target, &
                                plot_count, width, height, backend, dpi)
        !! Initialize the figure with specified dimensions and backend
        !! Added DPI support for OO interface consistency with matplotlib interface
        use, intrinsic :: iso_fortran_env, only: wp => real64
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(plot_data_t), allocatable, intent(inout) :: streamlines(:)
        type(subplot_data_t), allocatable, intent(inout) :: subplots_array(:,:)
        integer, intent(inout) :: subplot_rows, subplot_cols, current_subplot
        character(len=:), allocatable, intent(inout) :: title_target, xlabel_target, ylabel_target
        integer, intent(inout) :: plot_count
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        real(wp), intent(in), optional :: dpi

        call initialize_figure_state(state, width, height, backend, dpi)
        
        ! Allocate/resize plots array using move_alloc to avoid manual deallocate()
        block
            type(plot_data_t), allocatable :: new_plots(:)
            allocate(new_plots(state%max_plots))
            call move_alloc(new_plots, plots)
        end block
        
        ! Reset streamlines to zero-length using move_alloc (no manual deallocate)
        block
            type(plot_data_t), allocatable :: new_streamlines(:)
            allocate(new_streamlines(0))
            call move_alloc(new_streamlines, streamlines)
        end block
        
        ! Reset subplot data to zero-size using move_alloc (no manual deallocate)
        block
            type(subplot_data_t), allocatable :: new_subplots(:,:)
            allocate(new_subplots(0,0))
            call move_alloc(new_subplots, subplots_array)
        end block
        subplot_rows = 0
        subplot_cols = 0
        current_subplot = 1
        
        ! Clear backward compatibility members via assignment (auto reallocation)
        title_target = ''
        xlabel_target = ''
        ylabel_target = ''
        plot_count = 0
    end subroutine figure_initialize

    subroutine figure_destroy(state, plots, streamlines, title_target, xlabel_target, ylabel_target)
        !! Finalize and clean up figure
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(plot_data_t), allocatable, intent(inout) :: streamlines(:)
        character(len=:), allocatable, intent(inout) :: title_target, xlabel_target, ylabel_target
        
        ! Backend is polymorphic allocatable; skip manual deallocation here.
        
        ! Reset plots/streamlines to zero-length using move_alloc
        block
            type(plot_data_t), allocatable :: tmp_plots(:), tmp_streamlines(:)
            allocate(tmp_plots(0))
            allocate(tmp_streamlines(0))
            call move_alloc(tmp_plots, plots)
            call move_alloc(tmp_streamlines, streamlines)
        end block
        ! Clear labels via assignment (auto reallocation)
        state%title = ''
        state%xlabel = ''
        state%ylabel = ''
        if (allocated(state%stream_arrows)) then
            block
                type(arrow_data_t), allocatable :: tmp_arrows(:)
                call move_alloc(state%stream_arrows, tmp_arrows)
            end block
        end if
        ! Clean up backward compatibility members via assignment
        title_target = ''
        xlabel_target = ''
        ylabel_target = ''
    end subroutine figure_destroy

    subroutine figure_savefig(state, plots, plot_count, filename, blocking, annotations, annotation_count, &
                              subplots_array, subplot_rows, subplot_cols)
        !! Save figure to file (backward compatibility version)
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        type(subplot_data_t), intent(in), optional :: subplots_array(:,:)
        integer, intent(in), optional :: subplot_rows, subplot_cols
        
        call savefig_figure(state, plots, plot_count, filename, blocking, annotations, annotation_count, &
                            subplots_array, subplot_rows, subplot_cols)
    end subroutine figure_savefig
    
    subroutine figure_savefig_with_status(state, plots, plot_count, filename, status, blocking, &
                                          annotations, annotation_count, subplots_array, subplot_rows, subplot_cols)
        !! Save figure to file with error status reporting
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        logical, intent(in), optional :: blocking
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        type(subplot_data_t), intent(in), optional :: subplots_array(:,:)
        integer, intent(in), optional :: subplot_rows, subplot_cols
        
        call savefig_with_status_figure(state, plots, plot_count, &
                                       filename, status, blocking, &
                                       annotations, annotation_count, subplots_array, subplot_rows, subplot_cols)
    end subroutine figure_savefig_with_status

    subroutine figure_show(state, plots, plot_count, blocking, annotations, annotation_count, &
                           subplots_array, subplot_rows, subplot_cols)
        !! Display the figure
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in), optional :: blocking
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        type(subplot_data_t), intent(in), optional :: subplots_array(:,:)
        integer, intent(in), optional :: subplot_rows, subplot_cols
        
        call show_figure(state, plots, plot_count, blocking, annotations, annotation_count, &
                         subplots_array, subplot_rows, subplot_cols)
    end subroutine figure_show

    subroutine figure_clear(state, streamlines, subplots_array, &
                           subplot_rows, subplot_cols, current_subplot, &
                           title_target, xlabel_target, ylabel_target, &
                           plot_count, annotation_count)
        !! Clear the figure to prepare for reuse (preserving backend settings)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: streamlines(:)
        type(subplot_data_t), allocatable, intent(inout) :: subplots_array(:,:)
        integer, intent(inout) :: subplot_rows, subplot_cols, current_subplot
        character(len=:), allocatable, intent(inout) :: title_target, xlabel_target, ylabel_target
        integer, intent(inout) :: plot_count, annotation_count
        
        ! Clear plot data
        plot_count = 0
        
        ! Clear streamlines by moving in a zero-length allocation
        block
            type(plot_data_t), allocatable :: tmp_streamlines(:)
            allocate(tmp_streamlines(0))
            call move_alloc(tmp_streamlines, streamlines)
        end block
        
        ! Clear subplot data by moving in a zero-size array
        block
            type(subplot_data_t), allocatable :: tmp_subplots(:,:)
            allocate(tmp_subplots(0,0))
            call move_alloc(tmp_subplots, subplots_array)
        end block
        subplot_rows = 0
        subplot_cols = 0
        current_subplot = 1
        
        ! Clear labels in backward compatibility members via assignment
        title_target = ''
        xlabel_target = ''
        ylabel_target = ''
        
        ! Clear labels in state via assignment
        state%title = ''
        state%xlabel = ''
        state%ylabel = ''
        if (allocated(state%stream_arrows)) then
            block
                type(arrow_data_t), allocatable :: tmp_arrows(:)
                call move_alloc(state%stream_arrows, tmp_arrows)
            end block
        end if

        ! Clear annotation count
        annotation_count = 0
        
        ! Reset rendered state to allow new rendering
        state%rendered = .false.
    end subroutine figure_clear

    subroutine figure_clear_streamlines(streamlines)
        !! Clear streamline data
        type(plot_data_t), allocatable, intent(inout) :: streamlines(:)
        call clear_streamline_data(streamlines)
    end subroutine figure_clear_streamlines

    ! Animation support - delegate to animation module
    subroutine figure_setup_png_backend_for_animation(state)
        !! Setup PNG backend for animation
        type(figure_state_t), intent(inout) :: state
        call setup_figure_png_backend_for_animation(state)
    end subroutine figure_setup_png_backend_for_animation
    
    subroutine figure_extract_rgb_data_for_animation(state, rgb_data, rendered)
        !! Extract RGB data for animation
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(out) :: rgb_data(:,:,:)
        logical, intent(in) :: rendered
        
        call extract_figure_rgb_data_for_animation(state, rgb_data, rendered)
    end subroutine figure_extract_rgb_data_for_animation
    
    subroutine figure_extract_png_data_for_animation(state, png_data, status, rendered)
        !! Extract PNG data for animation
        type(figure_state_t), intent(inout) :: state
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        logical, intent(in) :: rendered
        
        call extract_figure_png_data_for_animation(state, png_data, status, rendered)
    end subroutine figure_extract_png_data_for_animation

    subroutine figure_subplots(subplots_array, subplot_rows, subplot_cols, &
                              current_subplot, nrows, ncols)
        !! Create a grid of subplots
        type(subplot_data_t), allocatable, intent(inout) :: subplots_array(:,:)
        integer, intent(inout) :: subplot_rows, subplot_cols, current_subplot
        integer, intent(in) :: nrows, ncols
        logical :: subplot_active
        
        ! Delegate to module implementation
        call create_subplots(subplots_array, subplot_rows, &
                            subplot_cols, nrows, ncols, subplot_active)
        current_subplot = 1
    end subroutine figure_subplots
    
    subroutine figure_subplot_plot(subplots_array, subplot_rows, subplot_cols, &
                                  row, col, x, y, label, linestyle, color, colors, num_colors)
        !! Add a plot to a specific subplot
        type(subplot_data_t), intent(inout) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols
        integer, intent(in) :: row, col
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in) :: colors(:,:)
        integer, intent(in) :: num_colors
        
        call add_subplot_plot(subplots_array, subplot_rows, &
                             subplot_cols, row, col, x, y, label, &
                             linestyle, color, colors, num_colors)
    end subroutine figure_subplot_plot
    
    function figure_subplot_plot_count(subplots_array, subplot_rows, subplot_cols, &
                                      row, col) result(count)
        !! Get the number of plots in a specific subplot
        type(subplot_data_t), intent(in) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols
        integer, intent(in) :: row, col
        integer :: count
        
        count = get_subplot_plot_count(subplots_array, subplot_rows, &
                                       subplot_cols, row, col)
    end function figure_subplot_plot_count
    
    subroutine figure_subplot_set_title(subplots_array, subplot_rows, subplot_cols, &
                                       row, col, title)
        !! Set the title for a specific subplot
        type(subplot_data_t), intent(inout) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: title
        
        call set_subplot_title(subplots_array, subplot_rows, &
                              subplot_cols, row, col, title)
    end subroutine figure_subplot_set_title
    
    subroutine figure_subplot_set_xlabel(subplots_array, subplot_rows, subplot_cols, &
                                        row, col, xlabel)
        !! Set the x-axis label for a specific subplot
        type(subplot_data_t), intent(inout) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: xlabel
        
        call set_subplot_xlabel(subplots_array, subplot_rows, &
                               subplot_cols, row, col, xlabel)
    end subroutine figure_subplot_set_xlabel
    
    subroutine figure_subplot_set_ylabel(subplots_array, subplot_rows, subplot_cols, &
                                        row, col, ylabel)
        !! Set the y-axis label for a specific subplot
        type(subplot_data_t), intent(inout) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: ylabel
        
        call set_subplot_ylabel(subplots_array, subplot_rows, &
                               subplot_cols, row, col, ylabel)
    end subroutine figure_subplot_set_ylabel
    
    function figure_subplot_title(subplots_array, subplot_rows, subplot_cols, &
                                 row, col) result(title)
        !! Get the title for a specific subplot
        type(subplot_data_t), intent(in) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols
        integer, intent(in) :: row, col
        character(len=:), allocatable :: title
        
        title = get_subplot_title(subplots_array, subplot_rows, &
                                  subplot_cols, row, col)
    end function figure_subplot_title

    !!=============================================================================
    !! CORE MODULE DELEGATION WRAPPERS
    !! Simple wrapper procedures for core module delegation pattern
    !!=============================================================================

    subroutine core_clear(state, streamlines, subplots_array, subplot_rows, &
                         subplot_cols, current_subplot, title_target, xlabel_target, &
                         ylabel_target, plot_count, annotation_count)
        !! Clear the figure for reuse, preserving backend settings
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: streamlines(:)
        type(subplot_data_t), allocatable, intent(inout) :: subplots_array(:,:)
        integer, intent(inout) :: subplot_rows, subplot_cols, current_subplot, plot_count
        character(len=:), allocatable, intent(inout) :: title_target, xlabel_target, ylabel_target
        integer, intent(inout) :: annotation_count
        
        call figure_clear(state, streamlines, subplots_array, subplot_rows, &
                         subplot_cols, current_subplot, title_target, xlabel_target, &
                         ylabel_target, plot_count, annotation_count)
    end subroutine core_clear

    subroutine core_clear_streamlines(streamlines)
        type(plot_data_t), allocatable, intent(inout) :: streamlines(:)
        call figure_clear_streamlines(streamlines)
    end subroutine core_clear_streamlines

    subroutine core_destroy(state, plots, streamlines, title_target, xlabel_target, ylabel_target)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(plot_data_t), allocatable, intent(inout) :: streamlines(:)
        character(len=:), allocatable, intent(inout) :: title_target, xlabel_target, ylabel_target
        
        call figure_destroy(state, plots, streamlines, title_target, xlabel_target, ylabel_target)
    end subroutine core_destroy

end module fortplot_figure_management
