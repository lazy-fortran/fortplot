! Consolidated (Issue #934)

! ==== Begin: src/figures/core/fortplot_figure_core_io.f90 ====

module fortplot_figure_core_io
    !! Figure I/O and rendering operations module
    !! 
    !! This module contains figure save/show/render functionality
    !! extracted from fortplot_figure_core for architectural compliance
    !!
    !! ARCHITECTURAL REFACTORING (Issue #678):
    !! - Focused module for I/O operations
    !! - Single Responsibility Principle compliance
    !! - Clean separation from plot management

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_utils, only: get_backend_from_filename
    use fortplot_figure_initialization, only: setup_figure_backend, figure_state_t
    use fortplot_errors, only: SUCCESS, ERROR_FILE_IO, is_error
    use fortplot_logging, only: log_error, log_warning
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_ascii, only: ascii_context
    use fortplot_margins, only: calculate_plot_area
    use fortplot_pdf_coordinate, only: calculate_pdf_plot_area
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges, &
                                                    setup_coordinate_system, &
                                                    render_figure_background, &
                                                    render_figure_axes, &
                                                    render_all_plots, &
                                                    render_figure_axes_labels_only
    use fortplot_figure_grid, only: render_grid_lines
    use fortplot_annotation_rendering, only: render_figure_annotations
    use fortplot_figure_io, only: save_backend_with_status
    use fortplot_figure_utilities, only: is_interactive_environment, wait_for_user_input
    use fortplot_plot_data, only: plot_data_t, subplot_data_t
    implicit none

    private
    public :: savefig_figure, savefig_with_status_figure, show_figure, render_figure_impl

contains

    subroutine savefig_figure(state, plots, plot_count, filename, blocking, annotations, annotation_count, &
                              subplots_array, subplot_rows, subplot_cols)
        !! Save figure to file (backward compatibility version)
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        ! Optional subplot data for multi-axes rendering
        type(subplot_data_t), intent(in), optional :: subplots_array(:,:)
        integer, intent(in), optional :: subplot_rows, subplot_cols
        
        integer :: status
        
        ! Delegate to version with status reporting
        call savefig_with_status_figure(state, plots, plot_count, filename, status, blocking, &
                                        annotations, annotation_count, subplots_array, subplot_rows, subplot_cols)
        
        ! Log error if save failed (maintains existing behavior)
        if (status /= SUCCESS) then
            call log_error("Failed to save figure to '" // trim(filename) // "'")
        end if
    end subroutine savefig_figure
    
    subroutine savefig_with_status_figure(state, plots, plot_count, filename, status, blocking, &
                                          annotations, annotation_count, subplots_array, subplot_rows, subplot_cols)
        !! Save figure to file with error status reporting
        !! Added Issue #854: File path validation for user input safety
        use fortplot_annotations, only: text_annotation_t
        use fortplot_parameter_validation, only: validate_file_path, parameter_validation_result_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        logical, intent(in), optional :: blocking
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        ! Optional subplot data for multi-axes rendering
        type(subplot_data_t), intent(in), optional :: subplots_array(:,:)
        integer, intent(in), optional :: subplot_rows, subplot_cols
        
        character(len=20) :: required_backend, current_backend
        logical :: block, need_backend_switch
        type(parameter_validation_result_t) :: path_validation
        
        ! Initialize success status
        status = SUCCESS
        
        ! Validate filename path before proceeding
        path_validation = validate_file_path(filename, check_parent=.true., context="savefig")
        if (.not. path_validation%is_valid) then
            status = ERROR_FILE_IO
            return
        end if
        
        block = .true.
        if (present(blocking)) block = blocking
        
        ! Determine required backend from filename extension
        required_backend = get_backend_from_filename(filename)
        
        ! Determine current backend type
        select type (backend => state%backend)
        type is (png_context)
            current_backend = 'png'
        type is (pdf_context)
            current_backend = 'pdf'
        type is (ascii_context)
            current_backend = 'ascii'
        class default
            current_backend = 'unknown'
        end select
        
        ! Check if we need to switch backends
        need_backend_switch = (trim(required_backend) /= trim(current_backend))
        
        if (need_backend_switch) then
            call setup_figure_backend(state, required_backend)
        end if
        
        ! Render if not already rendered (with annotations if provided)
        if (.not. state%rendered) then
            call render_figure_impl(state, plots, plot_count, annotations, annotation_count, &
                                   subplots_array, subplot_rows, subplot_cols)
        end if
        
        ! Save the figure with status checking
        call save_backend_with_status(state%backend, filename, status)
    end subroutine savefig_with_status_figure

    subroutine show_figure(state, plots, plot_count, blocking, annotations, annotation_count, &
                           subplots_array, subplot_rows, subplot_cols)
        !! Display the figure
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in), optional :: blocking
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        ! Optional subplot data for multi-axes rendering
        type(subplot_data_t), intent(in), optional :: subplots_array(:,:)
        integer, intent(in), optional :: subplot_rows, subplot_cols
        
        logical :: block
        
        ! Default to non-blocking behavior to prevent hangs in automated environments
        ! Users can explicitly set blocking=true for interactive sessions
        block = .false.
        if (present(blocking)) block = blocking
        
        ! Render if not already rendered (with annotations if provided)
        if (.not. state%rendered) then
            call render_figure_impl(state, plots, plot_count, annotations, annotation_count, &
                                   subplots_array, subplot_rows, subplot_cols)
        end if
        
        ! Display the figure
        call state%backend%save("terminal")
        
        ! Handle blocking behavior - when blocking=true, wait for user input
        if (block) then
            call wait_for_user_input()
        end if
    end subroutine show_figure

    subroutine render_figure_impl(state, plots, plot_count, annotations, annotation_count, &
                                  subplots_array, subplot_rows, subplot_cols)
        !! Main rendering pipeline implementation
        !! Fixed Issue #432: Always render axes/labels even with no plot data
        !! Fixed Issue #844: ASCII annotation functionality
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        ! Optional subplot data for multi-axes rendering
        type(subplot_data_t), intent(in), optional :: subplots_array(:,:)
        integer, intent(in), optional :: subplot_rows, subplot_cols
        
        logical :: have_subplots
        
        have_subplots = .false.
        if (present(subplots_array) .and. present(subplot_rows) .and. present(subplot_cols)) then
            if (size(subplots_array, 1) == subplot_rows .and. size(subplots_array, 2) == subplot_cols) then
                have_subplots = (subplot_rows > 0 .and. subplot_cols > 0)
            end if
        end if
        
        if (have_subplots) then
            call render_subplots_impl(state, subplots_array, subplot_rows, subplot_cols)
            state%rendered = .true.
            return
        end if
        
        ! Single-axes rendering path (legacy)
        ! Calculate final data ranges
        call calculate_figure_data_ranges(plots, plot_count, &
                                        state%xlim_set, state%ylim_set, &
                                        state%x_min, state%x_max, &
                                        state%y_min, state%y_max, &
                                        state%x_min_transformed, &
                                        state%x_max_transformed, &
                                        state%y_min_transformed, &
                                        state%y_max_transformed, &
                                        state%xscale, state%yscale, &
                                        state%symlog_threshold)
        
        call render_single_axes_impl(state, plots, plot_count, annotations, annotation_count)
        state%rendered = .true.
    end subroutine render_figure_impl

    subroutine render_subplots_impl(state, subplots_array, subplot_rows, subplot_cols)
        !! Render a grid of subplots with independent axes/labels per cell
        type(figure_state_t), intent(inout) :: state
        type(subplot_data_t), intent(in) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols

        integer :: nr, nc, i, j
        real(wp) :: base_left, base_right, base_bottom, base_top
        real(wp) :: cell_w, cell_h, left_f, right_f, bottom_f, top_f
        real(wp) :: lxmin, lxmax, lymin, lymax
        real(wp) :: lxmin_t, lxmax_t, lymin_t, lymax_t

        base_left   = 0.10_wp
        base_right  = 0.95_wp
        base_bottom = 0.10_wp
        base_top    = 0.90_wp
        nr = subplot_rows; nc = subplot_cols
        cell_w = (base_right - base_left) / real(nc, wp)
        cell_h = (base_top   - base_bottom) / real(nr, wp)

        do i = 1, nr
            do j = 1, nc
                left_f   = base_left   + real(j-1, wp) * cell_w
                right_f  = base_left   + real(j,   wp) * cell_w
                bottom_f = base_bottom + real(nr-i, wp) * cell_h
                top_f    = base_bottom + real(nr-i+1, wp) * cell_h

                select type (bk => state%backend)
                class is (png_context)
                    bk%margins%left = left_f
                    bk%margins%right = right_f
                    bk%margins%bottom = bottom_f
                    bk%margins%top = top_f
                    call calculate_plot_area(bk%width, bk%height, bk%margins, bk%plot_area)
                class is (pdf_context)
                    bk%margins%left = left_f
                    bk%margins%right = right_f
                    bk%margins%bottom = bottom_f
                    bk%margins%top = top_f
                    call calculate_pdf_plot_area(bk%width, bk%height, bk%margins, bk%plot_area)
                class is (ascii_context)
                    ! ASCII backend ignores plot area; keep defaults
                class default
                    ! Unknown backend; fall back to defaults silently
                end select

                call calculate_figure_data_ranges(subplots_array(i,j)%plots, subplots_array(i,j)%plot_count, &
                                                subplots_array(i,j)%xlim_set, subplots_array(i,j)%ylim_set, &
                                                lxmin, lxmax, lymin, lymax, &
                                                lxmin_t, lxmax_t, lymin_t, lymax_t, &
                                                state%xscale, state%yscale, state%symlog_threshold)

                call setup_coordinate_system(state%backend, lxmin_t, lxmax_t, lymin_t, lymax_t)

                call render_figure_axes(state%backend, state%xscale, state%yscale, state%symlog_threshold, &
                                       lxmin, lxmax, lymin, lymax, &
                                       subplots_array(i,j)%title, subplots_array(i,j)%xlabel, subplots_array(i,j)%ylabel, &
                                       subplots_array(i,j)%plots, subplots_array(i,j)%plot_count)

                if (subplots_array(i,j)%plot_count > 0) then
                    call render_all_plots(state%backend, subplots_array(i,j)%plots, subplots_array(i,j)%plot_count, &
                                         lxmin_t, lxmax_t, lymin_t, lymax_t, &
                                         state%xscale, state%yscale, state%symlog_threshold, &
                                         state%width, state%height, &
                                         state%margin_left, state%margin_right, state%margin_bottom, state%margin_top)
                end if

                call render_figure_axes_labels_only(state%backend, state%xscale, state%yscale, state%symlog_threshold, &
                                                   lxmin, lxmax, lymin, lymax, &
                                                   subplots_array(i,j)%title, subplots_array(i,j)%xlabel, subplots_array(i,j)%ylabel, &
                                                   subplots_array(i,j)%plots, subplots_array(i,j)%plot_count)
            end do
        end do
    end subroutine render_subplots_impl

    subroutine render_single_axes_impl(state, plots, plot_count, annotations, annotation_count)
        !! Render the legacy single-axes path
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count

        call setup_coordinate_system(state%backend, &
                                   state%x_min_transformed, state%x_max_transformed, &
                                   state%y_min_transformed, state%y_max_transformed)

        call render_figure_background(state%backend)

        if (state%grid_enabled) then
            call render_grid_lines(state%backend, state%grid_enabled, &
                                  state%grid_which, state%grid_axis, &
                                  state%grid_alpha, state%width, state%height, &
                                  state%margin_left, state%margin_right, &
                                  state%margin_bottom, state%margin_top, &
                                  state%xscale, state%yscale, &
                                  state%symlog_threshold, state%x_min, state%x_max, &
                                  state%y_min, state%y_max, &
                                  state%x_min_transformed, state%x_max_transformed, &
                                  state%y_min_transformed, state%y_max_transformed, &
                                  state%grid_linestyle)
        end if

        call render_figure_axes(state%backend, state%xscale, state%yscale, &
                               state%symlog_threshold, state%x_min, state%x_max, &
                               state%y_min, state%y_max, state%title, &
                               state%xlabel, state%ylabel, plots, plot_count)

        if (plot_count > 0) then
            call render_all_plots(state%backend, plots, plot_count, &
                                 state%x_min_transformed, state%x_max_transformed, &
                                 state%y_min_transformed, state%y_max_transformed, &
                                 state%xscale, state%yscale, state%symlog_threshold, &
                                 state%width, state%height, &
                                 state%margin_left, state%margin_right, &
                                 state%margin_bottom, state%margin_top)
        end if

        call render_figure_axes_labels_only(state%backend, state%xscale, state%yscale, &
                                           state%symlog_threshold, state%x_min, state%x_max, &
                                           state%y_min, state%y_max, state%title, &
                                           state%xlabel, state%ylabel, plots, plot_count)

        if (state%show_legend .and. state%legend_data%num_entries > 0) then
            call state%legend_data%render(state%backend)
        end if

        if (present(annotations) .and. present(annotation_count)) then
            if (annotation_count > 0) then
                call render_figure_annotations(state%backend, annotations, annotation_count, &
                                              state%x_min, state%x_max, &
                                              state%y_min, state%y_max, &
                                              state%width, state%height, &
                                              state%margin_left, state%margin_right, &
                                              state%margin_bottom, state%margin_top)
            end if
        end if
    end subroutine render_single_axes_impl

end module fortplot_figure_core_io
! ==== End: src/figures/core/fortplot_figure_core_io.f90 ====

! ==== Begin: src/figures/core/fortplot_figure_core_config.f90 ====

module fortplot_figure_core_config
    !! Figure configuration operations module
    !! 
    !! This module contains figure configuration functionality (labels, scales, limits)
    !! extracted from fortplot_figure_core for architectural compliance
    !!
    !! ARCHITECTURAL REFACTORING (Issue #678):
    !! - Focused module for configuration operations
    !! - Single Responsibility Principle compliance
    !! - Clean separation from plot data management

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization
    use fortplot_figure_grid
    implicit none

    private
    public :: set_xlabel_figure, set_ylabel_figure, set_title_figure
    public :: set_xscale_figure, set_yscale_figure, set_xlim_figure, set_ylim_figure
    public :: set_line_width_figure, grid_figure
    ! Configuration wrapper procedures for core module delegation
    public :: core_set_xlabel, core_set_ylabel, core_set_title
    public :: core_set_xscale, core_set_yscale, core_set_xlim, core_set_ylim
    public :: core_set_line_width, core_grid

contains

    subroutine set_xlabel_figure(state, xlabel_compat, label)
        !! Set x-axis label
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: xlabel_compat
        character(len=*), intent(in) :: label
        call set_figure_labels(state, xlabel=label)
        ! Update backward compatibility member
        xlabel_compat = label
    end subroutine set_xlabel_figure

    subroutine set_ylabel_figure(state, ylabel_compat, label)
        !! Set y-axis label
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: ylabel_compat
        character(len=*), intent(in) :: label
        call set_figure_labels(state, ylabel=label)
        ! Update backward compatibility member
        ylabel_compat = label
    end subroutine set_ylabel_figure

    subroutine set_title_figure(state, title_compat, title)
        !! Set figure title
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: title_compat
        character(len=*), intent(in) :: title
        call set_figure_labels(state, title=title)
        ! Update backward compatibility member
        title_compat = title
    end subroutine set_title_figure

    subroutine set_xscale_figure(state, scale, threshold)
        !! Set x-axis scale type
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        call set_figure_scales(state, xscale=scale, threshold=threshold)
    end subroutine set_xscale_figure

    subroutine set_yscale_figure(state, scale, threshold)
        !! Set y-axis scale type
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        call set_figure_scales(state, yscale=scale, threshold=threshold)
    end subroutine set_yscale_figure

    subroutine set_xlim_figure(state, x_min, x_max)
        !! Set x-axis limits
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_min, x_max
        
        call set_figure_limits(state, x_min=x_min, x_max=x_max)
    end subroutine set_xlim_figure

    subroutine set_ylim_figure(state, y_min, y_max)
        !! Set y-axis limits
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: y_min, y_max
        
        call set_figure_limits(state, y_min=y_min, y_max=y_max)
    end subroutine set_ylim_figure

    subroutine set_line_width_figure(state, width)
        !! Set line width for subsequent plots
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: width
        state%current_line_width = width
    end subroutine set_line_width_figure

    subroutine grid_figure(state, enabled, which, axis, alpha, linestyle)
        !! Enable/disable and configure grid lines
        type(figure_state_t), intent(inout) :: state
        logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha
        
        call configure_grid(state%grid_enabled, state%grid_which, &
                           state%grid_axis, state%grid_alpha, &
                           state%grid_linestyle, enabled, which, axis, alpha, linestyle)
    end subroutine grid_figure

    !!=============================================================================
    !! CORE MODULE DELEGATION PROCEDURES
    !! Simple wrapper procedures for core module delegation pattern
    !!=============================================================================

    subroutine core_set_xlabel(state, xlabel_compat, label)
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: xlabel_compat
        character(len=*), intent(in) :: label
        call set_xlabel_figure(state, xlabel_compat, label)
    end subroutine core_set_xlabel

    subroutine core_set_ylabel(state, ylabel_compat, label)
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: ylabel_compat
        character(len=*), intent(in) :: label
        call set_ylabel_figure(state, ylabel_compat, label)
    end subroutine core_set_ylabel

    subroutine core_set_title(state, title_compat, title)
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: title_compat
        character(len=*), intent(in) :: title
        call set_title_figure(state, title_compat, title)
    end subroutine core_set_title

    subroutine core_set_xscale(state, scale, threshold)
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call set_xscale_figure(state, scale, threshold)
    end subroutine core_set_xscale

    subroutine core_set_yscale(state, scale, threshold)
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call set_yscale_figure(state, scale, threshold)
    end subroutine core_set_yscale

    subroutine core_set_xlim(state, x_min, x_max)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_min, x_max
        call set_xlim_figure(state, x_min, x_max)
    end subroutine core_set_xlim

    subroutine core_set_ylim(state, y_min, y_max)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: y_min, y_max
        call set_ylim_figure(state, y_min, y_max)
    end subroutine core_set_ylim

    subroutine core_set_line_width(state, width)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: width
        call set_line_width_figure(state, width)
    end subroutine core_set_line_width

    subroutine core_grid(state, enabled, which, axis, alpha, linestyle)
        type(figure_state_t), intent(inout) :: state
        logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha
        call grid_figure(state, enabled, which, axis, alpha, linestyle)
    end subroutine core_grid

end module fortplot_figure_core_config
! ==== End: src/figures/core/fortplot_figure_core_config.f90 ====

! ==== Begin: src/figures/core/fortplot_figure_core_compat.f90 ====

module fortplot_figure_core_compat
    !! Figure backward compatibility and animation support module
    !! 
    !! This module contains backward compatibility and animation methods
    !! extracted from fortplot_figure_core for architectural compliance
    !!
    !! ARCHITECTURAL REFACTORING (Issue #678):
    !! - Focused module for backward compatibility operations
    !! - Single Responsibility Principle compliance
    !! - Clean separation from core plotting functionality

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_compatibility
    use fortplot_figure_accessors
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_core_io, only: render_figure_impl
    implicit none

    private
    public :: get_width_figure, get_height_figure, get_rendered_figure, set_rendered_figure
    public :: get_plot_count_figure, setup_png_backend_for_animation_figure
    public :: extract_rgb_data_for_animation_figure, extract_png_data_for_animation_figure
    public :: backend_color_figure, backend_associated_figure, backend_line_figure
    public :: get_x_min_figure, get_x_max_figure, get_y_min_figure, get_y_max_figure

contains

    function get_width_figure(state) result(width)
        !! Get figure width
        type(figure_state_t), intent(in) :: state
        integer :: width
        width = get_figure_width_compat(state)
    end function get_width_figure
    
    function get_height_figure(state) result(height)
        !! Get figure height
        type(figure_state_t), intent(in) :: state
        integer :: height
        height = get_figure_height_compat(state)
    end function get_height_figure
    
    function get_rendered_figure(state) result(rendered)
        !! Get rendered state
        type(figure_state_t), intent(in) :: state
        logical :: rendered
        rendered = get_figure_rendered_compat(state)
    end function get_rendered_figure
    
    subroutine set_rendered_figure(state, rendered)
        !! Set rendered state
        type(figure_state_t), intent(inout) :: state
        logical, intent(in) :: rendered
        call set_figure_rendered_compat(state, rendered)
    end subroutine set_rendered_figure
    
    function get_plot_count_figure(state) result(plot_count)
        !! Get number of plots
        type(figure_state_t), intent(in) :: state
        integer :: plot_count
        plot_count = get_figure_plot_count_compat(state)
    end function get_plot_count_figure
    
    subroutine setup_png_backend_for_animation_figure(state)
        !! Setup PNG backend for animation (temporary method)
        type(figure_state_t), intent(inout) :: state
        call setup_png_backend_for_animation_compat(state)
    end subroutine setup_png_backend_for_animation_figure
    
    subroutine extract_rgb_data_for_animation_figure(state, plots, plot_count, rgb_data)
        !! Extract RGB data for animation
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        real(wp), intent(out) :: rgb_data(:,:,:)
        
        
        if (.not. state%rendered) then
            call render_figure_impl(state, plots, plot_count)
        end if
        
        call extract_rgb_data_for_animation_compat(state, rgb_data)
    end subroutine extract_rgb_data_for_animation_figure
    
    subroutine extract_png_data_for_animation_figure(state, plots, plot_count, png_data, status)
        !! Extract PNG data for animation
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        
        if (.not. state%rendered) then
            call render_figure_impl(state, plots, plot_count)
        end if
        
        call extract_png_data_for_animation_compat(state, png_data, status)
    end subroutine extract_png_data_for_animation_figure
    
    subroutine backend_color_figure(state, r, g, b)
        !! Set backend color
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: r, g, b
        call backend_color_compat(state, r, g, b)
    end subroutine backend_color_figure
    
    function backend_associated_figure(state) result(is_associated)
        !! Check if backend is allocated
        type(figure_state_t), intent(in) :: state
        logical :: is_associated
        is_associated = backend_associated_compat(state)
    end function backend_associated_figure
    
    subroutine backend_line_figure(state, x1, y1, x2, y2)
        !! Draw line using backend
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x1, y1, x2, y2
        call backend_line_compat(state, x1, y1, x2, y2)
    end subroutine backend_line_figure
    
    function get_x_min_figure(state) result(x_min)
        !! Get x minimum value
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_min
        x_min = get_figure_x_min_compat(state)
    end function get_x_min_figure
    
    function get_x_max_figure(state) result(x_max)
        !! Get x maximum value
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_max
        x_max = get_figure_x_max_compat(state)
    end function get_x_max_figure
    
    function get_y_min_figure(state) result(y_min)
        !! Get y minimum value
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_min
        y_min = get_figure_y_min_compat(state)
    end function get_y_min_figure
    
    function get_y_max_figure(state) result(y_max)
        !! Get y maximum value
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_max
        y_max = get_figure_y_max_compat(state)
    end function get_y_max_figure

end module fortplot_figure_core_compat
! ==== End: src/figures/core/fortplot_figure_core_compat.f90 ====
