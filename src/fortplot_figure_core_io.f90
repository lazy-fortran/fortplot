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
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges, &
                                                    setup_coordinate_system, &
                                                    render_figure_background, &
                                                    render_figure_axes, &
                                                    render_all_plots
    use fortplot_figure_grid, only: render_grid_lines
    use fortplot_annotation_rendering, only: render_figure_annotations
    use fortplot_figure_io, only: save_backend_with_status
    use fortplot_figure_utilities, only: is_interactive_environment, wait_for_user_input
    use fortplot_plot_data, only: plot_data_t
    implicit none

    private
    public :: savefig_figure, savefig_with_status_figure, show_figure, render_figure_impl

contains

    subroutine savefig_figure(state, plots, plot_count, filename, blocking, annotations, annotation_count)
        !! Save figure to file (backward compatibility version)
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        
        integer :: status
        
        ! Delegate to version with status reporting
        call savefig_with_status_figure(state, plots, plot_count, filename, status, blocking, &
                                        annotations, annotation_count)
        
        ! Log error if save failed (maintains existing behavior)
        if (status /= SUCCESS) then
            call log_error("Failed to save figure to '" // trim(filename) // "'")
        end if
    end subroutine savefig_figure
    
    subroutine savefig_with_status_figure(state, plots, plot_count, filename, status, blocking, &
                                          annotations, annotation_count)
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
            call render_figure_impl(state, plots, plot_count, annotations, annotation_count)
        end if
        
        ! Save the figure with status checking
        call save_backend_with_status(state%backend, filename, status)
    end subroutine savefig_with_status_figure

    subroutine show_figure(state, plots, plot_count, blocking, annotations, annotation_count)
        !! Display the figure
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in), optional :: blocking
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        
        logical :: block
        
        ! Default to non-blocking behavior to prevent hangs in automated environments
        ! Users can explicitly set blocking=true for interactive sessions
        block = .false.
        if (present(blocking)) block = blocking
        
        ! Render if not already rendered (with annotations if provided)
        if (.not. state%rendered) then
            call render_figure_impl(state, plots, plot_count, annotations, annotation_count)
        end if
        
        ! Display the figure
        call state%backend%save("terminal")
        
        ! Handle blocking behavior - when blocking=true, wait for user input
        if (block) then
            call wait_for_user_input()
        end if
    end subroutine show_figure

    subroutine render_figure_impl(state, plots, plot_count, annotations, annotation_count)
        !! Main rendering pipeline implementation
        !! Fixed Issue #432: Always render axes/labels even with no plot data
        !! Fixed Issue #844: ASCII annotation functionality
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        
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
        
        ! Setup coordinate system
        call setup_coordinate_system(state%backend, &
                                   state%x_min_transformed, state%x_max_transformed, &
                                   state%y_min_transformed, state%y_max_transformed)
        
        ! Render background
        call render_figure_background(state%backend)
        
        ! Render grid if enabled
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
                                  state%y_min_transformed, state%y_max_transformed)
        end if
        
        ! Render axes
        call render_figure_axes(state%backend, state%xscale, state%yscale, &
                               state%symlog_threshold, state%x_min, state%x_max, &
                               state%y_min, state%y_max, state%title, &
                               state%xlabel, state%ylabel)
        
        ! Render all plots (only if there are plots to render)
        if (plot_count > 0) then
            call render_all_plots(state%backend, plots, plot_count, &
                                 state%x_min_transformed, state%x_max_transformed, &
                                 state%y_min_transformed, state%y_max_transformed, &
                                 state%xscale, state%yscale, state%symlog_threshold, &
                                 state%width, state%height, &
                                 state%margin_left, state%margin_right, &
                                 state%margin_bottom, state%margin_top)
        end if
        
        ! Render legend if requested
        if (state%show_legend .and. state%legend_data%num_entries > 0) then
            call state%legend_data%render(state%backend)
        end if
        
        ! Render annotations if any exist (Issue #844: ASCII annotation functionality)
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
        
        state%rendered = .true.
    end subroutine render_figure_impl

end module fortplot_figure_core_io