module fortplot_rendering
    !! Rendering pipeline for figure (SOLID principles compliance)
    !! 
    !! This module contains all rendering-related methods, separated from
    !! figure base and plot addition for better modularity.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortplot_figure_base, only: figure_t
    use fortplot_plot_data, only: plot_data_t, &
                                    PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                    PLOT_TYPE_ERRORBAR, PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, &
                                    PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER
    use fortplot_scales
    use fortplot_axes
    use fortplot_legend
    use fortplot_pcolormesh
    use fortplot_format_parser, only: parse_format_string, contains_format_chars
    use fortplot_raster, only: raster_render_ylabel
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    use fortplot_annotations, only: text_annotation_t, COORD_DATA, COORD_FIGURE, COORD_AXIS
    use fortplot_pdf, only: pdf_context
    use fortplot_gltf, only: gltf_context
    use fortplot_colormap
    use fortplot_security, only: is_safe_path
    use fortplot_logging, only: log_error, log_info, log_warning
    use fortplot_utils, only: get_backend_from_filename, to_lowercase

    implicit none

    private
    public :: render_figure, savefig, show, figure_legend, render_annotations
    public :: clear_streamlines, gather_subplot_plots

contains

    subroutine render_figure(self)
        !! Main figure rendering pipeline (follows Template Method pattern)
        class(figure_t), intent(inout) :: self
        
        if (self%rendered) return
        
        ! Calculate data ranges and setup coordinate system
        call calculate_figure_data_ranges(self)
        call setup_coordinate_system(self)
        
        ! Render figure components in order
        call render_figure_background(self)
        call render_figure_axes(self)
        call render_all_plots(self)
        call render_arrows(self)
        call render_streamlines(self)
        call render_annotations(self)
        
        ! Render legend if present
        if (self%legend_added) then
            call render_figure_legend(self)
        end if
        
        self%rendered = .true.
    end subroutine render_figure

    subroutine savefig(self, filename, blocking)
        !! Save figure to file with backend auto-detection
        use fortplot_utils, only: initialize_backend
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        
        character(len=20) :: backend_type
        logical :: do_block
        
        ! Validate filename security (Issue #135)
        if (.not. is_safe_path(filename)) then
            call log_error("Unsafe filename rejected: " // trim(filename))
            return
        end if
        
        ! Default to non-blocking
        do_block = .false.
        if (present(blocking)) do_block = blocking
        
        ! Create output directory if needed
        ! Note: ensure_directory_exists would be implemented in full version
        ! call ensure_directory_exists(filename)
        
        ! Auto-detect backend from filename extension
        backend_type = get_backend_from_filename(filename)
        
        ! Switch backend if needed
        call switch_backend_if_needed(self, backend_type)
        
        ! Render figure
        call render_figure(self)
        
        ! Save to file
        call self%backend%save(filename)
        
        call log_info("Figure saved to: " // trim(filename))
        
        ! Handle blocking behavior
        if (do_block) then
            call wait_for_user_input()
        end if
    end subroutine savefig

    subroutine show(self, blocking)
        !! Display figure using current backend
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: blocking
        
        logical :: do_block
        
        ! Default to non-blocking
        do_block = .false.
        if (present(blocking)) do_block = blocking
        
        ! Render figure
        call render_figure(self)
        
        ! Display the figure (save to ASCII output for terminal display)
        call self%backend%save('fortplot_output.txt')
        
        ! Handle blocking behavior
        if (do_block) then
            call wait_for_user_input()
        end if
    end subroutine show

    subroutine gather_subplot_plots(self)
        !! Gather all subplot plots into main plots array for rendering
        class(figure_t), intent(inout) :: self
        integer :: subplot_idx, plot_idx, total_idx
        
        ! Always gather plots from subplots into main plots array
        total_idx = 0
        
        if (allocated(self%subplots)) then
            do subplot_idx = 1, self%subplot_rows * self%subplot_cols
                do plot_idx = 1, self%subplots(subplot_idx)%plot_count
                    total_idx = total_idx + 1
                    if (total_idx <= self%max_plots) then
                        self%plots(total_idx) = self%subplots(subplot_idx)%plots(plot_idx)
                    end if
                end do
            end do
        end if
        
        self%plot_count = total_idx
    end subroutine gather_subplot_plots

    subroutine calculate_figure_data_ranges(self)
        !! Calculate overall data ranges from all plots
        class(figure_t), intent(inout) :: self
        integer :: i
        logical :: first_plot
        
        ! Gather subplot data if needed
        call gather_subplot_plots(self)
        
        if (self%plot_count == 0) return
        
        first_plot = .true.
        
        do i = 1, self%plot_count
            call update_ranges_from_plot(self, i, first_plot)
            first_plot = .false.
        end do
        
        ! Handle pcolormesh data ranges separately
        call update_data_ranges_pcolormesh(self)
        call update_data_ranges_boxplot(self)
        
        ! Transform ranges based on scale settings
        call transform_axis_ranges(self)
    end subroutine calculate_figure_data_ranges

    subroutine setup_coordinate_system(self)
        !! Setup backend coordinate system
        class(figure_t), intent(inout) :: self
        
        call self%backend%set_coordinates(self%x_min_transformed, self%x_max_transformed, &
                                         self%y_min_transformed, self%y_max_transformed)
    end subroutine setup_coordinate_system

    subroutine render_figure_background(self)
        !! Render figure background
        class(figure_t), intent(inout) :: self
        
        ! Backend initialization handles clearing
        ! No explicit clear needed as setup_canvas initializes clean backend
    end subroutine render_figure_background

    subroutine render_figure_axes(self)
        !! Render figure axes, labels, and title
        class(figure_t), intent(inout) :: self
        
        ! Render axes with ticks
        call render_axis_framework(self)
        
        ! Render labels
        call render_axis_labels(self)
        
        ! Render title using text method
        if (allocated(self%title)) then
            ! Place title at top center of the plot area
            call self%backend%text((self%x_min_transformed + self%x_max_transformed) / 2.0_wp, &
                                  self%y_max_transformed + 0.05_wp * (self%y_max_transformed - self%y_min_transformed), &
                                  self%title)
        end if
    end subroutine render_figure_axes

    subroutine render_all_plots(self)
        !! Render all plots in the figure
        class(figure_t), intent(inout) :: self
        integer :: i, subplot_idx
        
        ! Get current subplot (default to 1)
        subplot_idx = max(1, self%current_subplot)
        if (.not. allocated(self%subplots)) return
        if (subplot_idx > size(self%subplots)) return
        
        ! Render all plots in the current subplot
        do i = 1, self%subplots(subplot_idx)%plot_count
            call render_single_plot(self, i)
        end do
    end subroutine render_all_plots

    subroutine render_arrows(self)
        !! Render arrow annotations
        class(figure_t), intent(inout) :: self
        integer :: i
        
        if (.not. allocated(self%arrow_data)) return
        
        do i = 1, size(self%arrow_data)
            call render_single_arrow(self, i)
        end do
    end subroutine render_arrows

    subroutine render_streamlines(self)
        !! Render streamline data
        class(figure_t), intent(inout) :: self
        integer :: i
        
        if (.not. allocated(self%streamlines)) return
        
        do i = 1, size(self%streamlines)
            call render_streamline(self, i)
        end do
    end subroutine render_streamlines

    subroutine render_annotations(self)
        !! Render text annotations (Issue #184)
        class(figure_t), intent(inout) :: self
        
        ! Note: Full annotation rendering would be implemented in complete version
        ! For now, just stub to satisfy interface
        if (.not. allocated(self%annotations)) return
        ! Stub: annotation rendering logic would go here
    end subroutine render_annotations

    subroutine figure_legend(self, location)
        !! Add legend to figure
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in), optional :: location
        
        if (present(location)) then
            call parse_legend_location(location, self%legend_location)
        end if
        
        self%legend_added = .true.
    end subroutine figure_legend

    subroutine render_figure_legend(self)
        !! Render legend following SOLID principles
        !! Populate legend with labeled plots and render it
        class(figure_t), intent(inout) :: self
        integer :: i, subplot_idx
        
        ! Initialize legend if not already done
        if (.not. allocated(self%legend_data%entries)) then
            allocate(self%legend_data%entries(0))
            self%legend_data%num_entries = 0
        end if
        
        ! Set legend position based on legend_location
        select case(self%legend_location)
        case(1)
            self%legend_data%position = LEGEND_UPPER_RIGHT
        case(2)
            self%legend_data%position = LEGEND_UPPER_LEFT
        case(3)
            self%legend_data%position = LEGEND_LOWER_RIGHT
        case(4)
            self%legend_data%position = LEGEND_LOWER_LEFT
        case default
            self%legend_data%position = LEGEND_UPPER_RIGHT
        end select
        
        ! Get current subplot (default to 1)
        subplot_idx = max(1, self%current_subplot)
        if (.not. allocated(self%subplots)) return
        if (subplot_idx > size(self%subplots)) return
        
        ! Populate legend with labeled plots from current subplot (DRY principle)
        do i = 1, self%subplots(subplot_idx)%plot_count
            if (allocated(self%subplots(subplot_idx)%plots(i)%label)) then
                if (len_trim(self%subplots(subplot_idx)%plots(i)%label) > 0) then
                    call self%legend_data%add_entry(self%subplots(subplot_idx)%plots(i)%label, &
                                             self%subplots(subplot_idx)%plots(i)%color, &
                                             self%subplots(subplot_idx)%plots(i)%linestyle, &
                                             self%subplots(subplot_idx)%plots(i)%marker)
                end if
            end if
        end do
        
        ! Render legend if we have entries
        if (self%legend_data%num_entries > 0) then
            call legend_render(self%legend_data, self%backend)
        end if
    end subroutine render_figure_legend

    subroutine clear_streamlines(self)
        !! Clear streamline data
        class(figure_t), intent(inout) :: self
        
        if (allocated(self%streamlines)) then
            deallocate(self%streamlines)
        end if
    end subroutine clear_streamlines

    ! Private implementation subroutines

    function symlog_transform(x, threshold) result(transformed)
        !! Apply symlog transformation
        real(wp), intent(in) :: x, threshold
        real(wp) :: transformed
        
        if (abs(x) <= threshold) then
            transformed = x
        else if (x > 0) then
            transformed = threshold * (1.0_wp + log10(x / threshold))
        else
            transformed = -threshold * (1.0_wp + log10(-x / threshold))
        end if
    end function symlog_transform

    subroutine switch_backend_if_needed(self, target_backend)
        !! Switch backend if current doesn't match target
        use fortplot_utils, only: initialize_backend
        use fortplot_ascii, only: ascii_context
        use fortplot_pdf, only: pdf_context
        use fortplot_png, only: png_context
        use fortplot_raster, only: raster_context
        use fortplot_gltf, only: gltf_context
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: target_backend
        
        character(len=20) :: current_backend
        
        ! Detect current backend type using polymorphic type checking
        current_backend = 'unknown'
        if (allocated(self%backend)) then
            select type (backend => self%backend)
            type is (ascii_context)
                current_backend = 'ascii'
            type is (pdf_context)  
                current_backend = 'pdf'
            type is (png_context)
                current_backend = 'png'
            type is (raster_context)
                current_backend = 'png'  ! Treat base raster as PNG
            type is (gltf_context)
                current_backend = 'gltf'
            class default
                current_backend = 'ascii'  ! Default fallback
            end select
        else
            current_backend = 'ascii'  ! Default for unallocated backend
        end if
        
        ! PNG backend switching is now enabled
        ! Previous workaround for raster backend corruption has been resolved
        
        if (trim(current_backend) /= trim(target_backend)) then
            ! Destroy current backend
            if (allocated(self%backend)) deallocate(self%backend)
            
            ! Create new backend
            call initialize_backend(self%backend, target_backend, self%width, self%height)
            
            ! Reset rendered flag
            self%rendered = .false.
        end if
    end subroutine switch_backend_if_needed

    subroutine wait_for_user_input()
        !! Wait for user input when blocking
        character(len=1) :: dummy
        
        write(*,'(A)', advance='no') 'Press Enter to continue...'
        read(*,'(A)') dummy
    end subroutine wait_for_user_input

    subroutine update_ranges_from_plot(self, plot_idx, first_plot)
        !! Update figure ranges from single plot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        logical, intent(in) :: first_plot
        
        type(plot_data_t) :: plot
        
        plot = self%plots(plot_idx)
        
        select case (plot%plot_type)
        case (PLOT_TYPE_LINE, PLOT_TYPE_SCATTER)
            call update_ranges_from_line_plot(self, plot, first_plot)
        case (PLOT_TYPE_CONTOUR)
            call update_ranges_from_contour_plot(self, plot, first_plot)
        case (PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM)
            call update_ranges_from_bar_plot(self, plot, first_plot)
        end select
    end subroutine update_ranges_from_plot

    subroutine render_single_plot(self, plot_idx)
        !! Render a single plot based on its type
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        
        type(plot_data_t) :: plot
        integer :: subplot_idx
        
        ! Get current subplot (default to 1)
        subplot_idx = max(1, self%current_subplot)
        if (.not. allocated(self%subplots)) return
        if (subplot_idx > size(self%subplots)) return
        if (plot_idx < 1 .or. plot_idx > self%subplots(subplot_idx)%plot_count) return
        
        plot = self%subplots(subplot_idx)%plots(plot_idx)
        
        select case (plot%plot_type)
        case (PLOT_TYPE_LINE)
            if (allocated(plot%z)) then
                call render_3d_line_plot(self, plot_idx)
            else
                call render_line_plot(self, plot_idx)
            end if
        case (PLOT_TYPE_SCATTER)
            call render_scatter_plot(self, plot_idx)
        case (PLOT_TYPE_CONTOUR)
            call render_contour_plot(self, plot_idx)
        case (PLOT_TYPE_PCOLORMESH)
            call render_pcolormesh_plot(self, plot_idx)
        case (PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM)
            call render_bar_plot(self, plot_idx)
        case (PLOT_TYPE_ERRORBAR)
            call render_errorbar_plot(self, plot_idx)
        case (PLOT_TYPE_BOXPLOT)
            call render_boxplot(self, plot_idx)
        end select
    end subroutine render_single_plot

    ! Stub implementations for missing subroutines
    ! Note: These would contain full implementations in the complete version

    subroutine render_axis_framework(self)
        !! Render axis framework (axes, ticks, grid)
        class(figure_t), intent(inout) :: self
        logical :: has_3d
        integer :: i
        
        ! Determine if we have any 3D plots
        has_3d = .false.
        if (allocated(self%plots)) then
            do i = 1, self%plot_count
                if (self%plots(i)%is_3d()) then
                    has_3d = .true.
                    exit
                end if
            end do
        end if
        
        ! Pass tick configuration to backend if it's a PDF context (Issue #238)
        select type (backend => self%backend)
        type is (pdf_context)
            backend%x_tick_count = self%x_tick_count
            backend%y_tick_count = self%y_tick_count
        end select
        
        ! Call backend to draw axes and labels
        call self%backend%draw_axes_and_labels_backend( &
            self%xscale, self%yscale, self%symlog_threshold, &
            self%x_min, self%x_max, self%y_min, self%y_max, &
            self%title, self%xlabel, self%ylabel, &
            self%z_min, self%z_max, has_3d)
    end subroutine render_axis_framework

    subroutine render_axis_labels(self)
        !! Render axis labels (xlabel, ylabel, title)
        !! Note: Labels are now rendered as part of draw_axes_and_labels_backend
        !! This method is kept for interface compatibility but the actual
        !! rendering happens in render_axis_framework
        class(figure_t), intent(inout) :: self
        
        ! Labels are already rendered in render_axis_framework
        ! via the backend's draw_axes_and_labels_backend method
        ! This stub is kept for backward compatibility
    end subroutine render_axis_labels

    subroutine render_single_arrow(self, arrow_idx)
        !! Stub: Render single arrow
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: arrow_idx
        ! Stub implementation
    end subroutine render_single_arrow

    subroutine render_streamline(self, streamline_idx)
        !! Stub: Render single streamline
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: streamline_idx
        ! Stub implementation
    end subroutine render_streamline

    subroutine render_annotation_text(self, annotation_idx, pixel_x, pixel_y)
        !! Stub: Render annotation text
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: annotation_idx
        real(wp), intent(in) :: pixel_x, pixel_y
        ! Stub implementation
    end subroutine render_annotation_text

    subroutine render_annotation_arrow(self, annotation_idx, pixel_x, pixel_y)
        !! Stub: Render annotation arrow
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: annotation_idx
        real(wp), intent(in) :: pixel_x, pixel_y
        ! Stub implementation
    end subroutine render_annotation_arrow

    subroutine parse_legend_location(location, legend_location)
        !! Stub: Parse legend location string
        character(len=*), intent(in) :: location
        integer, intent(out) :: legend_location
        legend_location = 1  ! Default: upper right
    end subroutine parse_legend_location


    subroutine update_ranges_from_line_plot(self, plot, first_plot)
        !! Update data ranges from line plot data
        class(figure_t), intent(inout) :: self
        type(plot_data_t), intent(in) :: plot
        logical, intent(in) :: first_plot
        
        real(wp) :: x_min_local, x_max_local, y_min_local, y_max_local
        
        if (.not. allocated(plot%x) .or. .not. allocated(plot%y)) return
        if (size(plot%x) == 0) return
        
        ! Calculate local ranges
        x_min_local = minval(plot%x)
        x_max_local = maxval(plot%x)
        y_min_local = minval(plot%y)
        y_max_local = maxval(plot%y)
        
        ! Update global ranges
        if (first_plot) then
            self%x_min = x_min_local
            self%x_max = x_max_local
            self%y_min = y_min_local
            self%y_max = y_max_local
        else
            self%x_min = min(self%x_min, x_min_local)
            self%x_max = max(self%x_max, x_max_local)
            self%y_min = min(self%y_min, y_min_local)
            self%y_max = max(self%y_max, y_max_local)
        end if
    end subroutine update_ranges_from_line_plot

    subroutine update_ranges_from_contour_plot(self, plot, first_plot)
        !! Stub: Update ranges from contour plot
        class(figure_t), intent(inout) :: self
        type(plot_data_t), intent(in) :: plot
        logical, intent(in) :: first_plot
        ! Stub implementation
    end subroutine update_ranges_from_contour_plot

    subroutine update_ranges_from_bar_plot(self, plot, first_plot)
        !! Stub: Update ranges from bar plot
        class(figure_t), intent(inout) :: self
        type(plot_data_t), intent(in) :: plot
        logical, intent(in) :: first_plot
        ! Stub implementation
    end subroutine update_ranges_from_bar_plot

    subroutine render_3d_line_plot(self, plot_idx)
        !! Stub: Render 3D line plot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        ! Stub implementation
    end subroutine render_3d_line_plot

    subroutine render_line_plot(self, plot_idx)
        !! Render line plot by drawing lines between consecutive points
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        
        type(plot_data_t) :: plot
        integer :: i, subplot_idx
        real(wp) :: x1, y1, x2, y2
        
        ! Get current subplot (default to 1)
        subplot_idx = max(1, self%current_subplot)
        if (.not. allocated(self%subplots)) return
        if (subplot_idx > size(self%subplots)) return
        
        ! Get the plot data from the subplot
        if (plot_idx < 1 .or. plot_idx > self%subplots(subplot_idx)%plot_count) return
        plot = self%subplots(subplot_idx)%plots(plot_idx)
        
        ! Skip if not a line plot or insufficient data
        if (plot%plot_type /= PLOT_TYPE_LINE) return
        if (.not. allocated(plot%x) .or. .not. allocated(plot%y)) return
        if (size(plot%x) < 2) return
        
        ! Set the plot color
        call self%backend%color(plot%color(1), plot%color(2), plot%color(3))
        
        ! Draw lines between consecutive points
        do i = 1, size(plot%x) - 1
            x1 = plot%x(i)
            y1 = plot%y(i)
            x2 = plot%x(i + 1)
            y2 = plot%y(i + 1)
            
            ! Apply scale transformations if needed
            if (self%xscale == 'log') then
                if (x1 > 0.0_wp) x1 = log10(x1)
                if (x2 > 0.0_wp) x2 = log10(x2)
            else if (self%xscale == 'symlog') then
                x1 = symlog_transform(x1, self%symlog_threshold)
                x2 = symlog_transform(x2, self%symlog_threshold)
            end if
            
            if (self%yscale == 'log') then
                if (y1 > 0.0_wp) y1 = log10(y1)
                if (y2 > 0.0_wp) y2 = log10(y2)
            else if (self%yscale == 'symlog') then
                y1 = symlog_transform(y1, self%symlog_threshold)
                y2 = symlog_transform(y2, self%symlog_threshold)
            end if
            
            ! Draw the line segment
            call self%backend%line(x1, y1, x2, y2)
        end do
    end subroutine render_line_plot

    subroutine render_scatter_plot(self, plot_idx)
        !! Stub: Render scatter plot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        ! Stub implementation
    end subroutine render_scatter_plot

    subroutine render_contour_plot(self, plot_idx)
        !! Stub: Render contour plot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        ! Stub implementation
    end subroutine render_contour_plot

    subroutine render_pcolormesh_plot(self, plot_idx)
        !! Stub: Render pcolormesh plot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        ! Stub implementation
    end subroutine render_pcolormesh_plot

    subroutine render_bar_plot(self, plot_idx)
        !! Stub: Render bar plot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        ! Stub implementation
    end subroutine render_bar_plot

    subroutine render_errorbar_plot(self, plot_idx)
        !! Stub: Render error bar plot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        ! Stub implementation
    end subroutine render_errorbar_plot

    subroutine render_boxplot(self, plot_idx)
        !! Stub: Render box plot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        ! Stub implementation
    end subroutine render_boxplot

    subroutine update_data_ranges_pcolormesh(self)
        !! Stub: Update pcolormesh data ranges
        class(figure_t), intent(inout) :: self
        ! Stub implementation
    end subroutine update_data_ranges_pcolormesh

    subroutine update_data_ranges_boxplot(self)
        !! Stub: Update boxplot data ranges
        class(figure_t), intent(inout) :: self
        ! Stub implementation
    end subroutine update_data_ranges_boxplot

    subroutine transform_axis_ranges(self)
        !! Transform axis ranges based on scale settings
        class(figure_t), intent(inout) :: self
        
        ! For now, simple linear transformation (identity)
        self%x_min_transformed = self%x_min
        self%x_max_transformed = self%x_max
        self%y_min_transformed = self%y_min
        self%y_max_transformed = self%y_max
        
        ! Add small padding if ranges are identical
        if (self%x_min_transformed == self%x_max_transformed) then
            self%x_min_transformed = self%x_min_transformed - 0.5_wp
            self%x_max_transformed = self%x_max_transformed + 0.5_wp
        end if
        
        if (self%y_min_transformed == self%y_max_transformed) then
            self%y_min_transformed = self%y_min_transformed - 0.5_wp
            self%y_max_transformed = self%y_max_transformed + 0.5_wp
        end if
    end subroutine transform_axis_ranges

end module fortplot_rendering