module fortplot_plotting
    !! Plot addition methods for figure (SOLID principles compliance)
    !! 
    !! This module contains all the add_* methods for different plot types,
    !! separated from figure base for better modularity and maintainability.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, &
                                    PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                    PLOT_TYPE_ERRORBAR, PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, &
                                    PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER, &
                                    HALF_WIDTH, IQR_WHISKER_MULTIPLIER
    use fortplot_colors, only: parse_color, color_t
    use fortplot_annotations, only: text_annotation_t, COORD_DATA, COORD_FIGURE, COORD_AXIS
    use fortplot_streamplot_matplotlib
    use fortplot_logging, only: log_warning, log_error, log_info
    use fortplot_errors, only: fortplot_error_t, SUCCESS, ERROR_RESOURCE_LIMIT
    use fortplot_format_parser, only: parse_format_string
    use fortplot_coordinate_validation, only: validate_coordinate_arrays, coordinate_validation_result_t

    implicit none

    private
    public :: add_plot, add_3d_plot, add_scatter_2d, add_scatter_3d, add_surface
    public :: add_contour, add_contour_filled, add_pcolormesh, bar, barh, hist, boxplot
    public :: streamplot, streamplot_impl, errorbar, add_text_annotation, add_arrow_annotation

    ! Histogram constants
    integer, parameter :: DEFAULT_HISTOGRAM_BINS = 10
    integer, parameter :: MAX_SAFE_BINS = 10000
    real(wp), parameter :: IDENTICAL_VALUE_PADDING = 0.5_wp
    real(wp), parameter :: BIN_EDGE_PADDING_FACTOR = 0.001_wp
    
    ! Box plot constants
    real(wp), parameter :: BOX_PLOT_LINE_WIDTH = 2.0_wp

    interface add_plot
        module procedure add_plot_impl
    end interface

    interface add_scatter_2d
        module procedure add_scatter_2d_impl
    end interface

    interface add_scatter_3d
        module procedure add_scatter_3d_impl
    end interface

    interface bar
        module procedure bar_impl
    end interface

    interface barh
        module procedure barh_impl
    end interface

    interface hist
        module procedure hist_impl
    end interface

    interface boxplot
        module procedure boxplot_impl
    end interface

    interface streamplot
        module procedure streamplot_impl
    end interface

    interface errorbar
        module procedure errorbar_impl
    end interface

contains

    subroutine add_plot_impl(self, x, y, label, linestyle, color_rgb, color_str, marker, markercolor)
        !! Add 2D line plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: linestyle
        real(wp), intent(in), optional :: color_rgb(3)
        character(len=*), intent(in), optional :: color_str
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markercolor(3)
        
        call add_line_plot_data(self, x, y, label, linestyle, color_rgb, color_str, marker)
    end subroutine add_plot_impl

    subroutine add_3d_plot(self, x, y, z, label, linestyle, markersize, linewidth)
        !! Add 3D line plot to figure (projected to 2D)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: linestyle
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: linewidth
        
        call add_3d_line_plot_data(self, x, y, z, label, linestyle, &
                                  marker='', markersize=markersize, linewidth=linewidth)
    end subroutine add_3d_plot

    subroutine add_scatter_2d_impl(self, x, y, s, c, label, marker, markersize, color, &
                                   colormap, vmin, vmax, show_colorbar, alpha)
        !! Add 2D scatter plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:)  ! size array
        real(wp), intent(in), optional :: c(:)  ! color array  
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)  ! single RGB color
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: alpha
        
        call add_scatter_plot_data(self, x, y, s=s, c=c, label=label, &
                                  marker=marker, markersize=markersize, color=color, &
                                  colormap=colormap, vmin=vmin, vmax=vmax, &
                                  show_colorbar=show_colorbar, alpha=alpha)
    end subroutine add_scatter_2d_impl

    subroutine add_scatter_3d_impl(self, x, y, z, s, c, label, marker, markersize, color, &
                                   colormap, vmin, vmax, show_colorbar, alpha)
        !! Add 3D scatter plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:)
        real(wp), intent(in), optional :: s(:)  ! size array
        real(wp), intent(in), optional :: c(:)  ! color array  
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)  ! single RGB color
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: alpha
        
        call add_scatter_plot_data(self, x, y, z, s, c, label, marker, markersize, color, &
                                  colormap, vmin, vmax, show_colorbar, alpha)
    end subroutine add_scatter_3d_impl

    subroutine add_surface(self, x, y, z, label)
        !! Add surface plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:,:)
        character(len=*), intent(in), optional :: label
        
        call add_surface_plot_data(self, x, y, z, label)
    end subroutine add_surface

    subroutine add_contour(self, x_grid, y_grid, z_grid, levels, label)
        !! Add contour plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        call add_contour_plot_data(self, x_grid, y_grid, z_grid, levels, label)
    end subroutine add_contour

    subroutine add_contour_filled(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        !! Add filled contour plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap
        logical, intent(in), optional :: show_colorbar
        character(len=*), intent(in), optional :: label
        
        call add_colored_contour_plot_data(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
    end subroutine add_contour_filled

    subroutine add_pcolormesh(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
        !! Add pseudocolor mesh plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        character(len=*), intent(in), optional :: edgecolors
        real(wp), intent(in), optional :: linewidths
        
        type(fortplot_error_t) :: error
        call add_pcolormesh_plot_data(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths, error)
    end subroutine add_pcolormesh

    subroutine bar_impl(self, x, heights, width, label, color)
        !! Add vertical bar plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: heights(:)
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        real(wp) :: bar_width
        
        bar_width = 0.8_wp
        if (present(width)) bar_width = width
        
        call add_bar_plot_data(self, x, heights, bar_width, label, color, .false.)
    end subroutine bar_impl

    subroutine barh_impl(self, y, widths, height, label, color)
        !! Add horizontal bar plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y(:)
        real(wp), intent(in) :: widths(:)
        real(wp), intent(in), optional :: height
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        real(wp) :: bar_height
        
        bar_height = 0.8_wp
        if (present(height)) bar_height = height
        
        call add_bar_plot_data(self, y, widths, bar_height, label, color, .true.)
    end subroutine barh_impl

    subroutine hist_impl(self, data, bins, density, label, color)
        !! Add histogram plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        integer :: num_bins
        
        num_bins = DEFAULT_HISTOGRAM_BINS
        if (present(bins)) num_bins = bins
        
        if (.not. validate_histogram_input(self, data, num_bins)) return
        
        call add_histogram_plot_data(self, data, num_bins, density, label, color)
    end subroutine hist_impl

    subroutine boxplot_impl(self, data, position, width, label, show_outliers, horizontal, color)
        !! Add box plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        real(wp), intent(in), optional :: color(3)
        
        real(wp) :: pos, box_width
        
        pos = 1.0_wp
        if (present(position)) pos = position
        
        box_width = 0.5_wp
        if (present(width)) box_width = width
        
        call add_boxplot_data(self, data, pos, box_width, label, show_outliers, horizontal, color)
    end subroutine boxplot_impl

    subroutine streamplot_impl(self, x, y, u, v, density, color, linewidth, rtol, atol, max_time, arrowsize, arrowstyle)
        !! Add streamline plot to figure using matplotlib-compatible algorithm with arrow support
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth
        real(wp), intent(in), optional :: rtol        !! Relative tolerance for DOPRI5
        real(wp), intent(in), optional :: atol        !! Absolute tolerance for DOPRI5
        real(wp), intent(in), optional :: max_time    !! Maximum integration time
        real(wp), intent(in), optional :: arrowsize   !! Arrow size scaling factor (default: 1.0)
        character(len=*), intent(in), optional :: arrowstyle !! Arrow style (default: '->')
        
        call setup_streamplot_parameters(self, x, y, u, v, density, color, linewidth, &
                                        rtol, atol, max_time, arrowsize, arrowstyle)
    end subroutine streamplot_impl


    subroutine errorbar_impl(self, x, y, xerr, yerr, xerr_lower, xerr_upper, &
                            yerr_lower, yerr_upper, linestyle, marker, color, label)
        !! Add error bar plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        real(wp), intent(in), optional :: xerr_lower(:), xerr_upper(:)
        real(wp), intent(in), optional :: yerr_lower(:), yerr_upper(:)
        character(len=*), intent(in), optional :: linestyle, marker, label
        real(wp), intent(in), optional :: color(3)
        
        type(plot_data_t) :: plot_data
        integer :: i, color_idx
        
        ! Validate input arrays
        if (size(x) /= size(y)) then
            self%has_error = .true.
            return
        end if
        
        ! Initialize plot data
        plot_data%plot_type = PLOT_TYPE_ERRORBAR
        plot_data%x = x
        plot_data%y = y
        
        ! Set error bars
        if (present(xerr)) plot_data%xerr = xerr
        if (present(yerr)) plot_data%yerr = yerr
        if (present(xerr_lower)) plot_data%xerr_lower = xerr_lower
        if (present(xerr_upper)) plot_data%xerr_upper = xerr_upper
        if (present(yerr_lower)) plot_data%yerr_lower = yerr_lower
        if (present(yerr_upper)) plot_data%yerr_upper = yerr_upper
        
        ! Set style
        plot_data%linestyle = '-'
        if (present(linestyle)) plot_data%linestyle = trim(linestyle)
        
        plot_data%marker = 'o'
        if (present(marker)) plot_data%marker = trim(marker)
        
        ! Only set label if provided and non-empty
        if (present(label)) then
            if (len_trim(label) > 0) then
                plot_data%label = trim(label)
            end if
        end if
        
        ! Set color
        color_idx = mod(self%plot_count, size(self%colors, 2)) + 1
        plot_data%color = self%colors(:, color_idx)
        if (present(color)) plot_data%color = color
        
        ! Add to figure
        call add_plot_to_figure(self, plot_data)
    end subroutine errorbar_impl

    subroutine add_text_annotation(self, x, y, text, coord_type, font_size, rotation, &
                                  color, halign, valign, bbox)
        !! Add text annotation to figure (Issue #184)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        integer, intent(in), optional :: coord_type  ! COORD_DATA, COORD_FIGURE, COORD_AXIS
        real(wp), intent(in), optional :: font_size
        real(wp), intent(in), optional :: rotation
        real(wp), intent(in), optional :: color(3)
        character(len=*), intent(in), optional :: halign
        character(len=*), intent(in), optional :: valign
        logical, intent(in), optional :: bbox
        
        type(text_annotation_t) :: annotation
        
        ! Check annotation limit
        if (self%annotation_count >= self%max_annotations) then
            call log_warning("Maximum annotations reached, ignoring new annotation")
            return
        end if
        
        ! Initialize annotation
        annotation%x = x
        annotation%y = y
        annotation%text = trim(text)
        annotation%coord_type = COORD_DATA
        if (present(coord_type)) annotation%coord_type = coord_type
        
        annotation%font_size = 12.0_wp
        if (present(font_size)) annotation%font_size = font_size
        
        annotation%rotation = 0.0_wp
        if (present(rotation)) annotation%rotation = rotation
        
        
        annotation%alignment = 'left'
        if (present(halign)) annotation%alignment = trim(halign)
        
        ! Note: valign would be handled separately in full implementation
        
        annotation%has_bbox = .false.
        if (present(bbox)) annotation%has_bbox = bbox
        
        ! Add to annotations array
        if (.not. allocated(self%annotations)) then
            allocate(self%annotations(self%max_annotations))
        end if
        
        self%annotation_count = self%annotation_count + 1
        self%annotations(self%annotation_count) = annotation
    end subroutine add_text_annotation

    subroutine add_arrow_annotation(self, text, xy, xytext, xy_coord_type, xytext_coord_type, &
                                   arrowprops, font_size, color, halign, valign)
        !! Add arrow annotation to figure (Issue #184)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: xy(2)  ! [x, y] coordinates of arrow tip
        real(wp), intent(in) :: xytext(2)  ! [x, y] coordinates of text
        integer, intent(in), optional :: xy_coord_type
        integer, intent(in), optional :: xytext_coord_type
        character(len=*), intent(in), optional :: arrowprops
        real(wp), intent(in), optional :: font_size
        real(wp), intent(in), optional :: color(3)
        character(len=*), intent(in), optional :: halign
        character(len=*), intent(in), optional :: valign
        
        type(text_annotation_t) :: annotation
        
        ! Check annotation limit
        if (self%annotation_count >= self%max_annotations) then
            call log_warning("Maximum annotations reached, ignoring new annotation")
            return
        end if
        
        ! Initialize annotation with arrow
        annotation%x = xytext(1)
        annotation%y = xytext(2)
        annotation%text = trim(text)
        annotation%coord_type = COORD_DATA
        if (present(xytext_coord_type)) annotation%coord_type = xytext_coord_type
        
        annotation%font_size = 12.0_wp
        if (present(font_size)) annotation%font_size = font_size
        
        
        annotation%alignment = 'center'
        if (present(halign)) annotation%alignment = trim(halign)
        
        ! Note: valign would be handled separately in full implementation
        
        ! Arrow properties (using available fields)
        annotation%has_arrow = .true.
        annotation%xytext_x = xy(1)
        annotation%xytext_y = xy(2)
        ! Note: arrow coordinate type would be handled in proper implementation
        
        
        ! Add to annotations array
        if (.not. allocated(self%annotations)) then
            allocate(self%annotations(self%max_annotations))
        end if
        
        self%annotation_count = self%annotation_count + 1
        self%annotations(self%annotation_count) = annotation
    end subroutine add_arrow_annotation

    ! Private implementation subroutines

    subroutine add_line_plot_data(self, x, y, label, linestyle, color_rgb, color_str, marker)
        !! Add line plot data to internal storage with comprehensive validation
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle, color_str, marker
        real(wp), intent(in), optional :: color_rgb(3)
        
        integer :: plot_idx, color_idx, subplot_idx
        real(wp) :: rgb(3)
        logical :: success
        type(coordinate_validation_result_t) :: validation
        character(len=64) :: suggested_marker
        
        ! Validate coordinate arrays (Issue #436 fix)
        validation = validate_coordinate_arrays(x, y, "add_line_plot_data")
        if (.not. validation%is_valid) then
            call log_error(trim(validation%message))
            return  ! Skip adding invalid data
        end if
        
        ! For single points, suggest using markers for better visibility
        if (validation%is_single_point .and. .not. present(marker)) then
            call log_warning("Single point detected - consider adding markers for better visibility")
        end if
        
        ! Get current subplot
        subplot_idx = self%current_subplot
        plot_idx = self%subplots(subplot_idx)%plot_count + 1
        self%subplots(subplot_idx)%plot_count = plot_idx
        
        self%subplots(subplot_idx)%plots(plot_idx)%plot_type = PLOT_TYPE_LINE
        
        ! Store data
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%x(size(x)))
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%y(size(y)))
        self%subplots(subplot_idx)%plots(plot_idx)%x = x
        self%subplots(subplot_idx)%plots(plot_idx)%y = y
        
        ! Set properties
        if (present(label)) then
            if (len_trim(label) > 0) then
                self%subplots(subplot_idx)%plots(plot_idx)%label = label
            end if
            ! If label is empty or not provided, leave it unallocated
        end if
        
        ! Parse linestyle to extract marker and line style components
        block
            character(len=20) :: parsed_marker, parsed_linestyle
            
            if (present(linestyle)) then
                call parse_format_string(linestyle, parsed_marker, parsed_linestyle)
                
                if (len_trim(parsed_marker) > 0) then
                    ! Format string contained a marker
                    self%subplots(subplot_idx)%plots(plot_idx)%marker = trim(parsed_marker)
                    if (len_trim(parsed_linestyle) > 0) then
                        self%subplots(subplot_idx)%plots(plot_idx)%linestyle = trim(parsed_linestyle)
                    else
                        self%subplots(subplot_idx)%plots(plot_idx)%linestyle = 'None'  ! No line, marker only
                    end if
                else
                    ! Pure linestyle, no marker
                    self%subplots(subplot_idx)%plots(plot_idx)%linestyle = linestyle
                    if (present(marker)) then
                        self%subplots(subplot_idx)%plots(plot_idx)%marker = marker
                    else
                        self%subplots(subplot_idx)%plots(plot_idx)%marker = 'None'
                    end if
                end if
            else
                self%subplots(subplot_idx)%plots(plot_idx)%linestyle = 'solid'
                if (present(marker)) then
                    self%subplots(subplot_idx)%plots(plot_idx)%marker = marker
                else
                    self%subplots(subplot_idx)%plots(plot_idx)%marker = 'None'
                end if
            end if
        end block
        
        ! Handle color specification
        if (present(color_str)) then
            call parse_color(color_str, rgb, success)
            if (success) then
                self%subplots(subplot_idx)%plots(plot_idx)%color = rgb
            else
                color_idx = mod(plot_idx - 1, 6) + 1
                self%subplots(subplot_idx)%plots(plot_idx)%color = self%colors(:, color_idx)
            end if
        else if (present(color_rgb)) then
            self%subplots(subplot_idx)%plots(plot_idx)%color = color_rgb
        else
            color_idx = mod(plot_idx - 1, 6) + 1
            self%subplots(subplot_idx)%plots(plot_idx)%color = self%colors(:, color_idx)
        end if
        
        ! Update main plot count with warning generation
        self%plot_count = self%plot_count + 1
        if (self%plot_count <= self%max_plots) then
            self%plots(self%plot_count) = self%subplots(subplot_idx)%plots(plot_idx)
        else
            ! Generate warning when max plots exceeded
            call log_warning('Maximum number of plots reached, additional plots may not be rendered properly')
        end if
        
        ! Also check subplot-specific plot limits
        if (self%subplots(subplot_idx)%plot_count > self%subplots(subplot_idx)%max_plots) then
            call log_warning('Subplot plot limit exceeded, performance may be degraded')
        end if
    end subroutine add_line_plot_data

    ! Additional plot implementation subroutines

    subroutine add_3d_line_plot_data(self, x, y, z, label, linestyle, marker, markersize, linewidth)
        !! Add 3D line plot data by projecting to 2D
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(wp), intent(in), optional :: markersize, linewidth
        
        ! Project 3D to 2D for basic plotting (z-axis ignored for now)
        call add_line_plot_data(self, x, y, label, linestyle)
    end subroutine add_3d_line_plot_data

    subroutine add_surface_plot_data(self, x, y, z, label)
        !! Add surface plot data (simplified as contour representation)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:,:)
        character(len=*), intent(in), optional :: label
        
        integer :: subplot_idx, plot_idx
        
        ! Get current subplot
        subplot_idx = self%current_subplot
        plot_idx = self%subplots(subplot_idx)%plot_count + 1
        self%subplots(subplot_idx)%plot_count = plot_idx
        
        self%subplots(subplot_idx)%plots(plot_idx)%plot_type = PLOT_TYPE_CONTOUR
        
        ! Store grid data
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%x_grid(size(x)))
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%y_grid(size(y)))
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%z_grid(size(z, 1), size(z, 2)))
        
        self%subplots(subplot_idx)%plots(plot_idx)%x_grid = x
        self%subplots(subplot_idx)%plots(plot_idx)%y_grid = y
        self%subplots(subplot_idx)%plots(plot_idx)%z_grid = z
        
        if (present(label) .and. len_trim(label) > 0) then
            self%subplots(subplot_idx)%plots(plot_idx)%label = label
        end if
    end subroutine add_surface_plot_data

    subroutine add_contour_plot_data(self, x_grid, y_grid, z_grid, levels, label)
        !! Add contour plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        integer :: subplot_idx, plot_idx
        
        subplot_idx = self%current_subplot
        plot_idx = self%subplots(subplot_idx)%plot_count + 1
        self%subplots(subplot_idx)%plot_count = plot_idx
        
        self%subplots(subplot_idx)%plots(plot_idx)%plot_type = PLOT_TYPE_CONTOUR
        
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%x_grid(size(x_grid)))
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%y_grid(size(y_grid)))
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%z_grid(size(z_grid, 1), size(z_grid, 2)))
        
        self%subplots(subplot_idx)%plots(plot_idx)%x_grid = x_grid
        self%subplots(subplot_idx)%plots(plot_idx)%y_grid = y_grid
        self%subplots(subplot_idx)%plots(plot_idx)%z_grid = z_grid
        
        if (present(levels)) then
            allocate(self%subplots(subplot_idx)%plots(plot_idx)%contour_levels(size(levels)))
            self%subplots(subplot_idx)%plots(plot_idx)%contour_levels = levels
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%subplots(subplot_idx)%plots(plot_idx)%label = label
        end if
    end subroutine add_contour_plot_data

    subroutine add_colored_contour_plot_data(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        !! Add colored contour plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
        integer :: subplot_idx, plot_idx
        
        subplot_idx = self%current_subplot
        plot_idx = self%subplots(subplot_idx)%plot_count + 1
        self%subplots(subplot_idx)%plot_count = plot_idx
        
        self%subplots(subplot_idx)%plots(plot_idx)%plot_type = PLOT_TYPE_CONTOUR
        
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%x_grid(size(x_grid)))
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%y_grid(size(y_grid)))
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%z_grid(size(z_grid, 1), size(z_grid, 2)))
        
        self%subplots(subplot_idx)%plots(plot_idx)%x_grid = x_grid
        self%subplots(subplot_idx)%plots(plot_idx)%y_grid = y_grid
        self%subplots(subplot_idx)%plots(plot_idx)%z_grid = z_grid
        
        if (present(levels)) then
            allocate(self%subplots(subplot_idx)%plots(plot_idx)%contour_levels(size(levels)))
            self%subplots(subplot_idx)%plots(plot_idx)%contour_levels = levels
        end if
        
        if (present(colormap)) then
            self%subplots(subplot_idx)%plots(plot_idx)%colormap = colormap
        end if
        
        if (present(show_colorbar)) then
            self%subplots(subplot_idx)%plots(plot_idx)%show_colorbar = show_colorbar
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%subplots(subplot_idx)%plots(plot_idx)%label = label
        end if
    end subroutine add_colored_contour_plot_data

    subroutine add_pcolormesh_plot_data(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths, error)
        !! Add pcolormesh plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap, edgecolors
        real(wp), intent(in), optional :: vmin, vmax, linewidths
        type(fortplot_error_t), intent(out), optional :: error
        
        integer :: subplot_idx, plot_idx
        
        if (present(error)) then
            error%status = SUCCESS
        end if
        
        subplot_idx = self%current_subplot
        plot_idx = self%subplots(subplot_idx)%plot_count + 1
        self%subplots(subplot_idx)%plot_count = plot_idx
        
        self%subplots(subplot_idx)%plots(plot_idx)%plot_type = PLOT_TYPE_PCOLORMESH
        
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%x(size(x)))
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%y(size(y)))
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%z_grid(size(c, 1), size(c, 2)))
        
        self%subplots(subplot_idx)%plots(plot_idx)%x = x
        self%subplots(subplot_idx)%plots(plot_idx)%y = y
        self%subplots(subplot_idx)%plots(plot_idx)%z_grid = c
        
        if (present(colormap)) then
            self%subplots(subplot_idx)%plots(plot_idx)%colormap = colormap
        end if
        
        if (present(vmin)) then
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_vmin = vmin
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_vrange_set = .true.
        end if
        
        if (present(vmax)) then
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_vmax = vmax
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_vrange_set = .true.
        end if
    end subroutine add_pcolormesh_plot_data

    subroutine add_bar_plot_data(self, positions, values, bar_size, label, color, horizontal)
        !! Add bar plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: positions(:), values(:), bar_size
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        logical, intent(in) :: horizontal
        
        integer :: subplot_idx, plot_idx
        
        subplot_idx = self%current_subplot
        plot_idx = self%subplots(subplot_idx)%plot_count + 1
        self%subplots(subplot_idx)%plot_count = plot_idx
        
        self%subplots(subplot_idx)%plots(plot_idx)%plot_type = PLOT_TYPE_BAR
        
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%x(size(positions)))
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%y(size(values)))
        
        if (horizontal) then
            self%subplots(subplot_idx)%plots(plot_idx)%x = values
            self%subplots(subplot_idx)%plots(plot_idx)%y = positions
        else
            self%subplots(subplot_idx)%plots(plot_idx)%x = positions
            self%subplots(subplot_idx)%plots(plot_idx)%y = values
        end if
        
        self%subplots(subplot_idx)%plots(plot_idx)%bar_width = bar_size
        self%subplots(subplot_idx)%plots(plot_idx)%bar_horizontal = horizontal
        
        if (present(color)) then
            self%subplots(subplot_idx)%plots(plot_idx)%color = color
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%subplots(subplot_idx)%plots(plot_idx)%label = label
        end if
    end subroutine add_bar_plot_data

    function validate_histogram_input(self, data, bins) result(is_valid)
        !! Validate histogram input parameters
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in) :: bins
        logical :: is_valid
        
        is_valid = .true.
        
        if (size(data) == 0) then
            is_valid = .false.
            return
        end if
        
        if (bins <= 0 .or. bins > MAX_SAFE_BINS) then
            is_valid = .false.
            return
        end if
        
        ! Check for finite values
        if (.not. all(ieee_is_finite(data))) then
            is_valid = .false.
            return
        end if
    end function validate_histogram_input

    subroutine add_histogram_plot_data(self, data, bins, density, label, color)
        !! Add histogram plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in) :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        integer :: subplot_idx, plot_idx
        
        if (.not. validate_histogram_input(self, data, bins)) return
        
        subplot_idx = self%current_subplot
        plot_idx = self%subplots(subplot_idx)%plot_count + 1
        self%subplots(subplot_idx)%plot_count = plot_idx
        
        self%subplots(subplot_idx)%plots(plot_idx)%plot_type = PLOT_TYPE_HISTOGRAM
        
        ! Store raw data for histogram computation
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%x(size(data)))
        self%subplots(subplot_idx)%plots(plot_idx)%x = data
        ! Store bins as metadata (would need custom field or use existing structure)
        
        if (present(density)) then
            self%subplots(subplot_idx)%plots(plot_idx)%hist_density = density
        end if
        
        if (present(color)) then
            self%subplots(subplot_idx)%plots(plot_idx)%color = color
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%subplots(subplot_idx)%plots(plot_idx)%label = label
        end if
    end subroutine add_histogram_plot_data

    subroutine add_boxplot_data(self, data, position, width, label, show_outliers, horizontal, color)
        !! Add boxplot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:), position, width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers, horizontal
        real(wp), intent(in), optional :: color(3)
        
        integer :: subplot_idx, plot_idx
        
        subplot_idx = self%current_subplot
        plot_idx = self%subplots(subplot_idx)%plot_count + 1
        self%subplots(subplot_idx)%plot_count = plot_idx
        
        self%subplots(subplot_idx)%plots(plot_idx)%plot_type = PLOT_TYPE_BOXPLOT
        
        ! Store raw data for boxplot computation
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%x(size(data)))
        self%subplots(subplot_idx)%plots(plot_idx)%x = data
        self%subplots(subplot_idx)%plots(plot_idx)%position = position
        self%subplots(subplot_idx)%plots(plot_idx)%width = width
        
        if (present(show_outliers)) then
            self%subplots(subplot_idx)%plots(plot_idx)%show_outliers = show_outliers
        end if
        
        if (present(horizontal)) then
            self%subplots(subplot_idx)%plots(plot_idx)%horizontal = horizontal
        end if
        
        if (present(color)) then
            self%subplots(subplot_idx)%plots(plot_idx)%color = color
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%subplots(subplot_idx)%plots(plot_idx)%label = label
        end if
    end subroutine add_boxplot_data

    subroutine add_scatter_plot_data(self, x, y, z, s, c, label, marker, markersize, color, &
                                    colormap, vmin, vmax, show_colorbar, alpha)
        !! Add scatter plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: z(:), s(:), c(:), markersize, color(3), vmin, vmax, alpha
        character(len=*), intent(in), optional :: label, marker, colormap
        logical, intent(in), optional :: show_colorbar
        
        integer :: subplot_idx, plot_idx
        
        subplot_idx = self%current_subplot
        plot_idx = self%subplots(subplot_idx)%plot_count + 1
        self%subplots(subplot_idx)%plot_count = plot_idx
        
        self%subplots(subplot_idx)%plots(plot_idx)%plot_type = PLOT_TYPE_SCATTER
        
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%x(size(x)))
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%y(size(y)))
        
        self%subplots(subplot_idx)%plots(plot_idx)%x = x
        self%subplots(subplot_idx)%plots(plot_idx)%y = y
        
        if (present(z)) then
            allocate(self%subplots(subplot_idx)%plots(plot_idx)%z(size(z)))
            self%subplots(subplot_idx)%plots(plot_idx)%z = z
        end if
        
        if (present(s)) then
            allocate(self%subplots(subplot_idx)%plots(plot_idx)%scatter_sizes(size(s)))
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_sizes = s
        end if
        
        if (present(c)) then
            allocate(self%subplots(subplot_idx)%plots(plot_idx)%scatter_colors(size(c)))
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_colors = c
        end if
        
        if (present(color)) then
            self%subplots(subplot_idx)%plots(plot_idx)%color = color
        end if
        
        if (present(marker)) then
            self%subplots(subplot_idx)%plots(plot_idx)%marker = marker
        end if
        
        if (present(markersize)) then
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_size_default = markersize
        end if
        
        if (present(colormap)) then
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_colormap = colormap
        end if
        
        if (present(vmin)) then
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_vmin = vmin
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_vrange_set = .true.
        end if
        
        if (present(vmax)) then
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_vmax = vmax
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_vrange_set = .true.
        end if
        
        if (present(show_colorbar)) then
            self%subplots(subplot_idx)%plots(plot_idx)%scatter_colorbar = show_colorbar
        end if
        
        ! Note: alpha not directly supported in plot_data_t structure
        
        if (present(label) .and. len_trim(label) > 0) then
            self%subplots(subplot_idx)%plots(plot_idx)%label = label
        end if
    end subroutine add_scatter_plot_data

    subroutine add_plot_to_figure(self, plot_data)
        !! Add plot data to figure
        class(figure_t), intent(inout) :: self
        type(plot_data_t), intent(in) :: plot_data
        
        integer :: subplot_idx, plot_idx
        
        subplot_idx = self%current_subplot
        plot_idx = self%subplots(subplot_idx)%plot_count + 1
        self%subplots(subplot_idx)%plot_count = plot_idx
        
        ! Copy plot data to subplot
        self%subplots(subplot_idx)%plots(plot_idx) = plot_data
    end subroutine add_plot_to_figure

    subroutine setup_streamplot_parameters(self, x, y, u, v, density, color, linewidth, &
                                          rtol, atol, max_time, arrowsize, arrowstyle)
        !! Delegate to streamplot core module
        use fortplot_streamplot_core, only: streamplot_core_setup => setup_streamplot_parameters
        
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density, linewidth, rtol, atol, max_time, arrowsize
        real(wp), intent(in), optional :: color(3)
        character(len=*), intent(in), optional :: arrowstyle
        
        ! Delegate to streamplot core implementation
        call streamplot_core_setup(self, x, y, u, v, density, color, linewidth, &
                                  rtol, atol, max_time, arrowsize, arrowstyle)
    end subroutine setup_streamplot_parameters

end module fortplot_plotting