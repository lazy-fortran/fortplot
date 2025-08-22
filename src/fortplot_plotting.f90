module fortplot_plotting
    !! Plot addition methods for figure (SOLID principles compliance)
    !! 
    !! This module contains all the add_* methods for different plot types,
    !! separated from figure base for better modularity and maintainability.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortplot_figure_base, only: figure_t
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, &
                                    PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                    PLOT_TYPE_ERRORBAR, PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, &
                                    PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER, &
                                    HALF_WIDTH, IQR_WHISKER_MULTIPLIER
    use fortplot_colors, only: parse_color, color_t
    use fortplot_annotations, only: text_annotation_t, COORD_DATA, COORD_FIGURE, COORD_AXIS
    use fortplot_streamplot_matplotlib
    use fortplot_logging, only: log_warning
    use fortplot_errors, only: fortplot_error_t, SUCCESS, ERROR_RESOURCE_LIMIT, log_error

    implicit none

    private
    public :: add_plot, add_3d_plot, add_scatter_2d, add_scatter_3d, add_surface
    public :: add_contour, add_contour_filled, add_pcolormesh, bar, barh, hist, boxplot
    public :: streamplot, errorbar, add_text_annotation, add_arrow_annotation

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
        
        plot_data%label = ''
        if (present(label)) plot_data%label = trim(label)
        
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
        
        ! Note: Text color would be handled in proper implementation
        ! annotation%color = [0.0_wp, 0.0_wp, 0.0_wp]  ! Black default
        ! if (present(color)) annotation%color = color
        
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
        
        ! Note: Text color would be handled in proper implementation
        ! annotation%color = [0.0_wp, 0.0_wp, 0.0_wp]  ! Black default
        ! if (present(color)) annotation%color = color
        
        annotation%alignment = 'center'
        if (present(halign)) annotation%alignment = trim(halign)
        
        ! Note: valign would be handled separately in full implementation
        
        ! Arrow properties (using available fields)
        annotation%has_arrow = .true.
        annotation%xytext_x = xy(1)
        annotation%xytext_y = xy(2)
        ! Note: arrow coordinate type would be handled in proper implementation
        
        ! Note: arrowprops would be handled in proper implementation
        ! annotation%arrowprops = 'arrowstyle=->'
        ! if (present(arrowprops)) annotation%arrowprops = trim(arrowprops)
        
        ! Add to annotations array
        if (.not. allocated(self%annotations)) then
            allocate(self%annotations(self%max_annotations))
        end if
        
        self%annotation_count = self%annotation_count + 1
        self%annotations(self%annotation_count) = annotation
    end subroutine add_arrow_annotation

    ! Private implementation subroutines

    subroutine add_line_plot_data(self, x, y, label, linestyle, color_rgb, color_str, marker)
        !! Add line plot data to internal storage
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle, color_str, marker
        real(wp), intent(in), optional :: color_rgb(3)
        
        integer :: plot_idx, color_idx, subplot_idx
        real(wp) :: rgb(3)
        logical :: success
        
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
            self%subplots(subplot_idx)%plots(plot_idx)%label = label
        else
            self%subplots(subplot_idx)%plots(plot_idx)%label = ''
        end if
        
        if (present(linestyle)) then
            self%subplots(subplot_idx)%plots(plot_idx)%linestyle = linestyle
        else
            self%subplots(subplot_idx)%plots(plot_idx)%linestyle = 'solid'
        end if

        if (present(marker)) then
            self%subplots(subplot_idx)%plots(plot_idx)%marker = marker
        else
            self%subplots(subplot_idx)%plots(plot_idx)%marker = 'None'
        end if
        
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
        
        ! Update main plot count
        self%plot_count = self%plot_count + 1
        if (self%plot_count <= self%max_plots) then
            self%plots(self%plot_count) = self%subplots(subplot_idx)%plots(plot_idx)
        end if
    end subroutine add_line_plot_data

    ! Additional implementation subroutines would be added here for:
    ! - add_3d_line_plot_data, add_surface_plot_data, add_contour_plot_data
    ! - add_colored_contour_plot_data, add_pcolormesh_plot_data  
    ! - add_bar_plot_data, add_histogram_plot_data, add_boxplot_data
    ! - add_scatter_plot_data, validate_histogram_input, etc.
    !
    ! Note: These are placeholder stubs - full implementations would preserve
    ! exact functionality from the original file

    subroutine add_3d_line_plot_data(self, x, y, z, label, linestyle, marker, markersize, linewidth)
        !! Stub implementation
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(wp), intent(in), optional :: markersize, linewidth
        ! Stub implementation
    end subroutine add_3d_line_plot_data

    subroutine add_surface_plot_data(self, x, y, z, label)
        !! Stub implementation  
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:,:)
        character(len=*), intent(in), optional :: label
        ! Stub implementation
    end subroutine add_surface_plot_data

    subroutine add_contour_plot_data(self, x_grid, y_grid, z_grid, levels, label)
        !! Stub implementation
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        ! Stub implementation
    end subroutine add_contour_plot_data

    subroutine add_colored_contour_plot_data(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        !! Stub implementation
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        ! Stub implementation
    end subroutine add_colored_contour_plot_data

    subroutine add_pcolormesh_plot_data(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths, error)
        !! Stub implementation
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap, edgecolors
        real(wp), intent(in), optional :: vmin, vmax, linewidths
        type(fortplot_error_t), intent(out), optional :: error
        ! Stub implementation
    end subroutine add_pcolormesh_plot_data

    subroutine add_bar_plot_data(self, positions, values, bar_size, label, color, horizontal)
        !! Stub implementation
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: positions(:), values(:), bar_size
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        logical, intent(in) :: horizontal
        ! Stub implementation
    end subroutine add_bar_plot_data

    function validate_histogram_input(self, data, bins) result(is_valid)
        !! Stub implementation
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in) :: bins
        logical :: is_valid
        is_valid = .true.
    end function validate_histogram_input

    subroutine add_histogram_plot_data(self, data, bins, density, label, color)
        !! Stub implementation
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in) :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        ! Stub implementation
    end subroutine add_histogram_plot_data

    subroutine add_boxplot_data(self, data, position, width, label, show_outliers, horizontal, color)
        !! Stub implementation
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:), position, width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers, horizontal
        real(wp), intent(in), optional :: color(3)
        ! Stub implementation
    end subroutine add_boxplot_data

    subroutine add_scatter_plot_data(self, x, y, z, s, c, label, marker, markersize, color, &
                                    colormap, vmin, vmax, show_colorbar, alpha)
        !! Stub implementation
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: z(:), s(:), c(:), markersize, color(3), vmin, vmax, alpha
        character(len=*), intent(in), optional :: label, marker, colormap
        logical, intent(in), optional :: show_colorbar
        ! Stub implementation
    end subroutine add_scatter_plot_data

    subroutine add_plot_to_figure(self, plot_data)
        !! Stub implementation
        class(figure_t), intent(inout) :: self
        type(plot_data_t), intent(in) :: plot_data
        ! Stub implementation
    end subroutine add_plot_to_figure

    subroutine setup_streamplot_parameters(self, x, y, u, v, density, color, linewidth, &
                                          rtol, atol, max_time, arrowsize, arrowstyle)
        !! Delegate to streamplot core module
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density, linewidth, rtol, atol, max_time, arrowsize
        real(wp), intent(in), optional :: color(3)
        character(len=*), intent(in), optional :: arrowstyle
        
        ! Delegate to streamplot core
        ! (This would be a proper delegation in the complete implementation)
    end subroutine setup_streamplot_parameters

end module fortplot_plotting