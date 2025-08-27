module fortplot_plotting
    !! Basic plot addition methods for figure (SOLID principles compliance)
    !! 
    !! This module contains basic add_* methods for plot types,
    !! separated from figure base for better modularity and maintainability.
    !! Advanced plot types are in fortplot_plotting_advanced module.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortplot_figure_core, only: figure_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, &
                                    PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                    PLOT_TYPE_ERRORBAR, PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, &
                                    PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER, &
                                    HALF_WIDTH, IQR_WHISKER_MULTIPLIER
    use fortplot_colors, only: parse_color, color_t
    use fortplot_annotations, only: text_annotation_t, COORD_DATA, COORD_FIGURE, COORD_AXIS
    use fortplot_logging, only: log_warning, log_error, log_info
    use fortplot_errors, only: fortplot_error_t, SUCCESS, ERROR_RESOURCE_LIMIT
    use fortplot_format_parser, only: parse_format_string
    use fortplot_coordinate_validation, only: validate_coordinate_arrays, coordinate_validation_result_t

    implicit none

    private
    public :: add_plot, add_3d_plot, add_scatter_2d, add_scatter_3d, add_surface
    public :: errorbar, add_text_annotation, add_arrow_annotation
    public :: add_line_plot_data, add_scatter_plot_data  ! Export for advanced module

    interface add_plot
        module procedure add_plot_impl
    end interface

    interface add_scatter_2d
        module procedure add_scatter_2d_impl
    end interface

    interface add_scatter_3d
        module procedure add_scatter_3d_impl
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

    subroutine errorbar_impl(self, x, y, xerr, yerr, xerr_lower, xerr_upper, &
                            yerr_lower, yerr_upper, label, marker, markersize, &
                            ecolor, elinewidth, capsize, capthick, color)
        !! Add error bar plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        real(wp), intent(in), optional :: xerr_lower(:), xerr_upper(:)
        real(wp), intent(in), optional :: yerr_lower(:), yerr_upper(:)
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: ecolor(3)
        real(wp), intent(in), optional :: elinewidth, capsize, capthick
        real(wp), intent(in), optional :: color(3)
        
        integer :: subplot_idx, plot_idx, color_idx
        
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_ERRORBAR
        
        allocate(self%plots(plot_idx)%x(size(x)))
        allocate(self%plots(plot_idx)%y(size(y)))
        self%plots(plot_idx)%x = x
        self%plots(plot_idx)%y = y
        
        ! Handle symmetric error bars
        if (present(xerr)) then
            allocate(self%plots(plot_idx)%xerr(size(xerr)))
            self%plots(plot_idx)%xerr = xerr
            self%plots(plot_idx)%has_xerr = .true.
        end if
        
        if (present(yerr)) then
            allocate(self%plots(plot_idx)%yerr(size(yerr)))
            self%plots(plot_idx)%yerr = yerr
            self%plots(plot_idx)%has_yerr = .true.
        end if
        
        ! Handle asymmetric error bars
        if (present(xerr_lower) .and. present(xerr_upper)) then
            allocate(self%plots(plot_idx)%xerr_lower(size(xerr_lower)))
            allocate(self%plots(plot_idx)%xerr_upper(size(xerr_upper)))
            self%plots(plot_idx)%xerr_lower = xerr_lower
            self%plots(plot_idx)%xerr_upper = xerr_upper
            self%plots(plot_idx)%has_xerr = .true.
            self%plots(plot_idx)%asymmetric_xerr = .true.
        end if
        
        if (present(yerr_lower) .and. present(yerr_upper)) then
            allocate(self%plots(plot_idx)%yerr_lower(size(yerr_lower)))
            allocate(self%plots(plot_idx)%yerr_upper(size(yerr_upper)))
            self%plots(plot_idx)%yerr_lower = yerr_lower
            self%plots(plot_idx)%yerr_upper = yerr_upper
            self%plots(plot_idx)%has_yerr = .true.
            self%plots(plot_idx)%asymmetric_yerr = .true.
        end if
        
        if (present(capsize)) then
            self%plots(plot_idx)%capsize = capsize
        end if
        
        if (present(elinewidth)) then
            self%plots(plot_idx)%elinewidth = elinewidth
        end if
        
        if (present(marker)) then
            self%plots(plot_idx)%marker = marker
        else
            self%plots(plot_idx)%marker = 'o'  ! Default to circle marker
        end if
        
        if (present(color)) then
            self%plots(plot_idx)%color = color
        else if (present(ecolor)) then
            self%plots(plot_idx)%color = ecolor
        else
            color_idx = mod(plot_idx - 1, 6) + 1
            self%plots(plot_idx)%color = self%state%colors(:, color_idx)
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if
    end subroutine errorbar_impl

    subroutine add_text_annotation(self, x, y, text, coord_type, font_size, rotation, &
                                  ha, va, bbox, color, alpha, weight, style)
        !! Add text annotation to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=*), intent(in), optional :: coord_type
        real(wp), intent(in), optional :: font_size
        real(wp), intent(in), optional :: rotation
        character(len=*), intent(in), optional :: ha, va
        logical, intent(in), optional :: bbox
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: alpha
        character(len=*), intent(in), optional :: weight, style
        
        type(text_annotation_t) :: annotation
        
        ! Set annotation properties
        annotation%x = x
        annotation%y = y
        annotation%text = text
        
        if (present(coord_type)) then
            select case (coord_type)
            case ('data')
                annotation%coord_type = COORD_DATA
            case ('figure')
                annotation%coord_type = COORD_FIGURE
            case ('axes')
                annotation%coord_type = COORD_AXIS
            case default
                annotation%coord_type = COORD_DATA
            end select
        else
            annotation%coord_type = COORD_DATA
        end if
        
        if (present(font_size)) annotation%font_size = font_size
        if (present(rotation)) annotation%rotation = rotation
        if (present(ha)) annotation%ha = ha
        if (present(va)) annotation%va = va
        if (present(bbox)) annotation%bbox = bbox
        if (present(color)) annotation%color = color
        if (present(alpha)) annotation%alpha = alpha
        if (present(weight)) annotation%weight = weight
        if (present(style)) annotation%style = style
        
        ! Add annotation to figure
        if (.not. allocated(self%annotations)) then
            allocate(self%annotations(self%max_annotations))
        end if
        
        self%annotation_count = self%annotation_count + 1
        self%annotations(self%annotation_count) = annotation
    end subroutine add_text_annotation

    subroutine add_arrow_annotation(self, text, xy, xytext, xy_coord_type, xytext_coord_type, &
                                   arrowprops, font_size, color, alpha)
        !! Add arrow annotation to figure
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: xy(2)  ! Arrow point
        real(wp), intent(in) :: xytext(2)  ! Text location
        character(len=*), intent(in), optional :: xy_coord_type
        character(len=*), intent(in), optional :: xytext_coord_type
        character(len=*), intent(in), optional :: arrowprops  ! JSON-like string
        real(wp), intent(in), optional :: font_size
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: alpha
        
        type(text_annotation_t) :: annotation
        
        ! Set annotation properties
        annotation%x = xytext(1)
        annotation%y = xytext(2)
        annotation%text = text
        annotation%arrow_x = xy(1)
        annotation%arrow_y = xy(2)
        annotation%has_arrow = .true.
        
        if (present(xytext_coord_type)) then
            select case (xytext_coord_type)
            case ('data')
                annotation%coord_type = COORD_DATA
            case ('figure')
                annotation%coord_type = COORD_FIGURE
            case ('axes')
                annotation%coord_type = COORD_AXIS
            case default
                annotation%coord_type = COORD_DATA
            end select
        else
            annotation%coord_type = COORD_DATA
        end if
        
        if (present(xy_coord_type)) then
            select case (xy_coord_type)
            case ('data')
                annotation%arrow_coord_type = COORD_DATA
            case ('figure')
                annotation%arrow_coord_type = COORD_FIGURE
            case ('axes')
                annotation%arrow_coord_type = COORD_AXIS
            case default
                annotation%arrow_coord_type = COORD_DATA
            end select
        else
            annotation%arrow_coord_type = COORD_DATA
        end if
        
        if (present(arrowprops)) annotation%arrowstyle = arrowprops
        if (present(font_size)) annotation%font_size = font_size
        if (present(color)) annotation%color = color
        if (present(alpha)) annotation%alpha = alpha
        
        ! Add annotation to figure
        if (.not. allocated(self%annotations)) then
            allocate(self%annotations(self%max_annotations))
        end if
        
        self%annotation_count = self%annotation_count + 1
        self%annotations(self%annotation_count) = annotation
    end subroutine add_arrow_annotation

    ! Private implementation subroutines

    subroutine add_line_plot_data(self, x, y, label, linestyle, color_rgb, color_str, marker)
        !! Add line plot data with comprehensive validation
        !! Refactored to be under 100 lines (QADS compliance)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle, color_str, marker
        real(wp), intent(in), optional :: color_rgb(3)
        
        integer :: plot_idx
        type(coordinate_validation_result_t) :: validation
        
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
        
        ! Get current plot index
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        ! Initialize plot data
        call init_line_plot_data(self%plots(plot_idx), x, y)
        
        ! Set optional properties
        call set_line_plot_properties(self%plots(plot_idx), plot_idx, &
                                     label, linestyle, marker, color_rgb, color_str, self%state)
    end subroutine add_line_plot_data
    
    subroutine init_line_plot_data(plot, x, y)
        !! Initialize basic line plot data
        type(plot_data_t), intent(inout) :: plot
        real(wp), intent(in) :: x(:), y(:)
        
        plot%plot_type = PLOT_TYPE_LINE
        
        allocate(plot%x(size(x)))
        allocate(plot%y(size(y)))
        plot%x = x
        plot%y = y
    end subroutine init_line_plot_data
    
    subroutine set_line_plot_properties(plot, plot_idx, label, linestyle, marker, &
                                       color_rgb, color_str, state)
        !! Set line plot properties (style, color, etc.)
        !! Extracted from add_line_plot_data for QADS compliance
        type(plot_data_t), intent(inout) :: plot
        integer, intent(in) :: plot_idx
        character(len=*), intent(in), optional :: label, linestyle, marker, color_str
        real(wp), intent(in), optional :: color_rgb(3)
        type(figure_state_t), intent(in) :: state
        
        character(len=20) :: parsed_marker, parsed_linestyle
        real(wp) :: rgb(3)
        logical :: success
        integer :: color_idx
        
        ! Set label
        if (present(label) .and. len_trim(label) > 0) then
            plot%label = label
        end if
        
        ! Parse linestyle to extract marker and line style components
        if (present(linestyle)) then
            call parse_format_string(linestyle, parsed_marker, parsed_linestyle)
            
            if (len_trim(parsed_marker) > 0) then
                plot%marker = trim(parsed_marker)
                if (len_trim(parsed_linestyle) > 0) then
                    plot%linestyle = trim(parsed_linestyle)
                else
                    plot%linestyle = 'None'  ! No line, marker only
                end if
            else
                plot%linestyle = linestyle
                if (present(marker)) then
                    plot%marker = marker
                else
                    plot%marker = 'None'
                end if
            end if
        else
            plot%linestyle = 'solid'
            if (present(marker)) then
                plot%marker = marker
            else
                plot%marker = 'None'
            end if
        end if
        
        ! Handle color specification
        if (present(color_str)) then
            call parse_color(color_str, rgb, success)
            if (success) then
                plot%color = rgb
            else
                color_idx = mod(plot_idx - 1, 6) + 1
                plot%color = state%colors(:, color_idx)
            end if
        else if (present(color_rgb)) then
            plot%color = color_rgb
        else
            color_idx = mod(plot_idx - 1, 6) + 1
            plot%color = state%colors(:, color_idx)
        end if
    end subroutine set_line_plot_properties

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
        
        ! Get current plot index
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_CONTOUR
        
        ! Store grid data
        allocate(self%plots(plot_idx)%x_grid(size(x)))
        allocate(self%plots(plot_idx)%y_grid(size(y)))
        allocate(self%plots(plot_idx)%z_grid(size(z, 1), size(z, 2)))
        
        self%plots(plot_idx)%x_grid = x
        self%plots(plot_idx)%y_grid = y
        self%plots(plot_idx)%z_grid = z
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if
    end subroutine add_surface_plot_data

    subroutine add_scatter_plot_data(self, x, y, z, s, c, label, marker, markersize, color, &
                                    colormap, vmin, vmax, show_colorbar, alpha)
        !! Add scatter plot data with optional properties
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: z(:), s(:), c(:), markersize, color(3), vmin, vmax, alpha
        character(len=*), intent(in), optional :: label, marker, colormap
        logical, intent(in), optional :: show_colorbar
        
        integer :: subplot_idx, plot_idx
        
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_SCATTER
        
        allocate(self%plots(plot_idx)%x(size(x)))
        allocate(self%plots(plot_idx)%y(size(y)))
        
        self%plots(plot_idx)%x = x
        self%plots(plot_idx)%y = y
        
        if (present(z)) then
            allocate(self%plots(plot_idx)%z(size(z)))
            self%plots(plot_idx)%z = z
        end if
        
        if (present(s)) then
            allocate(self%plots(plot_idx)%scatter_sizes(size(s)))
            self%plots(plot_idx)%scatter_sizes = s
        end if
        
        if (present(c)) then
            allocate(self%plots(plot_idx)%scatter_colors(size(c)))
            self%plots(plot_idx)%scatter_colors = c
        end if
        
        if (present(color)) then
            self%plots(plot_idx)%color = color
        end if
        
        if (present(marker)) then
            self%plots(plot_idx)%marker = marker
        end if
        
        if (present(markersize)) then
            self%plots(plot_idx)%scatter_size_default = markersize
        end if
        
        if (present(colormap)) then
            self%plots(plot_idx)%scatter_colormap = colormap
        end if
        
        if (present(vmin)) then
            self%plots(plot_idx)%scatter_vmin = vmin
            self%plots(plot_idx)%scatter_vrange_set = .true.
        end if
        
        if (present(vmax)) then
            self%plots(plot_idx)%scatter_vmax = vmax
            self%plots(plot_idx)%scatter_vrange_set = .true.
        end if
        
        if (present(show_colorbar)) then
            self%plots(plot_idx)%scatter_colorbar = show_colorbar
        end if
        
        ! Note: alpha not directly supported in plot_data_t structure
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if
    end subroutine add_scatter_plot_data

end module fortplot_plotting