module fortplot_figure_plots
    !! Plot creation methods for figure_t
    !! 
    !! This module contains the core plot creation functionality extracted
    !! from fortplot_figure_core to achieve QADS compliance (<500 lines).
    !!
    !! Single Responsibility: Handle creation of different plot types
    !! (line plots, contours, filled contours, pcolormesh)

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_plot_management
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_format_parser, only: parse_format_string
    use fortplot_colors, only: parse_color
    use fortplot_logging, only: log_warning
    implicit none

    private
    public :: figure_add_plot, figure_add_contour, figure_add_contour_filled
    public :: figure_add_pcolormesh, figure_add_fill_between

contains

    subroutine figure_add_plot(plots, state, x, y, label, linestyle, color)
        !! Add a line plot to the figure
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        
        real(wp) :: plot_color(3)
        character(len=:), allocatable :: ls
        character(len=20) :: parsed_marker, parsed_linestyle
        
        ! Determine color
        if (present(color)) then
            plot_color = color
        else
            plot_color = next_plot_color(state)
        end if
        
        ! Parse linestyle to extract marker and actual linestyle
        if (present(linestyle)) then
            call parse_format_string(linestyle, parsed_marker, parsed_linestyle)
            if (len_trim(parsed_linestyle) > 0) then
                ls = trim(parsed_linestyle)
            else
                ! If only a marker was specified, do not draw connecting lines
                ls = 'none'
            end if
        else
            ls = '-'
            parsed_marker = ''
        end if
        
        ! Add the plot data using focused module
        call add_line_plot_data(plots, state%plot_count, state%max_plots, &
                               x, y, label, ls, plot_color, &
                               marker=trim(parsed_marker))
    end subroutine figure_add_plot

    subroutine figure_add_contour(plots, state, x_grid, y_grid, z_grid, levels, label)
        !! Add a contour plot to the figure
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        call add_contour_plot_data(plots, state%plot_count, state%max_plots, &
                                  state%colors, x_grid, y_grid, z_grid, levels, label)
    end subroutine figure_add_contour

    subroutine figure_add_contour_filled(plots, state, x_grid, y_grid, z_grid, levels, &
                                        colormap, show_colorbar, label)
        !! Add a filled contour plot with color mapping
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
        call add_colored_contour_plot_data(plots, state%plot_count, state%max_plots, &
                                          x_grid, y_grid, z_grid, levels, colormap, &
                                          show_colorbar, label)
    end subroutine figure_add_contour_filled

    subroutine figure_add_pcolormesh(plots, state, x, y, c, colormap, vmin, vmax, &
                                    edgecolors, linewidths)
        !! Add a pcolormesh plot
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        
        call add_pcolormesh_plot_data(plots, state%plot_count, state%max_plots, &
                                     x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
    end subroutine figure_add_pcolormesh

    subroutine figure_add_fill_between(plots, state, x, upper, lower, mask, color_string, alpha)
        !! Add an area fill between two curves
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: upper(:)
        real(wp), intent(in) :: lower(:)
        logical, intent(in), optional :: mask(:)
        character(len=*), intent(in), optional :: color_string
        real(wp), intent(in), optional :: alpha

        real(wp) :: fill_color(3)
        logical :: success
        real(wp) :: fill_alpha

        fill_color = next_plot_color(state)
        if (present(color_string)) then
            call parse_color(color_string, fill_color, success)
            if (.not. success) then
                call log_warning('fill_between: unsupported color string; using default palette color')
                fill_color = next_plot_color(state)
            end if
        end if

        fill_alpha = 1.0_wp
        if (present(alpha)) fill_alpha = max(0.0_wp, min(1.0_wp, alpha))

        call add_fill_between_plot_data(plots, state%plot_count, state%max_plots, x, upper, lower, &
                                        mask, fill_color, fill_alpha)
    end subroutine figure_add_fill_between

end module fortplot_figure_plots
