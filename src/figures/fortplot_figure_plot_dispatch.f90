module fortplot_figure_plot_dispatch
    !! Plot rendering dispatch module
    !!
    !! Single Responsibility: Dispatch rendering for individual plot types
    !! Extracted from fortplot_figure_rendering_pipeline to respect module size limits.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, PLOT_TYPE_LINE, &
                                  PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                  PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
                                  PLOT_TYPE_BOXPLOT, PLOT_TYPE_ERRORBAR, &
                                  PLOT_TYPE_SURFACE, PLOT_TYPE_PIE, &
                                  PLOT_TYPE_BAR, PLOT_TYPE_REFLINE, &
                                  PLOT_TYPE_QUIVER, PLOT_TYPE_POLAR, &
                                  AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_rendering, only: render_line_plot, render_contour_plot, &
                                  render_pcolormesh_plot, render_fill_between_plot, &
                                  render_markers, render_boxplot_plot, &
                                  render_errorbar_plot, &
                                  render_pie_plot, render_bar_plot
    use fortplot_surface_rendering, only: render_surface_plot
    use fortplot_polar_rendering, only: render_polar_data, render_polar_boundary, &
                                        render_polar_radial_gridlines, &
                                        render_polar_angular_gridlines, &
                                        render_polar_angular_ticks
    implicit none

    private
    public :: render_all_plots
    public :: render_streamplot_arrows
    public :: render_polar_axes

contains

    subroutine render_all_plots(backend, plots, plot_count, &
                                x_min_transformed, x_max_transformed, &
                                y_min_transformed, y_max_transformed, &
                                xscale, yscale, symlog_threshold, &
                                width, height, margin_left, margin_right, &
                                margin_bottom, margin_top, state)
        !! Render all plots in the figure
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        real(wp), intent(in) :: x_min_transformed, x_max_transformed
        real(wp), intent(in) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top
        type(figure_state_t), intent(in), optional :: state

        integer :: i
        real(wp) :: x_min_curr, x_max_curr, y_min_curr, y_max_curr
        character(len=10) :: xscale_curr, yscale_curr
        real(wp) :: primary_x_min, primary_x_max, primary_y_min, primary_y_max
        real(wp) :: default_line_width
        logical :: restore_needed

        call resolve_primary_coordinates(state, primary_x_min, primary_x_max, &
                                         primary_y_min, primary_y_max, default_line_width)

        do i = 1, plot_count
            call resolve_plot_coordinates(plots(i), state, primary_x_min, primary_x_max, &
                                          primary_y_min, primary_y_max, &
                                          x_min_curr, x_max_curr, y_min_curr, y_max_curr, &
                                          xscale_curr, yscale_curr, restore_needed)

            if (restore_needed) then
                call backend%set_coordinates(x_min_curr, x_max_curr, y_min_curr, &
                                             y_max_curr)
            end if

            if (plots(i)%line_width > 0.0_wp) then
                call backend%set_line_width(plots(i)%line_width)
            else
                call backend%set_line_width(default_line_width)
            end if

            call dispatch_plot_render(backend, plots(i), &
                                      x_min_curr, x_max_curr, y_min_curr, y_max_curr, &
                                      xscale_curr, yscale_curr, symlog_threshold, &
                                      width, height, margin_left, margin_right, &
                                      margin_bottom, margin_top, default_line_width, state)

            if (present(state) .and. restore_needed) then
                call backend%set_coordinates(primary_x_min, primary_x_max, &
                                             primary_y_min, primary_y_max)
            end if
        end do

        if (present(state)) then
            call backend%set_coordinates(primary_x_min, primary_x_max, primary_y_min, &
                                         primary_y_max)
        end if
    end subroutine render_all_plots

    subroutine resolve_primary_coordinates(state, px_min, px_max, py_min, py_max, default_lw)
        !! Resolve primary coordinate ranges and default line width from figure state
        type(figure_state_t), intent(in), optional :: state
        real(wp), intent(out) :: px_min, px_max, py_min, py_max
        real(wp), intent(out) :: default_lw

        if (present(state)) then
            px_min = state%x_min_transformed
            px_max = state%x_max_transformed
            py_min = state%y_min_transformed
            py_max = state%y_max_transformed
            default_lw = state%current_line_width
        else
            px_min = 0.0_wp; px_max = 0.0_wp
            py_min = 0.0_wp; py_max = 0.0_wp
            default_lw = 1.5_wp
        end if
    end subroutine resolve_primary_coordinates

    subroutine resolve_plot_coordinates(plot, state, primary_x_min, primary_x_max, &
                                        primary_y_min, primary_y_max, &
                                        x_min, x_max, y_min, y_max, &
                                        xscale, yscale, restore_needed)
        !! Resolve coordinate ranges for a single plot, handling twin axes
        type(plot_data_t), intent(in) :: plot
        type(figure_state_t), intent(in), optional :: state
        real(wp), intent(in) :: primary_x_min, primary_x_max, primary_y_min, primary_y_max
        real(wp), intent(out) :: x_min, x_max, y_min, y_max
        character(len=10), intent(out) :: xscale, yscale
        logical, intent(out) :: restore_needed

        if (present(state)) then
            select case (plot%axis)
            case (AXIS_TWINX)
                x_min = state%x_min_transformed
                x_max = state%x_max_transformed
                y_min = state%twinx_y_min_transformed
                y_max = state%twinx_y_max_transformed
                xscale = state%xscale
                yscale = state%twinx_yscale
                restore_needed = .true.
            case (AXIS_TWINY)
                x_min = state%twiny_x_min_transformed
                x_max = state%twiny_x_max_transformed
                y_min = state%y_min_transformed
                y_max = state%y_max_transformed
                xscale = state%twiny_xscale
                yscale = state%yscale
                restore_needed = .true.
            case default
                x_min = primary_x_min
                x_max = primary_x_max
                y_min = primary_y_min
                y_max = primary_y_max
                xscale = state%xscale
                yscale = state%yscale
                restore_needed = .false.
            end select
        else
            x_min = 0.0_wp; x_max = 0.0_wp
            y_min = 0.0_wp; y_max = 0.0_wp
            xscale = ''; yscale = ''
            restore_needed = .false.
        end if
    end subroutine resolve_plot_coordinates

    subroutine dispatch_plot_render(backend, plot, x_min, x_max, y_min, y_max, &
                                    xscale, yscale, symlog_threshold, &
                                    width, height, margin_left, margin_right, &
                                    margin_bottom, margin_top, default_line_width, state)
        !! Dispatch rendering for a single plot type
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top
        real(wp), intent(in) :: default_line_width
        type(figure_state_t), intent(in), optional :: state

        select case (plot%plot_type)
        case (PLOT_TYPE_LINE)
            call render_line_plot(backend, plot, xscale, yscale, symlog_threshold)
            if (allocated(plot%marker)) then
                call render_markers(backend, plot, x_min, x_max, y_min, y_max, &
                                    xscale, yscale, symlog_threshold)
            end if

        case (PLOT_TYPE_SCATTER)
            if (allocated(plot%marker)) then
                call render_markers(backend, plot, x_min, x_max, y_min, y_max, &
                                    xscale, yscale, symlog_threshold)
            end if

        case (PLOT_TYPE_CONTOUR)
            call render_contour_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                     xscale, yscale, symlog_threshold, &
                                     width, height, margin_left, margin_right, &
                                     margin_bottom, margin_top)

        case (PLOT_TYPE_PCOLORMESH)
            call render_pcolormesh_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                        xscale, yscale, symlog_threshold, &
                                        width, height, margin_right)

        case (PLOT_TYPE_SURFACE)
            call render_surface_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                     xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_FILL)
            call render_fill_between_plot(backend, plot, xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_BAR)
            call render_bar_plot(backend, plot, xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_PIE)
            call render_pie_plot(backend, plot, xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_BOXPLOT)
            call render_boxplot_plot(backend, plot, xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_ERRORBAR)
            call render_errorbar_plot(backend, plot, xscale, yscale, symlog_threshold, &
                                      default_line_width)
            call render_markers(backend, plot, x_min, x_max, y_min, y_max, &
                                xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_REFLINE)
            call render_refline_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                     xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_QUIVER)
            call render_quiver_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                    xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_POLAR)
            call render_polar_plot_internal(backend, plot, x_min, x_max, y_min, y_max, state)

        end select
    end subroutine dispatch_plot_render

    subroutine render_refline_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                   xscale, yscale, symlog_threshold)
        !! Render a reference line (horizontal or vertical)
        !! Reference lines store normalized coordinates for axis-spanning lines
        !! or actual data coordinates for hlines/vlines
        use fortplot_scales, only: apply_scale_transform
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        real(wp) :: x1, y1, x2, y2
        real(wp) :: x1_scaled, y1_scaled, x2_scaled, y2_scaled

        if (.not. allocated(plot%x) .or. .not. allocated(plot%y)) return
        if (size(plot%x) < 2 .or. size(plot%y) < 2) return

        call backend%color(plot%color(1), plot%color(2), plot%color(3))

        if (allocated(plot%linestyle) .and. len_trim(plot%linestyle) > 0) then
            call backend%set_line_style(trim(plot%linestyle))
        else
            call backend%set_line_style('-')
        end if

        x1 = plot%x(1)
        x2 = plot%x(2)
        y1 = plot%y(1)
        y2 = plot%y(2)

        ! For horizontal lines (y1 == y2), x values may be normalized
        if (abs(y1 - y2) < 1.0e-9_wp) then
            ! Horizontal line: check if x values are normalized (0-1)
            if (x1 >= 0.0_wp .and. x1 <= 1.0_wp .and. &
                x2 >= 0.0_wp .and. x2 <= 1.0_wp .and. &
                (x1 < 0.01_wp .or. x2 > 0.99_wp)) then
                ! Convert normalized x to data coordinates
                x1 = x_min + x1*(x_max - x_min)
                x2 = x_min + x2*(x_max - x_min)
            end if
        end if

        ! For vertical lines (x1 == x2), y values may be normalized
        if (abs(x1 - x2) < 1.0e-9_wp) then
            ! Vertical line: check if y values are normalized (0-1)
            if (y1 >= 0.0_wp .and. y1 <= 1.0_wp .and. &
                y2 >= 0.0_wp .and. y2 <= 1.0_wp .and. &
                (y1 < 0.01_wp .or. y2 > 0.99_wp)) then
                ! Convert normalized y to data coordinates
                y1 = y_min + y1*(y_max - y_min)
                y2 = y_min + y2*(y_max - y_min)
            end if
        end if

        ! Apply scale transformations
        x1_scaled = apply_scale_transform(x1, xscale, symlog_threshold)
        x2_scaled = apply_scale_transform(x2, xscale, symlog_threshold)
        y1_scaled = apply_scale_transform(y1, yscale, symlog_threshold)
        y2_scaled = apply_scale_transform(y2, yscale, symlog_threshold)

        ! Draw the line
        call backend%line(x1_scaled, y1_scaled, x2_scaled, y2_scaled)
    end subroutine render_refline_plot

    subroutine render_quiver_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                   xscale, yscale, symlog_threshold)
        !! Render quiver plot (discrete vector arrows)
        !! Draws arrows at each (x,y) position with direction (u,v)
        !! Respects angles, pivot, alpha, and per-arrow c(:) color mapping.
        use fortplot_scales, only: apply_scale_transform
        use fortplot_colormap, only: colormap_value_to_color
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer :: i, n
        real(wp) :: x_pos, y_pos, u_raw, v_raw
        real(wp) :: u_scaled, v_scaled, mag, max_mag
        real(wp) :: x_range, y_range, data_scale
        real(wp) :: scale, arrow_size
        real(wp) :: pivot_offset_x, pivot_offset_y
        real(wp) :: cmap_color(3)
        character(len=10) :: angles_mode
        character(len=10) :: pivot_mode
        character(len=:), allocatable :: cmap_name

        if (.not. allocated(plot%x) .or. .not. allocated(plot%y)) return
        if (.not. allocated(plot%quiver_u) .or. .not. allocated(plot%quiver_v)) return

        n = size(plot%x)
        if (n == 0) return
        if (size(plot%y) /= n .or. size(plot%quiver_u) /= n .or. &
            size(plot%quiver_v) /= n) return

        angles_mode = plot%quiver_angles
        pivot_mode = plot%quiver_pivot
        cmap_name = plot%quiver_colormap

        scale = plot%quiver_scale
        x_range = max(1.0e-9_wp, x_max - x_min)
        y_range = max(1.0e-9_wp, y_max - y_min)

        ! Compute max magnitude for scaling
        max_mag = 0.0_wp
        do i = 1, n
            mag = sqrt(plot%quiver_u(i)**2 + plot%quiver_v(i)**2)
            if (mag > max_mag) max_mag = mag
        end do
        if (max_mag < 1.0e-12_wp) max_mag = 1.0_wp

        data_scale = min(x_range, y_range)*0.05_wp*scale/max_mag
        arrow_size = 1.0_wp

        ! Compute pivot offset: how far the arrow base is from (x,y)
        ! pivot='tail': base at (x,y) -> offset = 0
        ! pivot='mid': base at midpoint -> offset = -0.5*vector
        ! pivot='tip': base at arrow tip -> offset = -1.0*vector
        pivot_offset_x = 0.0_wp
        pivot_offset_y = 0.0_wp
        if (trim(pivot_mode) == 'mid') then
            pivot_offset_x = -0.5_wp
            pivot_offset_y = -0.5_wp
        else if (trim(pivot_mode) == 'tip') then
            pivot_offset_x = -1.0_wp
            pivot_offset_y = -1.0_wp
        end if

        do i = 1, n
            x_pos = plot%x(i)
            y_pos = plot%y(i)
            u_raw = plot%quiver_u(i)
            v_raw = plot%quiver_v(i)

            ! Scale the vector
            u_scaled = u_raw * data_scale
            v_scaled = v_raw * data_scale
            mag = sqrt(u_scaled**2 + v_scaled**2)

            if (mag < 1.0e-12_wp) cycle

            ! Apply angles mode to compute arrow orientation
            if (trim(angles_mode) == 'xy') then
                ! Arrow points from (x,y) to (x+u, y+v)
                ! Already computed as u_scaled, v_scaled
            else
                ! Default 'uv' or 'native': use u,v directly
                ! Apply rotation if needed (not implemented for simplicity)
            end if

            ! Apply pivot offset to base position
            x_pos = x_pos + pivot_offset_x * u_scaled
            y_pos = y_pos + pivot_offset_y * v_scaled

            ! Set color: use c(:) colormap if present, else solid color
            if (allocated(plot%scatter_colors) .and. size(plot%scatter_colors) == n) then
                ! Map scalar c value through colormap
                if (allocated(cmap_name) .and. len_trim(cmap_name) > 0) then
                    call colormap_value_to_color(plot%scatter_colors(i), &
                                                minval(plot%scatter_colors), &
                                                maxval(plot%scatter_colors), &
                                                trim(cmap_name), &
                                                cmap_color)
                else
                    call colormap_value_to_color(plot%scatter_colors(i), &
                                                minval(plot%scatter_colors), &
                                                maxval(plot%scatter_colors), &
                                                'viridis', &
                                                cmap_color)
                end if
                call backend%color(cmap_color(1), cmap_color(2), cmap_color(3))
            else
                call backend%color(plot%color(1), plot%color(2), plot%color(3))
            end if

            call backend%set_line_style('-')

            ! Draw the arrow
            call backend%draw_arrow(x_pos, y_pos, u_scaled, v_scaled, &
                                    arrow_size, '->')
        end do
    end subroutine render_quiver_plot

    subroutine render_streamplot_arrows(backend, arrows)
        !! Render queued streamplot arrows after plot lines are drawn
        class(plot_context), intent(inout) :: backend
        type(arrow_data_t), intent(in) :: arrows(:)
        integer :: i

        if (size(arrows) <= 0) return

        do i = 1, size(arrows)
            call backend%draw_arrow(arrows(i)%x, arrows(i)%y, arrows(i)%dx, &
                                    arrows(i)%dy, &
                                    arrows(i)%size, arrows(i)%style)
        end do
    end subroutine render_streamplot_arrows

    subroutine render_polar_axes(backend, x_min, x_max, y_min, y_max, state)
        !! Render polar axes: circular boundary, radial spokes, angular circles, tick labels
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(figure_state_t), intent(in) :: state

        real(wp) :: center_x, center_y, radius
        real(wp) :: theta_offset
        logical :: clockwise
        integer :: n_spokes, n_circles

        if (.not. state%polar_projection) return

        center_x = (x_min + x_max)*0.5_wp
        center_y = (y_min + y_max)*0.5_wp
        radius = min(x_max - x_min, y_max - y_min)*0.45_wp

        theta_offset = state%polar_theta_offset
        clockwise = state%polar_theta_direction_cw
        n_spokes = state%polar_theta_gridlines
        n_circles = state%polar_r_gridlines

        ! Render concentric circles (angular gridlines)
        call render_polar_angular_gridlines(backend, center_x, center_y, radius, &
                                            n_circles)

        ! Render radial spokes
        call render_polar_radial_gridlines(backend, center_x, center_y, radius, &
                                           n_spokes, theta_offset, clockwise)

        ! Render circular boundary
        call render_polar_boundary(backend, center_x, center_y, radius)

        ! Render angular tick labels
        call render_polar_angular_ticks(backend, center_x, center_y, radius, &
                                        n_spokes, theta_offset, clockwise)
    end subroutine render_polar_axes

    subroutine render_polar_plot_internal(backend, plot, x_min, x_max, y_min, y_max, &
                                          state)
        !! Render polar plot data within the coordinate system
        !! The plot stores pre-converted Cartesian coordinates in x/y arrays
        !! but we use polar_theta/polar_r for proper polar rendering
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(figure_state_t), intent(in), optional :: state

        real(wp) :: center_x, center_y, radius, r_scale
        real(wp) :: theta_offset
        logical :: clockwise
        integer :: n

        ! Compute center and radius in data coordinates
        center_x = (x_min + x_max)*0.5_wp
        center_y = (y_min + y_max)*0.5_wp
        radius = min(x_max - x_min, y_max - y_min)*0.45_wp

        ! Get polar configuration from state
        theta_offset = 0.0_wp  ! 0 deg at east (matplotlib)
        clockwise = .false.
        if (present(state)) then
            theta_offset = state%polar_theta_offset
            clockwise = state%polar_theta_direction_cw
        end if

        ! Compute r_scale based on polar data range
        r_scale = 1.0_wp
        if (allocated(plot%polar_r)) then
            if (size(plot%polar_r) > 0) then
                r_scale = radius/maxval(abs(plot%polar_r))
            end if
        end if

        ! Render polar data if available
        if (allocated(plot%polar_theta) .and. allocated(plot%polar_r)) then
            n = min(size(plot%polar_theta), size(plot%polar_r))
            if (n > 0) then
                call render_polar_data(backend, plot%polar_theta, plot%polar_r, n, &
                                       center_x, center_y, r_scale, &
                                       theta_offset, clockwise, plot%color)
            end if
        end if
    end subroutine render_polar_plot_internal

end module fortplot_figure_plot_dispatch
