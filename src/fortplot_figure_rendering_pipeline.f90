module fortplot_figure_rendering_pipeline
    !! Figure rendering pipeline module
    !! 
    !! Single Responsibility: Coordinate the complete rendering pipeline
    !! Extracted from fortplot_figure_core to improve modularity
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH
    use fortplot_rendering, only: render_line_plot, render_contour_plot, &
                                 render_pcolormesh_plot, render_markers
    use fortplot_legend, only: legend_t
    implicit none
    
    private
    public :: calculate_figure_data_ranges, setup_coordinate_system
    public :: render_figure_background, render_figure_axes, render_all_plots
    
contains
    
    subroutine calculate_figure_data_ranges(plots, plot_count, xlim_set, ylim_set, &
                                          x_min, x_max, y_min, y_max, &
                                          x_min_transformed, x_max_transformed, &
                                          y_min_transformed, y_max_transformed, &
                                          xscale, yscale, symlog_threshold)
        !! Calculate overall data ranges for the figure with robust edge case handling
        !! Fixed Issue #432: Handles zero-size arrays and single points properly
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        real(wp), intent(out) :: x_min_transformed, x_max_transformed
        real(wp), intent(out) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        
        real(wp) :: x_min_data, x_max_data, y_min_data, y_max_data
        logical :: first_plot, has_valid_data
        integer :: i
        
        ! Initialize data ranges and check for early return
        call initialize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                   x_min_transformed, x_max_transformed, &
                                   y_min_transformed, y_max_transformed, &
                                   xscale, yscale, symlog_threshold, &
                                   x_min_data, x_max_data, y_min_data, y_max_data, &
                                   first_plot, has_valid_data)
        if (xlim_set .and. ylim_set) return
        
        ! Process all plots to calculate data ranges
        do i = 1, plot_count
            select case (plots(i)%plot_type)
            case (PLOT_TYPE_LINE)
                call process_line_plot_ranges(plots(i), first_plot, has_valid_data, &
                                             x_min_data, x_max_data, y_min_data, y_max_data)
                
            case (PLOT_TYPE_CONTOUR)
                call process_contour_plot_ranges(plots(i), first_plot, has_valid_data, &
                                                x_min_data, x_max_data, y_min_data, y_max_data)
                
            case (PLOT_TYPE_PCOLORMESH)
                call process_pcolormesh_ranges(plots(i), first_plot, has_valid_data, &
                                              x_min_data, x_max_data, y_min_data, y_max_data)
            end select
        end do
        
        ! Apply single point margins if needed
        call apply_single_point_margins(has_valid_data, x_min_data, x_max_data, &
                                       y_min_data, y_max_data)
        
        ! Finalize data ranges with user limits and transformations
        call finalize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                 x_min_data, x_max_data, y_min_data, y_max_data, &
                                 x_min_transformed, x_max_transformed, &
                                 y_min_transformed, y_max_transformed, &
                                 xscale, yscale, symlog_threshold)
    end subroutine calculate_figure_data_ranges
    
    subroutine initialize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                     x_min_transformed, x_max_transformed, &
                                     y_min_transformed, y_max_transformed, &
                                     xscale, yscale, symlog_threshold, &
                                     x_min_data, x_max_data, y_min_data, y_max_data, &
                                     first_plot, has_valid_data)
        !! Initialize data ranges and handle early return case
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(out) :: x_min_transformed, x_max_transformed
        real(wp), intent(out) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(out) :: x_min_data, x_max_data, y_min_data, y_max_data
        logical, intent(out) :: first_plot, has_valid_data
        
        if (xlim_set .and. ylim_set) then
            x_min_transformed = apply_scale_transform(x_min, xscale, symlog_threshold)
            x_max_transformed = apply_scale_transform(x_max, xscale, symlog_threshold)
            y_min_transformed = apply_scale_transform(y_min, yscale, symlog_threshold)
            y_max_transformed = apply_scale_transform(y_max, yscale, symlog_threshold)
            return
        end if
        
        first_plot = .true.
        has_valid_data = .false.
        
        ! Initialize with safe default ranges for empty data case
        x_min_data = 0.0_wp
        x_max_data = 1.0_wp
        y_min_data = 0.0_wp
        y_max_data = 1.0_wp
    end subroutine initialize_data_ranges
    
    subroutine process_line_plot_ranges(plot, first_plot, has_valid_data, &
                                       x_min_data, x_max_data, y_min_data, y_max_data)
        !! Process line plot data to calculate ranges
        type(plot_data_t), intent(in) :: plot
        logical, intent(inout) :: first_plot, has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data
        
        if (allocated(plot%x) .and. allocated(plot%y)) then
            ! CRITICAL FIX: Check for non-empty arrays before minval/maxval
            if (size(plot%x) > 0 .and. size(plot%y) > 0) then
                if (first_plot) then
                    x_min_data = minval(plot%x)
                    x_max_data = maxval(plot%x)
                    y_min_data = minval(plot%y)
                    y_max_data = maxval(plot%y)
                    first_plot = .false.
                else
                    x_min_data = min(x_min_data, minval(plot%x))
                    x_max_data = max(x_max_data, maxval(plot%x))
                    y_min_data = min(y_min_data, minval(plot%y))
                    y_max_data = max(y_max_data, maxval(plot%y))
                end if
                has_valid_data = .true.
            end if
        end if
    end subroutine process_line_plot_ranges
    
    subroutine process_contour_plot_ranges(plot, first_plot, has_valid_data, &
                                          x_min_data, x_max_data, y_min_data, y_max_data)
        !! Process contour plot data to calculate ranges
        type(plot_data_t), intent(in) :: plot
        logical, intent(inout) :: first_plot, has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data
        
        if (allocated(plot%x_grid) .and. allocated(plot%y_grid)) then
            if (size(plot%x_grid) > 0 .and. size(plot%y_grid) > 0) then
                if (first_plot) then
                    x_min_data = minval(plot%x_grid)
                    x_max_data = maxval(plot%x_grid)
                    y_min_data = minval(plot%y_grid)
                    y_max_data = maxval(plot%y_grid)
                    first_plot = .false.
                else
                    x_min_data = min(x_min_data, minval(plot%x_grid))
                    x_max_data = max(x_max_data, maxval(plot%x_grid))
                    y_min_data = min(y_min_data, minval(plot%y_grid))
                    y_max_data = max(y_max_data, maxval(plot%y_grid))
                end if
                has_valid_data = .true.
            end if
        end if
    end subroutine process_contour_plot_ranges
    
    subroutine process_pcolormesh_ranges(plot, first_plot, has_valid_data, &
                                        x_min_data, x_max_data, y_min_data, y_max_data)
        !! Process pcolormesh plot data to calculate ranges
        type(plot_data_t), intent(in) :: plot
        logical, intent(inout) :: first_plot, has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data
        
        if (allocated(plot%pcolormesh_data%x_vertices) .and. &
            allocated(plot%pcolormesh_data%y_vertices)) then
            if (size(plot%pcolormesh_data%x_vertices) > 0 .and. &
                size(plot%pcolormesh_data%y_vertices) > 0) then
                if (first_plot) then
                    x_min_data = minval(plot%pcolormesh_data%x_vertices)
                    x_max_data = maxval(plot%pcolormesh_data%x_vertices)
                    y_min_data = minval(plot%pcolormesh_data%y_vertices)
                    y_max_data = maxval(plot%pcolormesh_data%y_vertices)
                    first_plot = .false.
                else
                    x_min_data = min(x_min_data, minval(plot%pcolormesh_data%x_vertices))
                    x_max_data = max(x_max_data, maxval(plot%pcolormesh_data%x_vertices))
                    y_min_data = min(y_min_data, minval(plot%pcolormesh_data%y_vertices))
                    y_max_data = max(y_max_data, maxval(plot%pcolormesh_data%y_vertices))
                end if
                has_valid_data = .true.
            end if
        end if
    end subroutine process_pcolormesh_ranges
    
    subroutine apply_single_point_margins(has_valid_data, x_min_data, x_max_data, &
                                         y_min_data, y_max_data)
        !! Apply margins for single point case (zero range)
        logical, intent(in) :: has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data
        
        real(wp) :: range_x, range_y, margin_factor
        
        ! Default margin for single points and empty data (10% of unit range)
        margin_factor = 0.1_wp
        
        ! CRITICAL FIX: Handle single point case (zero range)
        if (has_valid_data) then
            range_x = x_max_data - x_min_data
            range_y = y_max_data - y_min_data
            
            ! If range is zero or very small (single point case), add margins
            if (abs(range_x) < 1.0e-10_wp) then
                if (abs(x_min_data) < 1.0e-10_wp) then
                    ! Point at origin, use symmetric range
                    x_min_data = -margin_factor
                    x_max_data = margin_factor
                else
                    ! Point not at origin, use percentage-based margin
                    range_x = abs(x_min_data) * margin_factor
                    x_min_data = x_min_data - range_x
                    x_max_data = x_max_data + range_x
                end if
            end if
            
            if (abs(range_y) < 1.0e-10_wp) then
                if (abs(y_min_data) < 1.0e-10_wp) then
                    ! Point at origin, use symmetric range
                    y_min_data = -margin_factor
                    y_max_data = margin_factor
                else
                    ! Point not at origin, use percentage-based margin
                    range_y = abs(y_min_data) * margin_factor
                    y_min_data = y_min_data - range_y
                    y_max_data = y_max_data + range_y
                end if
            end if
        end if
    end subroutine apply_single_point_margins
    
    subroutine finalize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                   x_min_data, x_max_data, y_min_data, y_max_data, &
                                   x_min_transformed, x_max_transformed, &
                                   y_min_transformed, y_max_transformed, &
                                   xscale, yscale, symlog_threshold)
        !! Apply user limits and scale transformations
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        real(wp), intent(in) :: x_min_data, x_max_data, y_min_data, y_max_data
        real(wp), intent(out) :: x_min_transformed, x_max_transformed
        real(wp), intent(out) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        
        ! Apply user-specified limits or use calculated data ranges
        if (.not. xlim_set) then
            x_min = x_min_data
            x_max = x_max_data
        end if
        
        if (.not. ylim_set) then
            y_min = y_min_data
            y_max = y_max_data
        end if
        
        ! Apply scale transformations
        x_min_transformed = apply_scale_transform(x_min, xscale, symlog_threshold)
        x_max_transformed = apply_scale_transform(x_max, xscale, symlog_threshold)
        y_min_transformed = apply_scale_transform(y_min, yscale, symlog_threshold)
        y_max_transformed = apply_scale_transform(y_max, yscale, symlog_threshold)
    end subroutine finalize_data_ranges
    
    subroutine setup_coordinate_system(backend, x_min_transformed, x_max_transformed, &
                                      y_min_transformed, y_max_transformed)
        !! Setup the coordinate system for rendering
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x_min_transformed, x_max_transformed
        real(wp), intent(in) :: y_min_transformed, y_max_transformed
        
        ! Set data ranges directly on backend
        backend%x_min = x_min_transformed
        backend%x_max = x_max_transformed
        backend%y_min = y_min_transformed
        backend%y_max = y_max_transformed
    end subroutine setup_coordinate_system
    
    subroutine render_figure_background(backend)
        !! Render figure background
        class(plot_context), intent(inout) :: backend
        ! Background clearing is handled by backend-specific rendering
    end subroutine render_figure_background
    
    subroutine render_figure_axes(backend, xscale, yscale, symlog_threshold, &
                                 x_min, x_max, y_min, y_max, title, xlabel, ylabel)
        !! Render figure axes and labels
        class(plot_context), intent(inout) :: backend
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in) :: title, xlabel, ylabel
        
        ! Draw axes using backend's polymorphic method
        call backend%draw_axes_and_labels_backend(xscale, yscale, &
                                                 symlog_threshold, &
                                                 x_min, x_max, &
                                                 y_min, y_max, &
                                                 title, xlabel, ylabel, &
                                                 z_min=0.0_wp, z_max=1.0_wp, &
                                                 has_3d_plots=.false.)
    end subroutine render_figure_axes
    
    subroutine render_all_plots(backend, plots, plot_count, &
                               x_min_transformed, x_max_transformed, &
                               y_min_transformed, y_max_transformed, &
                               xscale, yscale, symlog_threshold, &
                               width, height, margin_left, margin_right, &
                               margin_bottom, margin_top)
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
        
        integer :: i
        
        do i = 1, plot_count
            select case (plots(i)%plot_type)
            case (PLOT_TYPE_LINE)
                call render_line_plot(backend, plots(i), i, &
                                    x_min_transformed, x_max_transformed, &
                                    y_min_transformed, y_max_transformed, &
                                    xscale, yscale, symlog_threshold)
                
                if (allocated(plots(i)%marker)) then
                    call render_markers(backend, plots(i), &
                                      x_min_transformed, x_max_transformed, &
                                      y_min_transformed, y_max_transformed, &
                                      xscale, yscale, symlog_threshold)
                end if
                
            case (PLOT_TYPE_CONTOUR)
                call render_contour_plot(backend, plots(i), &
                                       x_min_transformed, x_max_transformed, &
                                       y_min_transformed, y_max_transformed, &
                                       xscale, yscale, symlog_threshold, &
                                       width, height, &
                                       margin_left, margin_right, &
                                       margin_bottom, margin_top)
                
            case (PLOT_TYPE_PCOLORMESH)
                call render_pcolormesh_plot(backend, plots(i), &
                                          x_min_transformed, x_max_transformed, &
                                          y_min_transformed, y_max_transformed, &
                                          xscale, yscale, symlog_threshold, &
                                          width, height, margin_right)
            end select
        end do
    end subroutine render_all_plots

end module fortplot_figure_rendering_pipeline