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
        !! Calculate overall data ranges for the figure
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        real(wp), intent(out) :: x_min_transformed, x_max_transformed
        real(wp), intent(out) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        
        real(wp) :: x_min_data, x_max_data, y_min_data, y_max_data
        integer :: i
        logical :: first_plot
        
        if (xlim_set .and. ylim_set) then
            x_min_transformed = apply_scale_transform(x_min, xscale, symlog_threshold)
            x_max_transformed = apply_scale_transform(x_max, xscale, symlog_threshold)
            y_min_transformed = apply_scale_transform(y_min, yscale, symlog_threshold)
            y_max_transformed = apply_scale_transform(y_max, yscale, symlog_threshold)
            return
        end if
        
        first_plot = .true.
        
        do i = 1, plot_count
            select case (plots(i)%plot_type)
            case (PLOT_TYPE_LINE)
                if (allocated(plots(i)%x) .and. allocated(plots(i)%y)) then
                    if (first_plot) then
                        x_min_data = minval(plots(i)%x)
                        x_max_data = maxval(plots(i)%x)
                        y_min_data = minval(plots(i)%y)
                        y_max_data = maxval(plots(i)%y)
                        first_plot = .false.
                    else
                        x_min_data = min(x_min_data, minval(plots(i)%x))
                        x_max_data = max(x_max_data, maxval(plots(i)%x))
                        y_min_data = min(y_min_data, minval(plots(i)%y))
                        y_max_data = max(y_max_data, maxval(plots(i)%y))
                    end if
                end if
                
            case (PLOT_TYPE_CONTOUR)
                if (allocated(plots(i)%x_grid) .and. allocated(plots(i)%y_grid)) then
                    if (first_plot) then
                        x_min_data = minval(plots(i)%x_grid)
                        x_max_data = maxval(plots(i)%x_grid)
                        y_min_data = minval(plots(i)%y_grid)
                        y_max_data = maxval(plots(i)%y_grid)
                        first_plot = .false.
                    else
                        x_min_data = min(x_min_data, minval(plots(i)%x_grid))
                        x_max_data = max(x_max_data, maxval(plots(i)%x_grid))
                        y_min_data = min(y_min_data, minval(plots(i)%y_grid))
                        y_max_data = max(y_max_data, maxval(plots(i)%y_grid))
                    end if
                end if
                
            case (PLOT_TYPE_PCOLORMESH)
                if (allocated(plots(i)%pcolormesh_data%x_vertices) .and. &
                    allocated(plots(i)%pcolormesh_data%y_vertices)) then
                    if (first_plot) then
                        x_min_data = minval(plots(i)%pcolormesh_data%x_vertices)
                        x_max_data = maxval(plots(i)%pcolormesh_data%x_vertices)
                        y_min_data = minval(plots(i)%pcolormesh_data%y_vertices)
                        y_max_data = maxval(plots(i)%pcolormesh_data%y_vertices)
                        first_plot = .false.
                    else
                        x_min_data = min(x_min_data, minval(plots(i)%pcolormesh_data%x_vertices))
                        x_max_data = max(x_max_data, maxval(plots(i)%pcolormesh_data%x_vertices))
                        y_min_data = min(y_min_data, minval(plots(i)%pcolormesh_data%y_vertices))
                        y_max_data = max(y_max_data, maxval(plots(i)%pcolormesh_data%y_vertices))
                    end if
                end if
            end select
        end do
        
        ! Apply user-specified limits or use data ranges
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
    end subroutine calculate_figure_data_ranges
    
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