module fortplot_figure_grid
    !! Figure grid functionality module
    !! 
    !! Single Responsibility: Handle grid configuration and rendering
    !! Extracted from fortplot_figure_core to improve modularity
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_axes, only: compute_scale_ticks
    use fortplot_scales, only: apply_scale_transform
    implicit none
    
    private
    public :: configure_grid, render_grid_lines
    
contains
    
    subroutine configure_grid(grid_enabled, grid_which, grid_axis, grid_alpha, &
                             grid_linestyle, enabled, which, axis, alpha, linestyle)
        !! Configure grid settings
        logical, intent(inout) :: grid_enabled
        character(len=10), intent(inout) :: grid_which
        character(len=1), intent(inout) :: grid_axis
        real(wp), intent(inout) :: grid_alpha
        character(len=10), intent(inout) :: grid_linestyle
        logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha
        
        ! Handle boolean toggle syntax
        if (present(enabled)) then
            grid_enabled = enabled
        end if
        
        ! Handle string-based which parameter
        if (present(which)) then
            grid_which = which
            grid_enabled = .true.  ! Implicitly enable grid
        end if
        
        ! Handle axis parameter with flexible input
        if (present(axis)) then
            select case(trim(axis))
            case('x')
                grid_axis = 'x'
            case('y') 
                grid_axis = 'y'
            case('both', 'b')
                grid_axis = 'b'
            case default
                grid_axis = 'b'
            end select
            grid_enabled = .true.  ! Implicitly enable grid
        end if
        
        ! Handle alpha parameter
        if (present(alpha)) then
            grid_alpha = max(0.0_wp, min(1.0_wp, alpha))
            grid_enabled = .true.  ! Implicitly enable grid
        end if
        
        ! Handle linestyle parameter
        if (present(linestyle)) then
            grid_linestyle = linestyle
            grid_enabled = .true.  ! Implicitly enable grid
        end if
    end subroutine configure_grid
    
    subroutine render_grid_lines(backend, grid_enabled, grid_which, grid_axis, &
                                grid_alpha, width, height, margin_left, margin_right, &
                                margin_bottom, margin_top, xscale, yscale, &
                                symlog_threshold, x_min, x_max, y_min, y_max, &
                                x_min_transformed, x_max_transformed, &
                                y_min_transformed, y_max_transformed)
        !! Render grid lines on the figure
        class(plot_context), intent(inout) :: backend
        logical, intent(in) :: grid_enabled
        character(len=10), intent(in) :: grid_which
        character(len=1), intent(in) :: grid_axis
        real(wp), intent(in) :: grid_alpha
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(in) :: x_min_transformed, x_max_transformed
        real(wp), intent(in) :: y_min_transformed, y_max_transformed
        
        real(wp) :: major_ticks(50)
        real(wp) :: tick_val, x1, y1, x2, y2, alpha_val
        integer :: i, num_ticks
        real(wp) :: grid_color(3)
        
        if (.not. grid_enabled) return
        
        ! Set grid color (gray) and alpha (alpha currently backend-specific; kept for API parity)
        grid_color = [0.7_wp, 0.7_wp, 0.7_wp]
        alpha_val = grid_alpha

        ! Draw X-axis grid lines (vertical lines)
        if (grid_axis == 'x' .or. grid_axis == 'b') then
            ! Get major ticks for x-axis
            call compute_scale_ticks(xscale, x_min, x_max, &
                                   symlog_threshold, major_ticks, num_ticks)
            
            ! Draw major tick grid lines
            if (grid_which == 'major' .or. grid_which == 'both') then
                do i = 1, num_ticks
                    tick_val = major_ticks(i)
                    if (tick_val >= x_min .and. tick_val <= x_max) then
                        ! Transform tick value to backend data coordinates
                        tick_val = apply_scale_transform(tick_val, xscale, symlog_threshold)
                        
                        ! In backend data coordinates, a vertical grid line is x = tick_val
                        x1 = tick_val
                        y1 = y_min_transformed
                        x2 = tick_val
                        y2 = y_max_transformed

                        ! Draw grid line in backend coordinate space
                        call backend%color(grid_color(1), grid_color(2), grid_color(3))
                        call backend%line(x1, y1, x2, y2)
                    end if
                end do
            end if
        end if
        
        ! Draw Y-axis grid lines (horizontal lines)
        if (grid_axis == 'y' .or. grid_axis == 'b') then
            ! Get major ticks for y-axis
            call compute_scale_ticks(yscale, y_min, y_max, &
                                   symlog_threshold, major_ticks, num_ticks)
            
            ! Draw major tick grid lines
            if (grid_which == 'major' .or. grid_which == 'both') then
                do i = 1, num_ticks
                    tick_val = major_ticks(i)
                    if (tick_val >= y_min .and. tick_val <= y_max) then
                        ! Transform tick value to backend data coordinates
                        tick_val = apply_scale_transform(tick_val, yscale, symlog_threshold)

                        ! In backend data coordinates, a horizontal grid line is y = tick_val
                        x1 = x_min_transformed
                        y1 = tick_val
                        x2 = x_max_transformed
                        y2 = tick_val

                        ! Draw grid line in backend coordinate space
                        call backend%color(grid_color(1), grid_color(2), grid_color(3))
                        call backend%line(x1, y1, x2, y2)
                    end if
                end do
            end if
        end if
        
    end subroutine render_grid_lines

end module fortplot_figure_grid
