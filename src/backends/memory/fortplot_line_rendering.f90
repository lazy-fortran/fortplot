module fortplot_line_rendering
    !! Line plot rendering module
    !! 
    !! This module handles all line-based rendering operations including
    !! solid lines, patterned lines, and line segments.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_utils
    use fortplot_plot_data
    use fortplot_coordinate_validation, only: validate_coordinate_arrays
    implicit none
    
    private
    public :: render_line_plot
    public :: draw_line_with_style
    public :: render_solid_line
    public :: render_patterned_line
    
contains
    
    subroutine render_line_plot(backend, plot_data, plot_idx, x_min_t, x_max_t, y_min_t, y_max_t, xscale, yscale, symlog_threshold)
        !! Render a line plot with proper scaling and clipping
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        integer, intent(in) :: plot_idx
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        
        real(wp), allocatable :: x_scaled(:), y_scaled(:)
        integer :: i, n
        
        ! Validate input data
        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) return
        if (size(plot_data%x) == 0 .or. size(plot_data%y) == 0) return
        if (size(plot_data%x) /= size(plot_data%y)) return
        
        n = size(plot_data%x)
        allocate(x_scaled(n), y_scaled(n))
        
        ! Apply scaling transformations
        do i = 1, n
            x_scaled(i) = apply_scale_transform(plot_data%x(i), xscale, symlog_threshold)
            y_scaled(i) = apply_scale_transform(plot_data%y(i), yscale, symlog_threshold)
        end do
        
        ! Check if we need to use linestyle
        if (allocated(plot_data%linestyle) .and. len_trim(plot_data%linestyle) > 0) then
            call draw_line_with_style(backend, x_scaled, y_scaled, &
                                    plot_data%linestyle, '')
        else
            call render_solid_line(backend, x_scaled, y_scaled)
        end if
    end subroutine render_line_plot

    subroutine draw_line_with_style(backend, x, y, linestyle, color)
        !! Draw line with specific style (solid, dashed, etc.)
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: linestyle
        character(len=*), intent(in) :: color
        
        ! Set line properties - parsing color string to RGB would be needed here
        ! For now, skip the color setting as it would require more complex parsing
        
        ! Draw line based on style
        select case (trim(linestyle))
        case ('--')
            call render_patterned_line(backend, x, y, 'dashed')
        case (':')
            call render_patterned_line(backend, x, y, 'dotted')
        case ('-.')
            call render_patterned_line(backend, x, y, 'dashdot')
        case default
            call render_solid_line(backend, x, y)
        end select
    end subroutine draw_line_with_style

    subroutine render_solid_line(backend, x, y)
        !! Render a solid line connecting all points
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x(:), y(:)
        integer :: i
        
        if (size(x) < 2) return
        
        do i = 1, size(x) - 1
            call backend%line(x(i), y(i), x(i+1), y(i+1))
        end do
    end subroutine render_solid_line

    subroutine render_patterned_line(backend, x, y, pattern)
        !! Render a patterned line (dashed, dotted, dash-dot)
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: pattern
        
        integer :: i
        real(wp) :: segment_length, total_length, current_length
        logical :: draw_segment
        
        if (size(x) < 2) return
        
        total_length = 0.0_wp
        draw_segment = .true.
        
        ! Calculate total line length for pattern scaling
        do i = 1, size(x) - 1
            total_length = total_length + sqrt((x(i+1) - x(i))**2 + (y(i+1) - y(i))**2)
        end do
        
        ! Set pattern parameters based on type
        select case (trim(pattern))
        case ('dashed')
            segment_length = total_length / 20.0_wp
        case ('dotted')
            segment_length = total_length / 40.0_wp
        case ('dashdot')
            segment_length = total_length / 30.0_wp
        case default
            call render_solid_line(backend, x, y)
            return
        end select
        
        current_length = 0.0_wp
        
        do i = 1, size(x) - 1
            call render_segment_with_pattern(backend, x(i), y(i), x(i+1), y(i+1), &
                                           segment_length, current_length, draw_segment, pattern)
        end do
    end subroutine render_patterned_line

    subroutine render_segment_with_pattern(backend, x1, y1, x2, y2, segment_length, &
                                         current_length, draw_segment, pattern)
        !! Render a single line segment with pattern
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp), intent(in) :: segment_length
        real(wp), intent(inout) :: current_length
        logical, intent(inout) :: draw_segment
        character(len=*), intent(in) :: pattern
        
        real(wp) :: dx, dy, total_seg_length, step_size
        real(wp) :: pattern_length, gap_length
        integer :: num_steps, step
        real(wp) :: x_current, y_current, x_next, y_next
        real(wp) :: seg_progress
        
        dx = x2 - x1
        dy = y2 - y1
        total_seg_length = sqrt(dx**2 + dy**2)
        
        if (total_seg_length == 0.0_wp) return
        
        ! Set pattern-specific lengths
        select case (trim(pattern))
        case ('dashed')
            pattern_length = segment_length
            gap_length = segment_length
        case ('dotted')
            pattern_length = segment_length * 0.3_wp
            gap_length = segment_length * 0.7_wp
        case ('dashdot')
            pattern_length = segment_length * 0.6_wp
            gap_length = segment_length * 0.4_wp
        case default
            pattern_length = segment_length
            gap_length = 0.0_wp
        end select
        
        num_steps = max(1, int(total_seg_length / (segment_length * 0.1_wp)))
        step_size = total_seg_length / real(num_steps, wp)
        
        x_current = x1
        y_current = y1
        
        do step = 1, num_steps
            seg_progress = real(step, wp) / real(num_steps, wp)
            x_next = x1 + dx * seg_progress
            y_next = y1 + dy * seg_progress
            
            current_length = current_length + step_size
            
            if (draw_segment) then
                call backend%line(x_current, y_current, x_next, y_next)
                if (current_length >= pattern_length) then
                    current_length = 0.0_wp
                    draw_segment = .false.
                end if
            else
                ! Skip drawing this segment (gap)
                if (current_length >= gap_length) then
                    current_length = 0.0_wp
                    draw_segment = .true.
                end if
            end if
            
            x_current = x_next
            y_current = y_next
        end do
    end subroutine render_segment_with_pattern

end module fortplot_line_rendering