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
    public :: render_solid_line
    
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
        
        ! CRITICAL FIX #857: Set line color from plot data before drawing
        call backend%color(plot_data%color(1), plot_data%color(2), plot_data%color(3))
        
        ! Apply scaling transformations
        do i = 1, n
            x_scaled(i) = apply_scale_transform(plot_data%x(i), xscale, symlog_threshold)
            y_scaled(i) = apply_scale_transform(plot_data%y(i), yscale, symlog_threshold)
        end do
        
        ! Delegate line style handling to backend to avoid duplication and
        ! ensure consistent dash/dot appearance across outputs.
        if (allocated(plot_data%linestyle) .and. len_trim(plot_data%linestyle) > 0) then
            select case (trim(plot_data%linestyle))
            case ('none', 'None')
                ! Skip drawing connecting lines; markers rendered separately
            case default
                call backend%set_line_style(trim(plot_data%linestyle))
                call render_solid_line(backend, x_scaled, y_scaled)
            end select
        else
            call backend%set_line_style('-')
            call render_solid_line(backend, x_scaled, y_scaled)
        end if
    end subroutine render_line_plot

    ! Removed style emulation here; styles are handled by backend

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

    ! Removed patterned rendering; use backend pattern implementation instead

end module fortplot_line_rendering
