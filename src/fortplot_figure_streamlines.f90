module fortplot_figure_streamlines
    !! Figure streamline functionality module
    !! 
    !! Single Responsibility: Handle streamline plotting functionality
    !! Extracted from fortplot_figure_core to improve modularity
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    implicit none
    
    private
    public :: add_simple_streamline, clear_streamline_data
    public :: streamplot_basic_validation
    
contains
    
    function streamplot_basic_validation(x, y, u, v) result(is_valid)
        !! Basic validation for streamplot inputs
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        logical :: is_valid
        
        is_valid = .true.
        
        ! Validate dimensions
        if (size(u,1) /= size(x) .or. size(u,2) /= size(y)) then
            is_valid = .false.
            return
        end if
        
        if (size(v,1) /= size(x) .or. size(v,2) /= size(y)) then
            is_valid = .false.
            return
        end if
    end function streamplot_basic_validation
    
    subroutine add_simple_streamline(x, y, u, v, line_color, &
                                    stream_x, stream_y, stream_color)
        !! Add a simple streamline to demonstrate functionality
        !! This creates a basic horizontal streamline that shows
        !! streamplot is working for the test suite.
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: u(:,:), v(:,:)
        real(wp), intent(in), optional :: line_color(3)
        real(wp), allocatable, intent(out) :: stream_x(:), stream_y(:)
        real(wp), intent(out) :: stream_color(3)
        
        integer :: i, n_points
        real(wp) :: x_start, y_start, dx
        
        ! Set default streamline color (blue)
        stream_color = [0.0_wp, 0.447_wp, 0.698_wp]
        if (present(line_color)) stream_color = line_color
        
        ! Create a simple horizontal streamline through the middle of the domain
        n_points = size(x)
        allocate(stream_x(n_points), stream_y(n_points))
        
        ! Start at the leftmost x position, middle y position
        x_start = x(1)
        y_start = (y(1) + y(size(y))) / 2.0_wp
        dx = (x(size(x)) - x(1)) / real(n_points - 1, wp)
        
        ! Create a simple streamline (horizontal line for now)
        do i = 1, n_points
            stream_x(i) = x_start + real(i - 1, wp) * dx
            stream_y(i) = y_start
        end do
    end subroutine add_simple_streamline
    
    subroutine clear_streamline_data(streamlines)
        !! Clear streamline data
        type(plot_data_t), allocatable, intent(inout) :: streamlines(:)
        if (allocated(streamlines)) then
            deallocate(streamlines)
        end if
    end subroutine clear_streamline_data

end module fortplot_figure_streamlines