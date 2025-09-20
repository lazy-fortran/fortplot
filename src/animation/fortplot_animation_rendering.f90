module fortplot_animation_rendering
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t, plot_data_t
    use fortplot_utils, only: initialize_backend
    implicit none
    private

    public :: extract_frame_rgb_data
    public :: render_frame_to_png
    public :: render_figure_components
    public :: data_to_screen_coords

contains

    subroutine extract_frame_rgb_data(fig, rgb_data, status)
        type(figure_t), intent(inout) :: fig
        real(wp), intent(out) :: rgb_data(:,:,:)
        integer, intent(out) :: status
        
        status = 0
        
        ! Setup PNG backend and extract data using new methods
        call fig%setup_png_backend_for_animation()
        call fig%extract_rgb_data_for_animation(rgb_data)
    end subroutine extract_frame_rgb_data

    subroutine render_frame_to_png(fig, png_data, status)
        type(figure_t), intent(inout) :: fig
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        call fig%setup_png_backend_for_animation()
        call fig%extract_png_data_for_animation(png_data, status)
    end subroutine render_frame_to_png

    ! Removed unused internal stubs: setup_png_backend, render_to_backend, extract_png_data

    subroutine render_figure_components(fig)
        type(figure_t), intent(inout) :: fig
        
        if (.not. fig%backend_associated()) return
        
        call render_background(fig)
        call render_all_plots(fig)
        call fig%set_rendered(.true.)
    end subroutine render_figure_components

    subroutine render_background(fig)
        type(figure_t), intent(inout) :: fig
        
        call fig%backend_color(1.0_wp, 1.0_wp, 1.0_wp)
    end subroutine render_background

    subroutine render_all_plots(fig)
        type(figure_t), intent(inout) :: fig
        integer :: i
        type(plot_data_t), allocatable :: plots(:)
        
        plots = fig%get_plots()
        do i = 1, fig%get_plot_count()
            call render_single_plot(fig, plots(i))
        end do
    end subroutine render_all_plots

    subroutine render_single_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        select case (plot_data%plot_type)
        case (1) ! PLOT_TYPE_LINE
            call render_line_plot(fig, plot_data)
        case (2) ! PLOT_TYPE_CONTOUR  
            call render_contour_plot(fig, plot_data)
        case (3) ! PLOT_TYPE_PCOLORMESH
            call render_pcolormesh_plot(fig, plot_data)
        case (4) ! PLOT_TYPE_ERRORBAR
            call render_errorbar_plot(fig, plot_data)
        case (5) ! PLOT_TYPE_BAR
            call render_bar_plot(fig, plot_data)
        case (6) ! PLOT_TYPE_HISTOGRAM
            call render_histogram_plot(fig, plot_data)
        end select
    end subroutine render_single_plot

    ! Removed unused mark_as_rendered

    subroutine render_line_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        if (.not. is_valid_line_data(plot_data)) return
        
        call set_plot_color(fig, plot_data)
        call draw_line_segments(fig, plot_data)
    end subroutine render_line_plot

    subroutine render_errorbar_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        if (.not. is_valid_errorbar_data(plot_data)) return
        
        ! Errorbar rendering in animation is simplified for performance
        ! Just draw the base line
        call set_plot_color(fig, plot_data)
        call draw_line_segments(fig, plot_data)
    end subroutine render_errorbar_plot

    function is_valid_errorbar_data(plot_data) result(valid)
        type(plot_data_t), intent(in) :: plot_data
        logical :: valid
        
        valid = allocated(plot_data%x) .and. allocated(plot_data%y)
        if (.not. valid) return
        
        valid = size(plot_data%x) > 0 .and. size(plot_data%y) > 0
        if (.not. valid) return
        
        valid = size(plot_data%x) == size(plot_data%y)
    end function is_valid_errorbar_data

    function is_valid_line_data(plot_data) result(valid)
        type(plot_data_t), intent(in) :: plot_data
        logical :: valid
        
        valid = allocated(plot_data%x) .and. allocated(plot_data%y) .and. size(plot_data%x) >= 2
    end function is_valid_line_data

    subroutine set_plot_color(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        call fig%backend_color(plot_data%color(1), plot_data%color(2), plot_data%color(3))
    end subroutine set_plot_color

    subroutine draw_line_segments(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        integer :: i
        real(wp) :: x_screen, y_screen, x_prev, y_prev
        
        call data_to_screen_coords(fig, plot_data%x(1), plot_data%y(1), x_prev, y_prev)
        
        do i = 2, size(plot_data%x)
            call data_to_screen_coords(fig, plot_data%x(i), plot_data%y(i), x_screen, y_screen)
            call fig%backend_line(x_prev, y_prev, x_screen, y_screen)
            x_prev = x_screen
            y_prev = y_screen
        end do
    end subroutine draw_line_segments

    subroutine render_contour_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        ! Simplified contour rendering for animation - draw as wireframe
        if (.not. is_valid_2d_data(plot_data)) return
        
        call set_plot_color(fig, plot_data)
        call draw_2d_wireframe(fig, plot_data)
    end subroutine render_contour_plot

    subroutine render_pcolormesh_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        ! Simplified pcolormesh rendering for animation - draw as grid
        if (.not. is_valid_2d_data(plot_data)) return
        
        call set_plot_color(fig, plot_data)
        call draw_mesh_grid(fig, plot_data)
    end subroutine render_pcolormesh_plot

    subroutine render_bar_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        ! Simplified bar plot rendering for animation - draw as vertical lines
        if (.not. is_valid_line_data(plot_data)) return
        
        call set_plot_color(fig, plot_data)
        call draw_vertical_bars(fig, plot_data)
    end subroutine render_bar_plot

    subroutine render_histogram_plot(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        ! Simplified histogram rendering for animation - draw as bars
        if (.not. is_valid_line_data(plot_data)) return
        
        call set_plot_color(fig, plot_data)
        call draw_histogram_bars(fig, plot_data)
    end subroutine render_histogram_plot

    subroutine data_to_screen_coords(fig, x_data, y_data, x_screen, y_screen)
        type(figure_t), intent(in) :: fig
        real(wp), intent(in) :: x_data, y_data
        real(wp), intent(out) :: x_screen, y_screen
        
        ! Simple linear mapping from data to screen coordinates
        ! This is a simplified version - the actual implementation would handle
        ! logarithmic scales, margins, etc.
        x_screen = real(fig%get_width(), wp) * (x_data - fig%get_x_min()) / (fig%get_x_max() - fig%get_x_min())
        y_screen = real(fig%get_height(), wp) * (1.0_wp - (y_data - fig%get_y_min()) / (fig%get_y_max() - fig%get_y_min()))
    end subroutine data_to_screen_coords

    function is_valid_2d_data(plot_data) result(valid)
        type(plot_data_t), intent(in) :: plot_data
        logical :: valid
        
        valid = allocated(plot_data%x) .and. allocated(plot_data%y)
        if (.not. valid) return
        
        valid = size(plot_data%x) > 1 .and. size(plot_data%y) > 1
    end function is_valid_2d_data

    subroutine draw_2d_wireframe(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        integer :: i, j, nx, ny
        real(wp) :: x1, y1, x2, y2
        
        nx = size(plot_data%x)
        ny = size(plot_data%y)
        
        ! Draw horizontal lines
        do j = 1, ny
            do i = 1, nx - 1
                call data_to_screen_coords(fig, plot_data%x(i), plot_data%y(j), x1, y1)
                call data_to_screen_coords(fig, plot_data%x(i+1), plot_data%y(j), x2, y2)
                call fig%backend_line(x1, y1, x2, y2)
            end do
        end do
        
        ! Draw vertical lines
        do i = 1, nx
            do j = 1, ny - 1
                call data_to_screen_coords(fig, plot_data%x(i), plot_data%y(j), x1, y1)
                call data_to_screen_coords(fig, plot_data%x(i), plot_data%y(j+1), x2, y2)
                call fig%backend_line(x1, y1, x2, y2)
            end do
        end do
    end subroutine draw_2d_wireframe

    subroutine draw_mesh_grid(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        
        ! For animation, mesh grid is same as wireframe for simplicity
        call draw_2d_wireframe(fig, plot_data)
    end subroutine draw_mesh_grid

    subroutine draw_vertical_bars(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        integer :: i
        real(wp) :: x_screen, y_base, y_top
        
        do i = 1, size(plot_data%x)
            call data_to_screen_coords(fig, plot_data%x(i), 0.0_wp, x_screen, y_base)
            call data_to_screen_coords(fig, plot_data%x(i), plot_data%y(i), x_screen, y_top)
            call fig%backend_line(x_screen, y_base, x_screen, y_top)
        end do
    end subroutine draw_vertical_bars

    subroutine draw_histogram_bars(fig, plot_data)
        type(figure_t), intent(inout) :: fig
        type(plot_data_t), intent(in) :: plot_data
        integer :: i
        real(wp) :: x1, x2, y_base, y_top, bar_width
        
        if (size(plot_data%x) < 2) return
        
        bar_width = (plot_data%x(2) - plot_data%x(1)) * 0.8_wp
        
        do i = 1, size(plot_data%x)
            call data_to_screen_coords(fig, plot_data%x(i) - bar_width/2, 0.0_wp, x1, y_base)
            call data_to_screen_coords(fig, plot_data%x(i) + bar_width/2, 0.0_wp, x2, y_base)
            call data_to_screen_coords(fig, plot_data%x(i) - bar_width/2, plot_data%y(i), x1, y_top)
            call data_to_screen_coords(fig, plot_data%x(i) + bar_width/2, plot_data%y(i), x2, y_top)
            
            ! Draw rectangle outline for histogram bar
            call fig%backend_line(x1, y_base, x2, y_base)  ! bottom
            call fig%backend_line(x2, y_base, x2, y_top)   ! right
            call fig%backend_line(x2, y_top, x1, y_top)    ! top
            call fig%backend_line(x1, y_top, x1, y_base)   ! left
        end do
    end subroutine draw_histogram_bars

end module fortplot_animation_rendering
