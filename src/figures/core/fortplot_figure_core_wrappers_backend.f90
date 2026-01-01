submodule(fortplot_figure_core) fortplot_figure_core_wrappers_backend

    implicit none

contains

    module subroutine setup_png_backend_for_animation(self)
        class(figure_t), intent(inout) :: self
        call core_setup_png_backend_for_animation(self%state)
    end subroutine setup_png_backend_for_animation

    module subroutine extract_rgb_data_for_animation(self, rgb_data)
        class(figure_t), intent(inout) :: self
        real(wp), intent(out) :: rgb_data(:, :, :)
        call core_extract_rgb_data_for_animation(self%state, rgb_data, &
                                                 self%plots, self%state%plot_count, &
                                                 self%annotations, &
                                                 self%annotation_count, &
                                                 self%state%rendered)
    end subroutine extract_rgb_data_for_animation

    module subroutine extract_png_data_for_animation(self, png_data, status)
        class(figure_t), intent(inout) :: self
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        call core_extract_png_data_for_animation(self%state, png_data, status, &
                                                 self%plots, self%state%plot_count, &
                                                 self%annotations, &
                                                 self%annotation_count, &
                                                 self%state%rendered)
    end subroutine extract_png_data_for_animation

    module subroutine backend_color(self, r, g, b)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: r, g, b
        call core_backend_color(self%state, r, g, b)
    end subroutine backend_color

    module function backend_associated(self) result(is_associated)
        class(figure_t), intent(in) :: self
        logical :: is_associated
        is_associated = core_backend_associated(self%state)
    end function backend_associated

    module subroutine backend_line(self, x1, y1, x2, y2)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x1, y1, x2, y2
        call core_backend_line(self%state, x1, y1, x2, y2)
    end subroutine backend_line

    module subroutine backend_arrow(self, x, y, dx, dy, size, style)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        call core_backend_arrow(self%state, x, y, dx, dy, size, style)
    end subroutine backend_arrow

    module subroutine clear_backend_arrows(self)
        class(figure_t), intent(inout) :: self
        logical :: had_arrows

        had_arrows = .false.
        if (allocated(self%state%stream_arrows)) then
            had_arrows = size(self%state%stream_arrows) > 0
            deallocate (self%state%stream_arrows)
        end if

        if (had_arrows) self%state%rendered = .false.
    end subroutine clear_backend_arrows

    module function get_x_min(self) result(x_min)
        class(figure_t), intent(in) :: self
        real(wp) :: x_min
        x_min = core_get_x_min(self%state)
    end function get_x_min

    module function get_x_max(self) result(x_max)
        class(figure_t), intent(in) :: self
        real(wp) :: x_max
        x_max = core_get_x_max(self%state)
    end function get_x_max

    module function get_y_min(self) result(y_min)
        class(figure_t), intent(in) :: self
        real(wp) :: y_min
        y_min = core_get_y_min(self%state)
    end function get_y_min

    module function get_y_max(self) result(y_max)
        class(figure_t), intent(in) :: self
        real(wp) :: y_max
        y_max = core_get_y_max(self%state)
    end function get_y_max

end submodule fortplot_figure_core_wrappers_backend
