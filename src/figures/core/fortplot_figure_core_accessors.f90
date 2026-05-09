module fortplot_figure_core_accessors
    !! Property accessor methods extracted from fortplot_figure_core
    !!
    !! This module contains property accessor methods for the core figure
    !! to maintain architectural compliance with size limits.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_annotations, only: text_annotation_t
    use fortplot_plot_data, only: plot_data_t, arrow_data_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_properties
    use fortplot_figure_management
    use fortplot_figure_operations, only: figure_render
    implicit none

    private
    public :: core_get_width, core_get_height, core_get_rendered, core_set_rendered
    public :: core_get_plot_count, core_get_plots, core_get_x_min, core_get_x_max
    public :: core_get_y_min, core_get_y_max, core_backend_color, &
              core_backend_associated
    public :: core_backend_line, core_setup_png_backend_for_animation, &
              core_backend_arrow
    public :: core_extract_rgb_data_for_animation, core_extract_png_data_for_animation

contains

    function core_get_width(state) result(width)
        type(figure_state_t), intent(in) :: state
        integer :: width
        width = figure_get_width(state)
    end function core_get_width

    function core_get_height(state) result(height)
        type(figure_state_t), intent(in) :: state
        integer :: height
        height = figure_get_height(state)
    end function core_get_height

    function core_get_rendered(state) result(rendered)
        type(figure_state_t), intent(in) :: state
        logical :: rendered
        rendered = figure_get_rendered(state)
    end function core_get_rendered

    subroutine core_set_rendered(state, rendered)
        type(figure_state_t), intent(inout) :: state
        logical, intent(in) :: rendered
        call figure_set_rendered(state, rendered)
    end subroutine core_set_rendered

    function core_get_plot_count(state) result(plot_count)
        type(figure_state_t), intent(in) :: state
        integer :: plot_count
        plot_count = figure_get_plot_count(state)
    end function core_get_plot_count

    function core_get_plots(plots) result(plots_ptr)
        type(plot_data_t), intent(in), target :: plots(:)
        type(plot_data_t), pointer :: plots_ptr(:)
        plots_ptr => figure_get_plots(plots)
    end function core_get_plots

    function core_get_x_min(state) result(x_min)
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_min
        x_min = figure_get_x_min(state)
    end function core_get_x_min

    function core_get_x_max(state) result(x_max)
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_max
        x_max = figure_get_x_max(state)
    end function core_get_x_max

    function core_get_y_min(state) result(y_min)
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_min
        y_min = figure_get_y_min(state)
    end function core_get_y_min

    function core_get_y_max(state) result(y_max)
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_max
        y_max = figure_get_y_max(state)
    end function core_get_y_max

    subroutine core_backend_color(state, r, g, b)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: r, g, b
        call figure_backend_color(state, r, g, b)
    end subroutine core_backend_color

    function core_backend_associated(state) result(is_associated)
        type(figure_state_t), intent(in) :: state
        logical :: is_associated
        is_associated = figure_backend_associated(state)
    end function core_backend_associated

    subroutine core_backend_line(state, x1, y1, x2, y2)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x1, y1, x2, y2
        call figure_backend_line(state, x1, y1, x2, y2)
    end subroutine core_backend_line

    subroutine core_backend_arrow(state, x, y, dx, dy, arrow_size, style)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x, y, dx, dy, arrow_size
        character(len=*), intent(in) :: style
        integer :: new_index
        type(arrow_data_t), allocatable :: tmp(:)
        character(len=10) :: style_local

        style_local = '->'
        if (len_trim(style) > 0) then
            select case (trim(style))
            case ('->', '-', '<-', '<->')
                style_local = trim(style)
            case default
                style_local = '->'
            end select
        end if

        if (.not. allocated(state%stream_arrows)) then
            allocate (state%stream_arrows(1))
            new_index = 1
        else
            new_index = size(state%stream_arrows) + 1
            allocate (tmp(new_index))
            if (new_index > 1) tmp(1:new_index - 1) = state%stream_arrows
            call move_alloc(tmp, state%stream_arrows)
        end if

        state%stream_arrows(new_index)%x = x
        state%stream_arrows(new_index)%y = y
        state%stream_arrows(new_index)%dx = dx
        state%stream_arrows(new_index)%dy = dy
        state%stream_arrows(new_index)%size = max(0.0_wp, arrow_size)
        state%stream_arrows(new_index)%style = style_local

        state%rendered = .false.
    end subroutine core_backend_arrow

    ! Animation support - delegate to animation module
    subroutine core_setup_png_backend_for_animation(state)
        type(figure_state_t), intent(inout) :: state
        call figure_setup_png_backend_for_animation(state)
    end subroutine core_setup_png_backend_for_animation

    subroutine core_extract_rgb_data_for_animation(state, rgb_data, plots, plot_count, &
                                                   annotations, &
                                                   annotation_count, rendered)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(out) :: rgb_data(:, :, :)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count, annotation_count
        type(text_annotation_t), allocatable, intent(inout) :: annotations(:)
        logical, intent(in) :: rendered

        if (.not. rendered) call figure_render(state, plots, plot_count, &
                                               annotations, annotation_count)
        call figure_extract_rgb_data_for_animation(state, rgb_data, rendered)
    end subroutine core_extract_rgb_data_for_animation

    subroutine core_extract_png_data_for_animation(state, png_data, status, plots, &
                                                   plot_count, &
                                                   annotations, &
                                                   annotation_count, rendered)
        type(figure_state_t), intent(inout) :: state
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count, annotation_count
        type(text_annotation_t), allocatable, intent(inout) :: annotations(:)
        logical, intent(in) :: rendered

        if (.not. rendered) call figure_render(state, plots, plot_count, &
                                               annotations, annotation_count)
        call figure_extract_png_data_for_animation(state, png_data, status, rendered)
    end subroutine core_extract_png_data_for_animation

end module fortplot_figure_core_accessors
