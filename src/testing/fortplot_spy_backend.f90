module fortplot_spy_backend
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_plot_data, only: plot_data_t
    implicit none

    private
    public :: spy_context_t

    type, extends(plot_context) :: spy_context_t
        real(wp) :: current_color(3) = [0.0_wp, 0.0_wp, 0.0_wp]
        real(wp) :: expected_fill(3) = [0.0_wp, 0.0_wp, 0.0_wp]
        real(wp) :: expected_edge(3) = [0.0_wp, 0.0_wp, 0.0_wp]
        integer :: fill_calls = 0
        integer :: line_calls = 0
        integer :: unexpected_calls = 0
        logical :: fill_color_ok = .true.
        logical :: line_color_ok = .true.
    contains
        procedure :: reset => spy_reset
        procedure :: line => spy_line
        procedure :: color => spy_color
        procedure :: text => spy_text
        procedure :: save => spy_save
        procedure :: set_line_width => spy_set_line_width
        procedure :: set_line_style => spy_set_line_style
        procedure :: draw_marker => spy_draw_marker
        procedure :: set_marker_colors => spy_set_marker_colors
        procedure :: set_marker_colors_with_alpha => spy_set_marker_colors_with_alpha
        procedure :: draw_arrow => spy_draw_arrow
        procedure :: get_ascii_output => spy_get_ascii_output
        procedure :: get_width_scale => spy_get_width_scale
        procedure :: get_height_scale => spy_get_height_scale
        procedure :: fill_quad => spy_fill_quad
        procedure :: fill_heatmap => spy_fill_heatmap
        procedure :: extract_rgb_data => spy_extract_rgb_data
        procedure :: get_png_data_backend => spy_get_png_data_backend
        procedure :: prepare_3d_data => spy_prepare_3d_data
        procedure :: render_ylabel => spy_render_ylabel
        procedure :: draw_axes_and_labels_backend => spy_draw_axes_and_labels_backend
        procedure :: save_coordinates => spy_save_coordinates
        procedure :: set_coordinates => spy_set_coordinates
        procedure :: render_axes => spy_render_axes
    end type spy_context_t

contains

    pure logical function colors_close(a, b) result(ok)
        real(wp), intent(in) :: a(3), b(3)
        real(wp), parameter :: tol = 1.0e-12_wp

        ok = all(abs(a - b) <= tol)
    end function colors_close

    subroutine spy_reset(this)
        class(spy_context_t), intent(inout) :: this

        this%current_color = [0.0_wp, 0.0_wp, 0.0_wp]
        this%expected_fill = [0.0_wp, 0.0_wp, 0.0_wp]
        this%expected_edge = [0.0_wp, 0.0_wp, 0.0_wp]
        this%fill_calls = 0
        this%line_calls = 0
        this%unexpected_calls = 0
        this%fill_color_ok = .true.
        this%line_color_ok = .true.

        this%width = 0
        this%height = 0
        this%x_min = -1.0_wp
        this%x_max = 1.0_wp
        this%y_min = -1.0_wp
        this%y_max = 1.0_wp
        this%has_rendered_arrows = .false.
        this%uses_vector_arrows = .false.
        this%has_triangular_arrows = .false.
    end subroutine spy_reset

    subroutine spy_line(this, x1, y1, x2, y2)
        class(spy_context_t), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2

        this%line_calls = this%line_calls + 1
        this%line_color_ok = this%line_color_ok .and. &
                             colors_close(this%current_color, this%expected_edge)
    end subroutine spy_line

    subroutine spy_color(this, r, g, b)
        class(spy_context_t), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        this%current_color = [r, g, b]
    end subroutine spy_color

    subroutine spy_text(this, x, y, text)
        class(spy_context_t), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_text

    subroutine spy_save(this, filename)
        class(spy_context_t), intent(inout) :: this
        character(len=*), intent(in) :: filename

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_save

    subroutine spy_set_line_width(this, width)
        class(spy_context_t), intent(inout) :: this
        real(wp), intent(in) :: width

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_set_line_width

    subroutine spy_set_line_style(this, style)
        class(spy_context_t), intent(inout) :: this
        character(len=*), intent(in) :: style

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_set_line_style

    subroutine spy_draw_marker(this, x, y, style)
        class(spy_context_t), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_draw_marker

    subroutine spy_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, face_g, &
                                     face_b)
        class(spy_context_t), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_set_marker_colors

    subroutine spy_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, &
                                                edge_alpha, face_r, face_g, face_b, &
                                                face_alpha)
        class(spy_context_t), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_set_marker_colors_with_alpha

    subroutine spy_draw_arrow(this, x, y, dx, dy, size, style)
        class(spy_context_t), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_draw_arrow

    function spy_get_ascii_output(this) result(output)
        class(spy_context_t), intent(in) :: this
        character(len=:), allocatable :: output

        output = ""
    end function spy_get_ascii_output

    function spy_get_width_scale(this) result(scale)
        class(spy_context_t), intent(in) :: this
        real(wp) :: scale

        scale = 1.0_wp
    end function spy_get_width_scale

    function spy_get_height_scale(this) result(scale)
        class(spy_context_t), intent(in) :: this
        real(wp) :: scale

        scale = 1.0_wp
    end function spy_get_height_scale

    subroutine spy_fill_quad(this, x_quad, y_quad)
        class(spy_context_t), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)

        this%fill_calls = this%fill_calls + 1
        this%fill_color_ok = this%fill_color_ok .and. &
                             colors_close(this%current_color, this%expected_fill)
    end subroutine spy_fill_quad

    subroutine spy_fill_heatmap(this, x_grid, y_grid, z_grid, z_min, z_max)
        class(spy_context_t), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in) :: z_min, z_max

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_fill_heatmap

    subroutine spy_extract_rgb_data(this, width, height, rgb_data)
        class(spy_context_t), intent(in) :: this
        integer, intent(in) :: width, height
        real(wp), intent(out) :: rgb_data(width, height, 3)

        rgb_data = 0.0_wp
    end subroutine spy_extract_rgb_data

    subroutine spy_get_png_data_backend(this, width, height, png_data, status)
        class(spy_context_t), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status

        allocate (png_data(0))
        status = 1
    end subroutine spy_get_png_data_backend

    subroutine spy_prepare_3d_data(this, plots)
        class(spy_context_t), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_prepare_3d_data

    subroutine spy_render_ylabel(this, ylabel)
        class(spy_context_t), intent(inout) :: this
        character(len=*), intent(in) :: ylabel

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_render_ylabel

    subroutine spy_draw_axes_and_labels_backend(this, xscale, yscale, &
                                                symlog_threshold, x_min, x_max, &
                                                y_min, y_max, title, xlabel, ylabel, &
                                                x_date_format, y_date_format, z_min, &
                                                z_max, has_3d_plots)
        class(spy_context_t), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_draw_axes_and_labels_backend

    subroutine spy_save_coordinates(this, x_min, x_max, y_min, y_max)
        class(spy_context_t), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max

        x_min = this%x_min
        x_max = this%x_max
        y_min = this%y_min
        y_max = this%y_max
    end subroutine spy_save_coordinates

    subroutine spy_set_coordinates(this, x_min, x_max, y_min, y_max)
        class(spy_context_t), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        this%x_min = x_min
        this%x_max = x_max
        this%y_min = y_min
        this%y_max = y_max
        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_set_coordinates

    subroutine spy_render_axes(this, title_text, xlabel_text, ylabel_text)
        class(spy_context_t), intent(inout) :: this
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text

        this%unexpected_calls = this%unexpected_calls + 1
    end subroutine spy_render_axes

end module fortplot_spy_backend
