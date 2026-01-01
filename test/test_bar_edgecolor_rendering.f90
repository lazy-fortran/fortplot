module test_bar_edgecolor_spy_backend
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_plot_data, only: plot_data_t
    implicit none

    public :: spy_context

    type, extends(plot_context) :: spy_context
        real(wp) :: current_color(3) = [0.0_wp, 0.0_wp, 0.0_wp]
        real(wp) :: expected_fill(3) = [0.0_wp, 0.0_wp, 0.0_wp]
        real(wp) :: expected_edge(3) = [0.0_wp, 0.0_wp, 0.0_wp]
        integer :: fill_calls = 0
        integer :: line_calls = 0
        integer :: other_calls = 0
        logical :: fill_color_ok = .true.
        logical :: line_color_ok = .true.
    contains
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
    end type spy_context

contains

    pure logical function colors_close(a, b) result(ok)
        real(wp), intent(in) :: a(3), b(3)
        real(wp), parameter :: tol = 1.0e-12_wp

        ok = all(abs(a - b) <= tol)
    end function colors_close

    subroutine spy_line(this, x1, y1, x2, y2)
        class(spy_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2

        this%line_calls = this%line_calls + 1
        this%line_color_ok = this%line_color_ok .and. &
                             colors_close(this%current_color, this%expected_edge)
    end subroutine spy_line

    subroutine spy_color(this, r, g, b)
        class(spy_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        this%current_color = [r, g, b]
    end subroutine spy_color

    subroutine spy_text(this, x, y, text)
        class(spy_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        this%other_calls = this%other_calls + 1
    end subroutine spy_text

    subroutine spy_save(this, filename)
        class(spy_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        this%other_calls = this%other_calls + 1
    end subroutine spy_save

    subroutine spy_set_line_width(this, width)
        class(spy_context), intent(inout) :: this
        real(wp), intent(in) :: width
        this%other_calls = this%other_calls + 1
    end subroutine spy_set_line_width

    subroutine spy_set_line_style(this, style)
        class(spy_context), intent(inout) :: this
        character(len=*), intent(in) :: style
        this%other_calls = this%other_calls + 1
    end subroutine spy_set_line_style

    subroutine spy_draw_marker(this, x, y, style)
        class(spy_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        this%other_calls = this%other_calls + 1
    end subroutine spy_draw_marker

    subroutine spy_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, face_g, &
                                     face_b)
        class(spy_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b
        this%other_calls = this%other_calls + 1
    end subroutine spy_set_marker_colors

    subroutine spy_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, &
                                                edge_alpha, face_r, face_g, face_b, &
                                                face_alpha)
        class(spy_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        this%other_calls = this%other_calls + 1
    end subroutine spy_set_marker_colors_with_alpha

    subroutine spy_draw_arrow(this, x, y, dx, dy, size, style)
        class(spy_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        this%other_calls = this%other_calls + 1
    end subroutine spy_draw_arrow

    function spy_get_ascii_output(this) result(output)
        class(spy_context), intent(in) :: this
        character(len=:), allocatable :: output

        output = ''
    end function spy_get_ascii_output

    function spy_get_width_scale(this) result(scale)
        class(spy_context), intent(in) :: this
        real(wp) :: scale

        scale = 1.0_wp
    end function spy_get_width_scale

    function spy_get_height_scale(this) result(scale)
        class(spy_context), intent(in) :: this
        real(wp) :: scale

        scale = 1.0_wp
    end function spy_get_height_scale

    subroutine spy_fill_quad(this, x_quad, y_quad)
        class(spy_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)

        this%fill_calls = this%fill_calls + 1
        this%fill_color_ok = this%fill_color_ok .and. &
                             colors_close(this%current_color, this%expected_fill)
    end subroutine spy_fill_quad

    subroutine spy_fill_heatmap(this, x_grid, y_grid, z_grid, z_min, z_max)
        class(spy_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in) :: z_min, z_max
        this%other_calls = this%other_calls + 1
    end subroutine spy_fill_heatmap

    subroutine spy_extract_rgb_data(this, width, height, rgb_data)
        class(spy_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(wp), intent(out) :: rgb_data(width, height, 3)

        rgb_data = 0.0_wp
    end subroutine spy_extract_rgb_data

    subroutine spy_get_png_data_backend(this, width, height, png_data, status)
        class(spy_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status

        allocate (png_data(0))
        status = 1
    end subroutine spy_get_png_data_backend

    subroutine spy_prepare_3d_data(this, plots)
        class(spy_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)
        this%other_calls = this%other_calls + 1
    end subroutine spy_prepare_3d_data

    subroutine spy_render_ylabel(this, ylabel)
        class(spy_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel
        this%other_calls = this%other_calls + 1
    end subroutine spy_render_ylabel

    subroutine spy_draw_axes_and_labels_backend(this, xscale, yscale, &
                                                symlog_threshold, x_min, x_max, &
                                                y_min, y_max, title, xlabel, ylabel, &
                                                x_date_format, y_date_format, z_min, &
                                                z_max, has_3d_plots)
        class(spy_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        this%other_calls = this%other_calls + 1
    end subroutine spy_draw_axes_and_labels_backend

    subroutine spy_save_coordinates(this, x_min, x_max, y_min, y_max)
        class(spy_context), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max

        x_min = this%x_min
        x_max = this%x_max
        y_min = this%y_min
        y_max = this%y_max
    end subroutine spy_save_coordinates

    subroutine spy_set_coordinates(this, x_min, x_max, y_min, y_max)
        class(spy_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        this%x_min = x_min
        this%x_max = x_max
        this%y_min = y_min
        this%y_max = y_max
        this%other_calls = this%other_calls + 1
    end subroutine spy_set_coordinates

    subroutine spy_render_axes(this, title_text, xlabel_text, ylabel_text)
        class(spy_context), intent(inout) :: this
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text
        this%other_calls = this%other_calls + 1
    end subroutine spy_render_axes

end module test_bar_edgecolor_spy_backend

program test_bar_edgecolor_rendering
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_bar_rendering, only: render_bar_plot
    use fortplot_context, only: setup_canvas
    use fortplot_matplotlib, only: bar, barh, figure, get_global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_BAR
    use test_bar_edgecolor_spy_backend, only: spy_context
    implicit none

    logical :: failed

    failed = .false.
    call test_render_bar_plot_draws_quads(failed)
    call test_bar_respects_edgecolor(failed)
    call test_bar_defaults_edgecolor_to_facecolor(failed)
    call test_barh_respects_edgecolor(failed)

    if (failed) stop 1
    print *, "bar/barh edgecolor rendering tests passed"

contains

    subroutine assert_true(name, condition, failed)
        character(len=*), intent(in) :: name
        logical, intent(in) :: condition
        logical, intent(inout) :: failed

        if (.not. condition) then
            failed = .true.
            print *, "FAIL: ", trim(name)
        end if
    end subroutine assert_true

    subroutine test_bar_respects_edgecolor(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        type(spy_context) :: backend

        real(wp) :: x(3), heights(3)
        real(wp) :: fill(3), edge(3)

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        heights = [1.0_wp, 2.0_wp, 3.0_wp]
        fill = [1.0_wp, 0.0_wp, 0.0_wp]
        edge = [0.0_wp, 0.0_wp, 1.0_wp]

        call figure()
        call bar(x, heights, color=fill, edgecolor=edge)
        fig => get_global_figure()

        backend%expected_fill = fill
        backend%expected_edge = edge
        call setup_canvas(backend, 200, 200)

        call render_bar_plot(backend, fig%plots(fig%plot_count), 'linear', 'linear', &
                             0.0_wp)

        call assert_true("bar edgecolor: fills present", backend%fill_calls == 3, &
                         failed)
        call assert_true("bar edgecolor: edges present", backend%line_calls == 12, &
                         failed)
        call assert_true("bar edgecolor: fill color used", backend%fill_color_ok, &
                         failed)
        call assert_true("bar edgecolor: edge color used", backend%line_color_ok, &
                         failed)
        call assert_true("bar edgecolor: no extra backend calls", &
                         backend%other_calls == 0, failed)
    end subroutine test_bar_respects_edgecolor

    subroutine test_render_bar_plot_draws_quads(failed)
        logical, intent(inout) :: failed
        type(spy_context) :: backend
        type(plot_data_t) :: plot
        real(wp) :: fill(3), edge(3)

        fill = [0.1_wp, 0.2_wp, 0.3_wp]
        edge = [0.9_wp, 0.8_wp, 0.7_wp]

        plot%plot_type = PLOT_TYPE_BAR
        plot%color = fill
        plot%bar_width = 1.0_wp
        plot%bar_horizontal = .false.
        plot%bar_edgecolor = edge
        plot%bar_edgecolor_set = .true.
        allocate (plot%bar_x(1))
        allocate (plot%bar_heights(1))
        allocate (plot%bar_bottom(1))
        plot%bar_x = [1.0_wp]
        plot%bar_heights = [2.0_wp]
        plot%bar_bottom = [0.0_wp]

        backend%expected_fill = fill
        backend%expected_edge = edge
        call setup_canvas(backend, 200, 200)

        call render_bar_plot(backend, plot, 'linear', 'linear', 0.0_wp)

        call assert_true("direct render draws one quad", backend%fill_calls == 1, &
                         failed)
        call assert_true("direct render draws quad edges", backend%line_calls == 4, &
                         failed)
        call assert_true("direct render uses fill color", backend%fill_color_ok, failed)
        call assert_true("direct render uses edge color", backend%line_color_ok, failed)
        call assert_true("direct render: no extra backend calls", &
                         backend%other_calls == 0, failed)
    end subroutine test_render_bar_plot_draws_quads

    subroutine test_bar_defaults_edgecolor_to_facecolor(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        type(spy_context) :: backend

        real(wp) :: x(2), heights(2)
        real(wp) :: fill(3)

        x = [1.0_wp, 2.0_wp]
        heights = [2.0_wp, 4.0_wp]
        fill = [0.25_wp, 0.5_wp, 0.75_wp]

        call figure()
        call bar(x, heights, color=fill)
        fig => get_global_figure()

        backend%expected_fill = fill
        backend%expected_edge = fill
        call setup_canvas(backend, 200, 200)

        call render_bar_plot(backend, fig%plots(fig%plot_count), 'linear', 'linear', &
                             0.0_wp)

        call assert_true("bar default edgecolor: fills present", &
                         backend%fill_calls == 2, failed)
        call assert_true("bar default edgecolor: edges present", &
                         backend%line_calls == 8, failed)
        call assert_true("bar default edgecolor: fill color used", &
                         backend%fill_color_ok, failed)
        call assert_true("bar default edgecolor: edge color used", &
                         backend%line_color_ok, failed)
        call assert_true("bar default edgecolor: no extra backend calls", &
                         backend%other_calls == 0, failed)
    end subroutine test_bar_defaults_edgecolor_to_facecolor

    subroutine test_barh_respects_edgecolor(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        type(spy_context) :: backend

        real(wp) :: y(3), widths(3)
        real(wp) :: fill(3), edge(3)

        y = [1.0_wp, 2.0_wp, 3.0_wp]
        widths = [1.5_wp, 0.5_wp, 2.25_wp]
        fill = [0.0_wp, 1.0_wp, 0.0_wp]
        edge = [1.0_wp, 0.0_wp, 1.0_wp]

        call figure()
        call barh(y, widths, color=fill, edgecolor=edge)
        fig => get_global_figure()

        backend%expected_fill = fill
        backend%expected_edge = edge
        call setup_canvas(backend, 200, 200)

        call render_bar_plot(backend, fig%plots(fig%plot_count), 'linear', 'linear', &
                             0.0_wp)

        call assert_true("barh edgecolor: fills present", backend%fill_calls == 3, &
                         failed)
        call assert_true("barh edgecolor: edges present", backend%line_calls == 12, &
                         failed)
        call assert_true("barh edgecolor: fill color used", backend%fill_color_ok, &
                         failed)
        call assert_true("barh edgecolor: edge color used", backend%line_color_ok, &
                         failed)
        call assert_true("barh edgecolor: no extra backend calls", &
                         backend%other_calls == 0, failed)
    end subroutine test_barh_respects_edgecolor

end program test_bar_edgecolor_rendering
