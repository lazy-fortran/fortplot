module fortplot_context
    !! Abstract base class for plotting backends
    !!
    !! This module defines the common interface that all plotting backends
    !! (PNG, PDF, ASCII) must implement. Provides polymorphic interface
    !! for unified plotting operations across different output formats.
    !!
    !! Author: fortplot contributors

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    implicit none

    private
    public :: plot_context, setup_canvas
    type, abstract :: plot_context
        integer :: width, height
        real(wp) :: x_min, x_max, y_min, y_max
        ! Arrow rendering properties
        logical :: has_rendered_arrows = .false.
        logical :: uses_vector_arrows = .false.
        logical :: has_triangular_arrows = .false.
    contains
        procedure(line_interface), deferred :: line
        procedure(color_interface), deferred :: color
        procedure(text_interface), deferred :: text
        procedure(save_interface), deferred :: save
        procedure(line_width_interface), deferred :: set_line_width
        procedure(line_style_interface), deferred :: set_line_style
        procedure(marker_interface), deferred :: draw_marker
        procedure(marker_colors_interface), deferred :: set_marker_colors
        procedure(marker_colors_alpha_interface), deferred :: &
            set_marker_colors_with_alpha
        procedure(arrow_interface), deferred :: draw_arrow
        procedure(ascii_output_interface), deferred :: get_ascii_output

        !! Additional polymorphic methods to eliminate SELECT TYPE violations
        procedure(get_width_scale_interface), deferred :: get_width_scale
        procedure(get_height_scale_interface), deferred :: get_height_scale
        procedure(fill_quad_interface), deferred :: fill_quad
        procedure(fill_heatmap_interface), deferred :: fill_heatmap
        procedure(extract_rgb_data_interface), deferred :: extract_rgb_data
        procedure(get_png_data_interface), deferred :: get_png_data_backend
        procedure(prepare_3d_data_interface), deferred :: prepare_3d_data
        procedure(render_ylabel_interface), deferred :: render_ylabel
        procedure(draw_axes_and_labels_interface), deferred :: &
            draw_axes_and_labels_backend
        procedure(save_coordinates_interface), deferred :: save_coordinates
        procedure(set_coordinates_interface), deferred :: set_coordinates
        procedure(render_axes_interface), deferred :: render_axes
    end type plot_context

    abstract interface
        subroutine line_interface(this, x1, y1, x2, y2)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: x1, y1, x2, y2
        end subroutine line_interface

        subroutine color_interface(this, r, g, b)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: r, g, b
        end subroutine color_interface

        subroutine text_interface(this, x, y, text)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: x, y
            character(len=*), intent(in) :: text
        end subroutine text_interface

        subroutine save_interface(this, filename)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            character(len=*), intent(in) :: filename
        end subroutine save_interface

        subroutine line_width_interface(this, width)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: width
        end subroutine line_width_interface

        subroutine line_style_interface(this, style)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            character(len=*), intent(in) :: style
        end subroutine line_style_interface

        subroutine marker_interface(this, x, y, style)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: x, y
            character(len=*), intent(in) :: style
        end subroutine marker_interface

        subroutine marker_colors_interface(this, edge_r, edge_g, edge_b, face_r, &
                                           face_g, face_b)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: edge_r, edge_g, edge_b
            real(wp), intent(in) :: face_r, face_g, face_b
        end subroutine marker_colors_interface

        subroutine marker_colors_alpha_interface(this, edge_r, edge_g, edge_b, &
                                                 edge_alpha, face_r, face_g, face_b, &
                                                 face_alpha)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
            real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        end subroutine marker_colors_alpha_interface

        subroutine arrow_interface(this, x, y, dx, dy, size, style)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: x, y, dx, dy, size
            character(len=*), intent(in) :: style
        end subroutine arrow_interface

        function ascii_output_interface(this) result(output)
            import :: plot_context
            class(plot_context), intent(in) :: this
            character(len=:), allocatable :: output
        end function ascii_output_interface

        function get_width_scale_interface(this) result(scale)
            import :: plot_context, wp
            class(plot_context), intent(in) :: this
            real(wp) :: scale
        end function get_width_scale_interface

        function get_height_scale_interface(this) result(scale)
            import :: plot_context, wp
            class(plot_context), intent(in) :: this
            real(wp) :: scale
        end function get_height_scale_interface

        subroutine fill_quad_interface(this, x_quad, y_quad)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: x_quad(4), y_quad(4)
        end subroutine fill_quad_interface

        subroutine fill_heatmap_interface(this, x_grid, y_grid, z_grid, z_min, z_max)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
            real(wp), intent(in) :: z_min, z_max
        end subroutine fill_heatmap_interface

        subroutine extract_rgb_data_interface(this, width, height, rgb_data)
            import :: plot_context, wp
            class(plot_context), intent(in) :: this
            integer, intent(in) :: width, height
            real(wp), intent(out) :: rgb_data(width, height, 3)
        end subroutine extract_rgb_data_interface

        subroutine get_png_data_interface(this, width, height, png_data, status)
            import :: plot_context
            class(plot_context), intent(in) :: this
            integer, intent(in) :: width, height
            integer(1), allocatable, intent(out) :: png_data(:)
            integer, intent(out) :: status
        end subroutine get_png_data_interface

        subroutine prepare_3d_data_interface(this, plots)
            import :: plot_context, plot_data_t
            class(plot_context), intent(inout) :: this
            type(plot_data_t), intent(in) :: plots(:)
        end subroutine prepare_3d_data_interface

        subroutine render_ylabel_interface(this, ylabel)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            character(len=*), intent(in) :: ylabel
        end subroutine render_ylabel_interface

        subroutine draw_axes_and_labels_interface(this, xscale, yscale, &
                                                  symlog_threshold, &
                                                  x_min, x_max, y_min, y_max, &
                                                  title, xlabel, ylabel, &
                                                  x_date_format, y_date_format, &
                                                  z_min, z_max, has_3d_plots)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            character(len=*), intent(in) :: xscale, yscale
            real(wp), intent(in) :: symlog_threshold
            real(wp), intent(in) :: x_min, x_max, y_min, y_max
            character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
            character(len=*), intent(in), optional :: x_date_format, y_date_format
            real(wp), intent(in), optional :: z_min, z_max
            logical, intent(in) :: has_3d_plots
        end subroutine draw_axes_and_labels_interface

        subroutine save_coordinates_interface(this, x_min, x_max, y_min, y_max)
            import :: plot_context, wp
            class(plot_context), intent(in) :: this
            real(wp), intent(out) :: x_min, x_max, y_min, y_max
        end subroutine save_coordinates_interface

        subroutine set_coordinates_interface(this, x_min, x_max, y_min, y_max)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: x_min, x_max, y_min, y_max
        end subroutine set_coordinates_interface

        subroutine render_axes_interface(this, title_text, xlabel_text, ylabel_text)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            character(len=*), intent(in), optional :: title_text, xlabel_text, &
                                                      ylabel_text
        end subroutine render_axes_interface
    end interface

contains

    subroutine setup_canvas(ctx, width, height)
        class(plot_context), intent(inout) :: ctx
        integer, intent(in) :: width, height

        ctx%width = width
        ctx%height = height
        ctx%x_min = -1.0_wp
        ctx%x_max = 1.0_wp
        ctx%y_min = -1.0_wp
        ctx%y_max = 1.0_wp
    end subroutine setup_canvas

end module fortplot_context
