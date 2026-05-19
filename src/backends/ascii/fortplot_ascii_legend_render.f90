submodule (fortplot_ascii_legend) fortplot_ascii_legend_render

    !! ASCII legend rendering and dimension/position calculations
    !!
    !! Single Responsibility: Handle ASCII-specific legend rendering,
    !! dimension calculation, and position calculation.

    implicit none

contains

    module subroutine render_ascii_legend_specialized(legend, canvas_context, legend_x, legend_y)
        !! Render legend using ASCII-specific compact layout
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(inout) :: canvas_context
        real(wp), intent(in) :: legend_x, legend_y

        call render_ascii_legend(legend, canvas_context, legend_x, legend_y)
    end subroutine render_ascii_legend_specialized

    module subroutine calculate_ascii_legend_dimensions(legend, width, legend_width, legend_height)
        !! Calculate ASCII-specific legend dimensions
        type(legend_t), intent(in) :: legend
        integer, intent(in) :: width
        real(wp), intent(out) :: legend_width, legend_height
        integer :: i
        character(len=512) :: processed_label
        integer :: processed_len

        legend_width = 15.0_wp
        do i = 1, legend%num_entries
            call process_latex_in_text(legend%entries(i)%label, processed_label, processed_len)
            legend_width = max(legend_width, real(processed_len + 5, wp))
        end do

        if (legend_width > real(width, wp) * 0.3) then
            legend_width = real(width, wp) * 0.3
        end if

        legend_height = real(legend%num_entries + 2, wp)
    end subroutine calculate_ascii_legend_dimensions

    module subroutine set_ascii_legend_border_width()
        !! ASCII doesn't use line widths - no-op
    end subroutine set_ascii_legend_border_width

    module subroutine calculate_ascii_legend_position(legend, width, height, x, y)
        !! Calculate ASCII-specific legend position using character coordinates
        type(legend_t), intent(in) :: legend
        integer, intent(in) :: width, height
        real(wp), intent(out) :: x, y
        real(wp) :: legend_width, legend_height, margin_x, margin_y

        call calculate_ascii_legend_dimensions(legend, width, legend_width, legend_height)

        margin_x = 2.0_wp
        margin_y = 1.0_wp

        select case (legend%position)
        case (LEGEND_UPPER_LEFT)
            x = margin_x
            y = margin_y
        case (LEGEND_UPPER_RIGHT)
            x = real(width, wp) - legend_width - margin_x - 5.0_wp
            x = max(margin_x, x)
            y = margin_y + 2.0_wp
        case (LEGEND_LOWER_LEFT)
            x = margin_x
            y = real(height, wp) - legend_height - margin_y
        case (LEGEND_LOWER_RIGHT)
            x = real(width, wp) - legend_width - margin_x
            y = real(height, wp) - legend_height - margin_y
        case (LEGEND_EAST)
            x = real(width, wp) - legend_width - margin_x
            y = (real(height, wp) - legend_height)*0.5_wp
        case default
            x = real(width, wp) - legend_width - margin_x
            y = margin_y
        end select
    end subroutine calculate_ascii_legend_position

    module subroutine ascii_calc_legend_dims_impl(legend, width, legend_width, legend_height)
        use fortplot_legend, only: legend_t
        type(legend_t), intent(in) :: legend
        integer, intent(in) :: width
        real(wp), intent(out) :: legend_width, legend_height

        call calculate_ascii_legend_dimensions(legend, width, legend_width, legend_height)
    end subroutine ascii_calc_legend_dims_impl

    module subroutine ascii_set_legend_border_impl()
    end subroutine ascii_set_legend_border_impl

    module subroutine ascii_calc_legend_pos_impl(legend, width, height, legend_width, legend_height, x, y)
        use fortplot_legend, only: legend_t
        type(legend_t), intent(in) :: legend
        integer, intent(in) :: width, height
        real(wp), intent(in) :: legend_width, legend_height
        real(wp), intent(out) :: x, y
        real(wp) :: margin_x, margin_y
        real(wp) :: rw, rh, lw, lh

        margin_x = 2.0_wp
        margin_y = 1.0_wp
        rw = real(width, wp)
        rh = real(height, wp)
        lw = legend_width
        lh = legend_height

        select case (legend%position)
        case (LEGEND_UPPER_LEFT)
            x = margin_x
            y = margin_y
        case (LEGEND_UPPER_RIGHT)
            x = rw - lw - margin_x - 5.0_wp
            x = max(margin_x, x)
            y = margin_y + 2.0_wp
        case (LEGEND_LOWER_LEFT)
            x = margin_x
            y = rh - lh - margin_y
        case (LEGEND_LOWER_RIGHT)
            x = rw - lw - margin_x
            y = rh - lh - margin_y
        case (LEGEND_EAST)
            x = rw - lw - margin_x
            y = (rh - lh)*0.5_wp
        case default
            x = rw - lw - margin_x
            y = margin_y
        end select
    end subroutine ascii_calc_legend_pos_impl

end submodule fortplot_ascii_legend_render
