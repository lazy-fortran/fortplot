module fortplot_vector_plot_helpers
    !! Vector plot (quiver, streamplot, refline) rendering helpers
    !!
    !! Extracted from fortplot_figure_rendering_pipeline to reduce module size
    !! Single Responsibility: Render vector-based plots

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_plot_data, only: plot_data_t, arrow_data_t
    use fortplot_scales, only: apply_scale_transform
    implicit none

    private
    public :: render_refline_plot, render_streamplot_arrows

contains

    subroutine render_refline_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                   xscale, yscale, symlog_threshold)
        !! Render a reference line (horizontal or vertical)
        !! Reference lines store normalized coordinates for axis-spanning lines
        !! or actual data coordinates for hlines/vlines
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        real(wp) :: x1, y1, x2, y2
        real(wp) :: x1_scaled, y1_scaled, x2_scaled, y2_scaled

        if (.not. allocated(plot%x) .or. .not. allocated(plot%y)) return
        if (size(plot%x) < 2 .or. size(plot%y) < 2) return

        call backend%color(plot%color(1), plot%color(2), plot%color(3))

        if (allocated(plot%linestyle) .and. len_trim(plot%linestyle) > 0) then
            call backend%set_line_style(trim(plot%linestyle))
        else
            call backend%set_line_style('-')
        end if

        x1 = plot%x(1)
        x2 = plot%x(2)
        y1 = plot%y(1)
        y2 = plot%y(2)

        ! For horizontal lines (y1 == y2), x values may be normalized
        if (abs(y1 - y2) < 1.0e-9_wp) then
            ! Horizontal line: check if x values are normalized (0-1)
            if (x1 >= 0.0_wp .and. x1 <= 1.0_wp .and. &
                x2 >= 0.0_wp .and. x2 <= 1.0_wp .and. &
                (x1 < 0.01_wp .or. x2 > 0.99_wp)) then
                ! Convert normalized x to data coordinates
                x1 = x_min + x1*(x_max - x_min)
                x2 = x_min + x2*(x_max - x_min)
            end if
        end if

        ! For vertical lines (x1 == x2), y values may be normalized
        if (abs(x1 - x2) < 1.0e-9_wp) then
            ! Vertical line: check if y values are normalized (0-1)
            if (y1 >= 0.0_wp .and. y1 <= 1.0_wp .and. &
                y2 >= 0.0_wp .and. y2 <= 1.0_wp .and. &
                (y1 < 0.01_wp .or. y2 > 0.99_wp)) then
                ! Convert normalized y to data coordinates
                y1 = y_min + y1*(y_max - y_min)
                y2 = y_min + y2*(y_max - y_min)
            end if
        end if

        ! Apply scale transformations
        x1_scaled = apply_scale_transform(x1, xscale, symlog_threshold)
        x2_scaled = apply_scale_transform(x2, xscale, symlog_threshold)
        y1_scaled = apply_scale_transform(y1, yscale, symlog_threshold)
        y2_scaled = apply_scale_transform(y2, yscale, symlog_threshold)

        ! Draw the line
        call backend%line(x1_scaled, y1_scaled, x2_scaled, y2_scaled)
    end subroutine render_refline_plot

    subroutine render_streamplot_arrows(backend, arrows)
        !! Render queued streamplot arrows after plot lines are drawn.
        !! See fortplot_figure_plot_renderers for the canonical version;
        !! kept in sync so this helpers module stays a drop-in.
        !!
        !! The text backend routes each arrowhead through the cell layer policy
        !! (LAYER_DATA) so a direction marker occupies its plot cell without
        !! overwriting axes, ticks, or tick/axis labels (issue #2070).
        class(plot_context), intent(inout) :: backend
        type(arrow_data_t), intent(in) :: arrows(:)
        integer :: i

        if (size(arrows) <= 0) return

        do i = 1, size(arrows)
            call backend%draw_arrowhead(arrows(i)%x, arrows(i)%y, arrows(i)%dx, &
                                        arrows(i)%dy, &
                                        arrows(i)%size, arrows(i)%style)
        end do
    end subroutine render_streamplot_arrows

end module fortplot_vector_plot_helpers
