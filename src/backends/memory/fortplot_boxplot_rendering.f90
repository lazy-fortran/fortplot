module fortplot_boxplot_rendering
    !! Box plot rendering for memory backends (shared for raster/PDF via context)
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_plot_data, only: plot_data_t
    implicit none

    private
    public :: render_boxplot_plot

contains

    subroutine render_boxplot_plot(backend, plot_data, xscale, yscale, symlog_threshold)
        !! Render a single box plot (vertical or horizontal)
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        real(wp) :: pos, halfw
        real(wp) :: q1, q2, q3, wlo, whi
        real(wp) :: capw
        logical :: horiz

        if (.not. allocated(plot_data%box_data)) return

        pos = plot_data%position
        halfw = 0.5_wp * plot_data%width
        q1 = plot_data%q1
        q2 = plot_data%q2
        q3 = plot_data%q3
        wlo = plot_data%whisker_low
        whi = plot_data%whisker_high
        capw = max(0.1_wp * plot_data%width, 0.05_wp)
        horiz = plot_data%horizontal

        call backend%color(plot_data%color(1), plot_data%color(2), plot_data%color(3))

        if (.not. horiz) then
            call draw_box_vertical(backend, pos, halfw, q1, q2, q3, wlo, whi, capw, xscale, yscale, symlog_threshold)
            if (plot_data%show_outliers) call draw_outliers_vertical(backend, pos, plot_data, xscale, yscale, symlog_threshold)
        else
            call draw_box_horizontal(backend, pos, halfw, q1, q2, q3, wlo, whi, capw, xscale, yscale, symlog_threshold)
            if (plot_data%show_outliers) call draw_outliers_horizontal(backend, pos, plot_data, xscale, yscale, symlog_threshold)
        end if
    end subroutine render_boxplot_plot

    subroutine draw_box_vertical(backend, pos, halfw, q1, q2, q3, wlo, whi, capw, xscale, yscale, t)
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: pos, halfw, q1, q2, q3, wlo, whi, capw
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: t
        real(wp) :: x0, x1
        ! no temporaries needed; transform per segment for cache friendliness

        x0 = pos - halfw; x1 = pos + halfw

        ! Box rectangle (q1 to q3)
        call line_scaled(backend, x0, q1, x1, q1, xscale, yscale, t)
        call line_scaled(backend, x1, q1, x1, q3, xscale, yscale, t)
        call line_scaled(backend, x1, q3, x0, q3, xscale, yscale, t)
        call line_scaled(backend, x0, q3, x0, q1, xscale, yscale, t)

        ! Median line
        call line_scaled(backend, x0, q2, x1, q2, xscale, yscale, t)

        ! Whiskers
        call line_scaled(backend, pos, q1, pos, wlo, xscale, yscale, t)
        call line_scaled(backend, pos, q3, pos, whi, xscale, yscale, t)
        ! Caps
        call line_scaled(backend, pos - capw, wlo, pos + capw, wlo, xscale, yscale, t)
        call line_scaled(backend, pos - capw, whi, pos + capw, whi, xscale, yscale, t)
    end subroutine draw_box_vertical

    subroutine draw_box_horizontal(backend, pos, halfw, q1, q2, q3, wlo, whi, capw, xscale, yscale, t)
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: pos, halfw, q1, q2, q3, wlo, whi, capw
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: t
        real(wp) :: y0, y1

        y0 = pos - halfw; y1 = pos + halfw

        ! Box rectangle (q1 to q3) but horizontal
        call line_scaled(backend, q1, y0, q3, y0, xscale, yscale, t)
        call line_scaled(backend, q3, y0, q3, y1, xscale, yscale, t)
        call line_scaled(backend, q3, y1, q1, y1, xscale, yscale, t)
        call line_scaled(backend, q1, y1, q1, y0, xscale, yscale, t)

        ! Median line
        call line_scaled(backend, q2, y0, q2, y1, xscale, yscale, t)

        ! Whiskers
        call line_scaled(backend, q1, pos, wlo, pos, xscale, yscale, t)
        call line_scaled(backend, q3, pos, whi, pos, xscale, yscale, t)
        ! Caps
        call line_scaled(backend, wlo, pos - capw, wlo, pos + capw, xscale, yscale, t)
        call line_scaled(backend, whi, pos - capw, whi, pos + capw, xscale, yscale, t)
    end subroutine draw_box_horizontal

    subroutine draw_outliers_vertical(backend, pos, plot, xscale, yscale, t)
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: pos
        type(plot_data_t), intent(in) :: plot
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: t
        integer :: i
        real(wp) :: xs, ys

        if (.not. allocated(plot%outliers)) return
        do i = 1, size(plot%outliers)
            xs = apply_scale_transform(pos, xscale, t)
            ys = apply_scale_transform(plot%outliers(i), yscale, t)
            call backend%draw_marker(xs, ys, 'o')
        end do
    end subroutine draw_outliers_vertical

    subroutine draw_outliers_horizontal(backend, pos, plot, xscale, yscale, t)
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: pos
        type(plot_data_t), intent(in) :: plot
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: t
        integer :: i
        real(wp) :: xs, ys

        if (.not. allocated(plot%outliers)) return
        do i = 1, size(plot%outliers)
            xs = apply_scale_transform(plot%outliers(i), xscale, t)
            ys = apply_scale_transform(pos, yscale, t)
            call backend%draw_marker(xs, ys, 'o')
        end do
    end subroutine draw_outliers_horizontal

    subroutine line_scaled(backend, x1, y1, x2, y2, xscale, yscale, t)
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x1, y1, x2, y2
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: t
        real(wp) :: xs1, ys1, xs2, ys2
        xs1 = apply_scale_transform(x1, xscale, t)
        ys1 = apply_scale_transform(y1, yscale, t)
        xs2 = apply_scale_transform(x2, xscale, t)
        ys2 = apply_scale_transform(y2, yscale, t)
        call backend%line(xs1, ys1, xs2, ys2)
    end subroutine line_scaled

end module fortplot_boxplot_rendering
