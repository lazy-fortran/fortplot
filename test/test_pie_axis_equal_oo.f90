program test_pie_axis_equal_oo
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortplot, only: figure_t
    use fortplot_figure_aspect, only: only_pie_plots
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_ascii, only: ascii_context
    implicit none

    type(figure_t) :: fig
    real(dp) :: values(4)
    real(dp) :: explode(4)
    character(len=20) :: labels(4)
    character(len=*), parameter :: output_dir = 'build/test/output/'
    character(len=*), parameter :: filename = output_dir//'test_pie_axis_equal_oo.png'
    real(dp) :: plot_width_px, plot_height_px
    real(dp) :: range_x, range_y
    real(dp) :: scale_x, scale_y
    real(dp) :: aspect_diff

    values = [42.0_dp, 28.0_dp, 18.0_dp, 12.0_dp]
    explode = [0.1_dp, 0.0_dp, 0.0_dp, 0.05_dp]
    labels(1) = 'Solar'
    labels(2) = 'Wind'
    labels(3) = 'Hydro'
    labels(4) = 'Storage'

    call fig%initialize()
    call fig%add_pie(values, labels=labels, autopct='%.0f%%', startangle=75.0_dp, explode=explode)
    call fig%savefig(filename)

    if (.not. only_pie_plots(fig%plots, fig%plot_count)) then
        error stop 'OO pie should be the sole plot type'
    end if

    range_x = fig%state%x_max - fig%state%x_min
    range_y = fig%state%y_max - fig%state%y_min

    if (range_x <= 0.0_dp .or. range_y <= 0.0_dp) then
        error stop 'Pie axis ranges are invalid for OO figure'
    end if

    call get_backend_plot_extent(fig%state%backend, real(fig%state%width, dp), &
        real(fig%state%height, dp), fig%state%margin_left, fig%state%margin_right, &
        fig%state%margin_top, fig%state%margin_bottom, plot_width_px, &
        plot_height_px)

    if (plot_width_px <= 0.0_dp .or. plot_height_px <= 0.0_dp) then
        error stop 'Plot area dimensions are invalid for OO figure'
    end if

    scale_x = plot_width_px / range_x
    scale_y = plot_height_px / range_y
    aspect_diff = abs(scale_x - scale_y) / max(scale_x, scale_y)

    if (aspect_diff > 1.0e-6_dp) then
        write(*,*) 'scale_x = ', scale_x, 'scale_y = ', scale_y
        error stop 'OO pie axis scaling is not equal'
    end if

    print *, '✓ OO pie chart uses equal axis scaling by default'

contains

    subroutine get_backend_plot_extent(backend, fig_width, fig_height, margin_left, &
        margin_right, margin_top, margin_bottom, width_px, height_px)
        class(*), intent(in) :: backend
        real(dp), intent(in) :: fig_width, fig_height
        real(dp), intent(in) :: margin_left, margin_right, margin_top, margin_bottom
        real(dp), intent(out) :: width_px, height_px

        select type (backend)
        type is (png_context)
            width_px = real(max(1, backend%plot_area%width), dp)
            height_px = real(max(1, backend%plot_area%height), dp)
        type is (pdf_context)
            width_px = real(max(1, backend%plot_area%width), dp)
            height_px = real(max(1, backend%plot_area%height), dp)
        type is (ascii_context)
            width_px = real(max(1, backend%plot_width - 3), dp)
            height_px = real(max(1, backend%plot_height - 3), dp)
        class default
            width_px = fig_width * max(0.0_dp, 1.0_dp - margin_left - margin_right)
            height_px = fig_height * max(0.0_dp, 1.0_dp - margin_top - margin_bottom)
        end select
    end subroutine get_backend_plot_extent

end program test_pie_axis_equal_oo
