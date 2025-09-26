program test_pie_axis_equal
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortplot, only: figure, pie, savefig, get_global_figure, figure_t
    implicit none

    character(len=*), parameter :: output_dir = 'build/test/output/'
    character(len=*), parameter :: filename = output_dir//'test_pie_axis_equal.png'
    real(dp) :: values(3)
    class(figure_t), pointer :: fig_ptr
    real(dp) :: plot_width_px, plot_height_px
    real(dp) :: range_x, range_y
    real(dp) :: scale_x, scale_y
    real(dp) :: aspect_diff

    values = [1.0_dp, 3.0_dp, 2.0_dp]

    call figure()
    call pie(values)
    call savefig(filename)

    fig_ptr => get_global_figure()
    if (.not. associated(fig_ptr)) then
        error stop 'Global figure not available after pie render'
    end if

    range_x = fig_ptr%state%x_max - fig_ptr%state%x_min
    range_y = fig_ptr%state%y_max - fig_ptr%state%y_min

    if (range_x <= 0.0_dp .or. range_y <= 0.0_dp) then
        error stop 'Pie axis ranges are invalid'
    end if

    plot_width_px = real(fig_ptr%state%width, dp) * &
                    max(0.0_dp, 1.0_dp - fig_ptr%state%margin_left - &
                    fig_ptr%state%margin_right)
    plot_height_px = real(fig_ptr%state%height, dp) * &
                     max(0.0_dp, 1.0_dp - fig_ptr%state%margin_bottom - &
                     fig_ptr%state%margin_top)

    if (plot_width_px <= 0.0_dp .or. plot_height_px <= 0.0_dp) then
        error stop 'Plot area dimensions are invalid'
    end if

    scale_x = plot_width_px / range_x
    scale_y = plot_height_px / range_y
    aspect_diff = abs(scale_x - scale_y) / max(scale_x, scale_y)

    if (aspect_diff > 1.0e-6_dp) then
        write(*,*) 'scale_x = ', scale_x, 'scale_y = ', scale_y
        error stop 'Pie axis scaling is not equal'
    end if

    print *, '✓ Pie chart uses equal axis scaling by default'
end program test_pie_axis_equal
