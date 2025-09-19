program test_scatter_color_cycle
    !! Verify scatter plots share the default color cycle with line plots
    use fortplot, only: figure_t, wp
    use fortplot_figure_plot_management, only: next_plot_color
    implicit none

    type(figure_t) :: fig
    real(wp) :: x(5), y_line(5), y_scatter(5)
    real(wp) :: expected_color(3)
    real(wp) :: scatter_color(3)

    integer :: i

    x = [(real(i, wp), i = 1, size(x))]
    y_line = x
    y_scatter = x + 1.0_wp

    call fig%initialize()

    call fig%plot(x, y_line)

    expected_color = next_plot_color(fig%state)

    call fig%scatter(x, y_scatter)

    scatter_color = fig%plots(fig%plot_count)%color

    if (any(abs(scatter_color - expected_color) > 1.0e-12_wp)) then
        print *, 'FAIL: scatter default color diverged from shared cycle'
        print *, '  expected:', expected_color
        print *, '  actual  :', scatter_color
        stop 1
    end if

    print *, 'PASS: scatter uses shared default color cycle'
end program test_scatter_color_cycle
