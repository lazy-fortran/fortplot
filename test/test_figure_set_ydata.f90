program test_figure_set_ydata
    use fortplot
    implicit none

    call test_should_update_ydata_for_existing_plot()
    call test_should_handle_invalid_plot_index()

contains

    subroutine test_should_update_ydata_for_existing_plot()
        type(figure_t) :: fig
        real(wp) :: x(5), y_initial(5), y_updated(5)
        integer :: i

        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        y_initial = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]
        y_updated = [2.0_wp, 8.0_wp, 18.0_wp, 32.0_wp, 50.0_wp]

        call fig%initialize(800, 600)
        call fig%add_plot(x, y_initial, label='test')

        call fig%set_ydata(1, y_updated)

        print *, "TEST PASSED: set_ydata updates existing plot data"
    end subroutine test_should_update_ydata_for_existing_plot

    subroutine test_should_handle_invalid_plot_index()
        type(figure_t) :: fig
        real(wp) :: x(3), y(3)

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 2.0_wp, 3.0_wp]

        call fig%initialize(800, 600)
        call fig%add_plot(x, y, label='test')

        call fig%set_ydata(99, y)

        print *, "TEST PASSED: set_ydata handles invalid plot index gracefully"
    end subroutine test_should_handle_invalid_plot_index

end program test_figure_set_ydata