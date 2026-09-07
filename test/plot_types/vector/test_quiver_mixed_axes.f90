program test_quiver_mixed_axes
    !! A distant line changes Matplotlib's nonlinear quiver derivative epsilon.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t, figure, plot, quiver, xlim, ylim, set_xscale, savefig
    use fortplot_matplotlib_session, only: get_global_figure
    use fortplot_figure_data_ranges, only: collect_figure_data_ranges
    use fortplot_test_helpers, only: test_initialize_environment, test_get_temp_path
    implicit none
    character(len=:), allocatable :: prefix
    class(figure_t), pointer :: current
    real(wp) :: bounds(4)
    logical :: has_data

    call test_initialize_environment('quiver_mixed_axes')
    prefix = test_get_temp_path('mixed_axes')
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot([10000.0_wp], [1.0_wp], color=[0.0_wp, 0.0_wp, 0.0_wp])
    call quiver([10.0_wp], [1.0_wp], [10.0_wp], [1.0_wp], &
        angles='xy', scale=35.0_wp, &
        color=[31.0_wp, 119.0_wp, 180.0_wp]/255.0_wp)
    call set_xscale('log')
    call xlim(1.0_wp, 10000.0_wp)
    call ylim(0.0_wp, 2.0_wp)
    current => get_global_figure()
    call collect_figure_data_ranges(current%plots, current%plot_count, &
        bounds(1), bounds(2), bounds(3), bounds(4), &
        has_data)
    if (.not. has_data) error stop 'mixed axes have no data extent'
    ! Actual Matplotlib ax.dataLim.extents for the same line and quiver.
    if (any(abs(bounds - [10.0_wp, 10000.0_wp, 1.0_wp, 1.0_wp]) > 1.0e-9_wp)) &
        error stop 'mixed data extent differs from Matplotlib'
    call savefig(prefix//'.svg')
    call savefig(prefix//'.png')
    call savefig(prefix//'.pdf')
end program test_quiver_mixed_axes
