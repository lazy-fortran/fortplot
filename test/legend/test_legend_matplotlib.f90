program test_legend_matplotlib
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, plot, bar, legend, savefig, xlim, ylim
    use fortplot_legend_layout, only: choose_best_legend_position
    use fortplot_legend_state, only: LEGEND_LOWER_LEFT, LEGEND_UPPER_LEFT
    use fortplot_test_helpers, only: test_get_temp_path
    implicit none

    character(len=12), parameter :: locations(10) = [character(len=12) :: &
        'upper right', 'upper left', 'lower left', 'lower right', &
        'right', 'center left', 'center right', 'lower center', &
        'upper center', 'center']
    real(wp) :: x(100), y(100), xs(2), ys(2), empty(0), rectangles(4, 1)
    character(len=16) :: name
    integer :: i, position

    ! Independent expectations come from the installed Matplotlib renderer in
    ! scripts/verify_legend_parity.py, including its ten-position best scorer.
    xs = [0.0_wp, 10.0_wp]
    ys = [9.5_wp, 9.5_wp]
    position = choose_best_legend_position(['crossing'], 10.0_wp, 10.0_wp, &
        1, xs, ys, 496, 369, [1, 1])
    if (position /= LEGEND_LOWER_LEFT) &
        error stop 'sparse line intersection must exclude both upper corners'
    call figure()
    call plot(xs, ys, label='crossing')
    call xlim(0.0_wp, 10.0_wp)
    call ylim(0.0_wp, 10.0_wp)
    call legend()
    call save_pair('crossing')

    rectangles(:, 1) = [6.0_wp, 0.0_wp, 10.0_wp, 10.0_wp]
    position = choose_best_legend_position(['bar'], 10.0_wp, 10.0_wp, &
        1, empty, empty, 496, 369, rectangles=rectangles)
    if (position /= LEGEND_UPPER_LEFT) &
        error stop 'bar interior must exclude right-side legend anchors'
    call figure()
    call bar([8.0_wp], [10.0_wp], width=4.0_wp, label='bar')
    call xlim(0.0_wp, 10.0_wp)
    call ylim(0.0_wp, 10.0_wp)
    call legend()
    call save_pair('bar')

    x = [(real(i - 1, wp)/5.0_wp, i=1, 100)]
    y = sin(x)
    call figure()
    call plot(x, y, label='sin(x)')
    call plot(x, cos(x), label='cos(x)')
    call legend()
    call save_pair('best')

    do i = 1, size(locations)
        call figure()
        call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp], label='series')
        call xlim(0.0_wp, 1.0_wp)
        call ylim(0.0_wp, 1.0_wp)
        call legend(trim(locations(i)))
        write (name, '("location_", I2.2)') i
        call save_pair(trim(name))
    end do
    print *, 'PASS: sparse-path legend placement; emitted PNG/PDF oracle fixtures'

contains

    subroutine save_pair(case_name)
        character(len=*), intent(in) :: case_name

        call savefig(test_get_temp_path('legend_'//case_name//'.png'))
        call savefig(test_get_temp_path('legend_'//case_name//'.pdf'))
    end subroutine save_pair

end program test_legend_matplotlib
