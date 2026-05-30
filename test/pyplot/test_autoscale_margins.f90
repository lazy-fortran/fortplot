program test_autoscale_margins
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    implicit none

    logical :: failed

    failed = .false.
    call test_scatter_uses_padded_axis_limits(failed)
    call test_histogram_keeps_sticky_baseline(failed)

    if (failed) stop 1
    print *, 'autoscale margin tests passed'

contains

    subroutine assert_true(name, condition, failed)
        character(len=*), intent(in) :: name
        logical, intent(in) :: condition
        logical, intent(inout) :: failed

        if (.not. condition) then
            failed = .true.
            print *, 'FAIL: ', trim(name)
        end if
    end subroutine assert_true

    subroutine render_figure(fig)
        type(figure_t), intent(inout) :: fig
        real(wp), allocatable :: rgb(:, :, :)

        allocate (rgb(fig%get_width(), fig%get_height(), 3))
        call fig%extract_rgb_data_for_animation(rgb)
        deallocate (rgb)
    end subroutine render_figure

    subroutine test_scatter_uses_padded_axis_limits(failed)
        logical, intent(inout) :: failed
        type(figure_t) :: fig
        real(wp) :: x(20), y(20)
        real(wp) :: expected_x_min, expected_x_max
        real(wp) :: expected_y_min, expected_y_max
        integer :: i

        x = [(real(i, wp), i = 1, 20)]
        y = x**2

        call fig%initialize(640, 480, backend='png')
        call fig%scatter(x, y)
        call render_figure(fig)

        expected_x_min = minval(x) - 0.05_wp * (maxval(x) - minval(x))
        expected_x_max = maxval(x) + 0.05_wp * (maxval(x) - minval(x))
        expected_y_min = minval(y) - 0.05_wp * (maxval(y) - minval(y))
        expected_y_max = maxval(y) + 0.05_wp * (maxval(y) - minval(y))

        call assert_true('scatter expands x_min by matplotlib''s 5% margin', &
                         abs(fig%state%x_min - expected_x_min) < 1.0e-9_wp, failed)
        call assert_true('scatter expands x_max by matplotlib''s 5% margin', &
                         abs(fig%state%x_max - expected_x_max) < 1.0e-9_wp, failed)
        call assert_true('scatter expands y_min by matplotlib''s 5% margin', &
                         abs(fig%state%y_min - expected_y_min) < 1.0e-9_wp, failed)
        call assert_true('scatter expands y_max by matplotlib''s 5% margin', &
                         abs(fig%state%y_max - expected_y_max) < 1.0e-9_wp, failed)
    end subroutine test_scatter_uses_padded_axis_limits

    subroutine test_histogram_keeps_sticky_baseline(failed)
        logical, intent(inout) :: failed
        type(figure_t) :: fig
        real(wp) :: data(8)

        data = [0.0_wp, 0.2_wp, 0.4_wp, 0.6_wp, 0.8_wp, 1.0_wp, 1.2_wp, 1.4_wp]

        call fig%initialize(640, 480, backend='png')
        call fig%hist(data, bins=4, color='steelblue')
        call render_figure(fig)

        call assert_true('histogram keeps the lower baseline sticky at zero', &
                         abs(fig%state%y_min) < 1.0e-12_wp, failed)
    end subroutine test_histogram_keeps_sticky_baseline

end program test_autoscale_margins
