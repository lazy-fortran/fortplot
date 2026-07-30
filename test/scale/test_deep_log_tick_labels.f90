program test_deep_log_tick_labels
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_axes, only: format_tick_label
    use fortplot, only: figure, plot, savefig, set_yscale, title, xlabel, ylabel
    use fortplot_system_runtime, only: create_directory_runtime
    use fortplot_tick_formatting, only: format_log_tick_value
    implicit none

    logical :: directory_created

    call assert_label(1.0e-12_wp, '$10^{-12}$')
    call assert_label(1.0e-18_wp, '$10^{-18}$')
    call assert_utility_label(1.0e-18_wp, '10^{-18}')
    call create_directory_runtime('build/test/output', directory_created)
    if (.not. directory_created) error stop 'failed to create test output directory'
    call figure()
    call plot([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], &
              [1.0e-3_wp, 1.0e-8_wp, 1.0e-13_wp, 1.0e-18_wp])
    call set_yscale('log')
    call title('Deep logarithmic tick labels')
    call xlabel('sample')
    call ylabel('magnitude')
    call savefig('build/test/output/deep_log_tick_labels.png')

    print *, 'PASS: deep logarithmic decades retain power-of-ten labels'

contains

    subroutine assert_label(value, expected)
        real(wp), intent(in) :: value
        character(len=*), intent(in) :: expected
        character(len=50) :: actual

        actual = format_tick_label(value, 'log')
        if (trim(actual) /= expected) then
            print *, 'FAIL: expected ', expected, ' for ', value, &
                ', got ', trim(actual)
            error stop 1
        end if
    end subroutine assert_label

    subroutine assert_utility_label(value, expected)
        real(wp), intent(in) :: value
        character(len=*), intent(in) :: expected
        character(len=20) :: actual

        actual = format_log_tick_value(value)
        if (trim(actual) /= expected) then
            print *, 'FAIL: expected utility label ', expected, ' for ', value, &
                ', got ', trim(actual)
            error stop 1
        end if
    end subroutine assert_utility_label

end program test_deep_log_tick_labels
