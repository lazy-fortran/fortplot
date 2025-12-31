program test_tight_layout
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_matplotlib, only: figure, subplots, subplot, plot, title, xlabel, &
                                   ylabel, suptitle, tight_layout, savefig
    implicit none

    logical :: all_passed
    all_passed = .true.

    print *, 'Running tight_layout tests...'

    call test_tight_layout_oo_basic(all_passed)
    call test_tight_layout_oo_with_params(all_passed)
    call test_tight_layout_stateful_basic(all_passed)
    call test_tight_layout_with_subplots(all_passed)

    if (all_passed) then
        print *, 'All tight_layout tests PASSED!'
    else
        print *, 'Some tight_layout tests FAILED!'
        stop 1
    end if

contains

    subroutine test_tight_layout_oo_basic(passed)
        logical, intent(inout) :: passed
        type(figure_t) :: fig
        real(wp), dimension(5) :: x, y

        print *, 'Testing: tight_layout OO basic'

        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        y = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]

        call fig%initialize(640, 480)
        call fig%plot(x, y, label='data')
        call fig%set_title('Test Title')
        call fig%set_xlabel('X Axis')
        call fig%set_ylabel('Y Axis')
        call fig%tight_layout()

        if (fig%state%tight_layout_enabled) then
            print *, '  PASS: tight_layout enabled flag set'
        else
            print *, '  FAIL: tight_layout enabled flag not set'
            passed = .false.
        end if

        if (abs(fig%state%tight_pad - 1.08_wp) < 1.0d-6) then
            print *, '  PASS: default pad value correct'
        else
            print *, '  FAIL: default pad value incorrect'
            passed = .false.
        end if
    end subroutine test_tight_layout_oo_basic

    subroutine test_tight_layout_oo_with_params(passed)
        logical, intent(inout) :: passed
        type(figure_t) :: fig

        print *, 'Testing: tight_layout OO with parameters'

        call fig%initialize(640, 480)
        call fig%tight_layout(pad=2.0_wp, w_pad=0.5_wp, h_pad=0.3_wp)

        if (abs(fig%state%tight_pad - 2.0_wp) < 1.0d-6) then
            print *, '  PASS: custom pad value set'
        else
            print *, '  FAIL: custom pad value not set'
            passed = .false.
        end if

        if (abs(fig%state%tight_w_pad - 0.5_wp) < 1.0d-6) then
            print *, '  PASS: custom w_pad value set'
        else
            print *, '  FAIL: custom w_pad value not set'
            passed = .false.
        end if

        if (abs(fig%state%tight_h_pad - 0.3_wp) < 1.0d-6) then
            print *, '  PASS: custom h_pad value set'
        else
            print *, '  FAIL: custom h_pad value not set'
            passed = .false.
        end if
    end subroutine test_tight_layout_oo_with_params

    subroutine test_tight_layout_stateful_basic(passed)
        logical, intent(inout) :: passed
        real(wp), dimension(5) :: x, y

        print *, 'Testing: tight_layout stateful interface'

        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        y = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]

        call figure(figsize=[6.4_wp, 4.8_wp])
        call plot(x, y)
        call title('Stateful Test')
        call xlabel('X')
        call ylabel('Y')
        call tight_layout()
        call savefig('/tmp/test_tight_layout_stateful.png')

        print *, '  PASS: stateful tight_layout executed without error'
    end subroutine test_tight_layout_stateful_basic

    subroutine test_tight_layout_with_subplots(passed)
        logical, intent(inout) :: passed
        type(figure_t) :: fig
        real(wp), dimension(5) :: x, y1, y2, y3, y4

        print *, 'Testing: tight_layout with subplots'

        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        y1 = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]
        y2 = [25.0_wp, 16.0_wp, 9.0_wp, 4.0_wp, 1.0_wp]
        y3 = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        y4 = [5.0_wp, 4.0_wp, 3.0_wp, 2.0_wp, 1.0_wp]

        call fig%initialize(800, 600)
        call fig%subplots(2, 2)

        call fig%subplot_plot(1, 1, x, y1, label='Plot 1')
        call fig%subplot_set_title(1, 1, 'Subplot (1,1)')
        call fig%subplot_set_xlabel(1, 1, 'X axis')
        call fig%subplot_set_ylabel(1, 1, 'Y axis')

        call fig%subplot_plot(1, 2, x, y2, label='Plot 2')
        call fig%subplot_set_title(1, 2, 'Subplot (1,2)')
        call fig%subplot_set_xlabel(1, 2, 'X axis')
        call fig%subplot_set_ylabel(1, 2, 'Y axis')

        call fig%subplot_plot(2, 1, x, y3, label='Plot 3')
        call fig%subplot_set_title(2, 1, 'Subplot (2,1)')
        call fig%subplot_set_xlabel(2, 1, 'X axis')
        call fig%subplot_set_ylabel(2, 1, 'Y axis')

        call fig%subplot_plot(2, 2, x, y4, label='Plot 4')
        call fig%subplot_set_title(2, 2, 'Subplot (2,2)')
        call fig%subplot_set_xlabel(2, 2, 'X axis')
        call fig%subplot_set_ylabel(2, 2, 'Y axis')

        call fig%suptitle('2x2 Subplot Grid')
        call fig%tight_layout()

        if (fig%state%tight_layout_enabled) then
            print *, '  PASS: tight_layout enabled with subplots'
        else
            print *, '  FAIL: tight_layout not enabled with subplots'
            passed = .false.
        end if

        call fig%savefig('/tmp/test_tight_layout_subplots.png')
        print *, '  PASS: saved subplot figure with tight_layout'
    end subroutine test_tight_layout_with_subplots

end program test_tight_layout
