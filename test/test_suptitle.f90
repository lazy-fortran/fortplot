program test_suptitle
    !! Test suite for suptitle functionality (fixes #1480)
    !! Tests figure-level title above subplots

    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t, subplots, suptitle, plot, savefig, &
                        subplot, xlabel, ylabel, title
    implicit none

    logical :: all_passed

    print *, 'Running suptitle tests...'

    all_passed = .true.

    call test_suptitle_oo_interface(all_passed)
    call test_suptitle_stateful_interface(all_passed)
    call test_suptitle_with_fontsize(all_passed)
    call test_suptitle_rendering(all_passed)

    if (all_passed) then
        print *, 'All suptitle tests PASSED!'
    else
        print *, 'Some suptitle tests FAILED!'
        stop 1
    end if

contains

    subroutine test_suptitle_oo_interface(passed)
        !! Test suptitle with OO interface
        logical, intent(inout) :: passed
        type(figure_t) :: fig
        real(wp), parameter :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), parameter :: y(5) = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]

        print *, 'Testing: suptitle OO interface'

        call fig%initialize(800, 600)
        call fig%subplots(2, 2)
        call fig%suptitle('Overall Figure Title')

        if (allocated(fig%state%suptitle)) then
            if (trim(fig%state%suptitle) == 'Overall Figure Title') then
                print *, '  PASS: suptitle set correctly'
            else
                print *, '  FAIL: suptitle text mismatch'
                passed = .false.
            end if
        else
            print *, '  FAIL: suptitle not allocated'
            passed = .false.
        end if

        call fig%subplot_plot(1, 1, x, y)
        call fig%subplot_plot(1, 2, x, y*2.0_wp)
        call fig%subplot_plot(2, 1, x, y*0.5_wp)
        call fig%subplot_plot(2, 2, x, y*1.5_wp)

        print *, 'Test completed'
    end subroutine test_suptitle_oo_interface

    subroutine test_suptitle_stateful_interface(passed)
        !! Test suptitle with stateful interface
        logical, intent(inout) :: passed
        real(wp), parameter :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), parameter :: y(5) = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]

        print *, 'Testing: suptitle stateful interface'

        call subplots(2, 2)
        call suptitle('Stateful Figure Title')

        call subplot(2, 2, 1)
        call plot(x, y)
        call title('Subplot 1')

        call subplot(2, 2, 2)
        call plot(x, y*2.0_wp)
        call title('Subplot 2')

        call subplot(2, 2, 3)
        call plot(x, y*0.5_wp)
        call title('Subplot 3')

        call subplot(2, 2, 4)
        call plot(x, y*1.5_wp)
        call title('Subplot 4')

        print *, '  PASS: stateful suptitle call succeeded'
        print *, 'Test completed'
    end subroutine test_suptitle_stateful_interface

    subroutine test_suptitle_with_fontsize(passed)
        !! Test suptitle with custom font size
        logical, intent(inout) :: passed
        type(figure_t) :: fig

        print *, 'Testing: suptitle with fontsize'

        call fig%initialize(800, 600)
        call fig%subplots(2, 1)
        call fig%suptitle('Large Title', fontsize=18.0_wp)

        if (abs(fig%state%suptitle_fontsize - 18.0_wp) < 0.001_wp) then
            print *, '  PASS: fontsize set correctly'
        else
            print *, '  FAIL: fontsize not set correctly'
            passed = .false.
        end if

        print *, 'Test completed'
    end subroutine test_suptitle_with_fontsize

    subroutine test_suptitle_rendering(passed)
        !! Test that suptitle renders correctly to PNG output
        logical, intent(inout) :: passed
        type(figure_t) :: fig
        real(wp), parameter :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), parameter :: y(5) = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]
        logical :: file_exists
        integer :: status

        print *, 'Testing: suptitle rendering to PNG'

        call fig%initialize(800, 600, backend='png')
        call fig%subplots(2, 2)
        call fig%suptitle('Test Suptitle Rendering', fontsize=16.0_wp)

        call fig%subplot_plot(1, 1, x, y, label='Plot 1')
        call fig%subplot_set_title(1, 1, 'Subplot 1')

        call fig%subplot_plot(1, 2, x, y*2.0_wp, label='Plot 2')
        call fig%subplot_set_title(1, 2, 'Subplot 2')

        call fig%subplot_plot(2, 1, x, y*0.5_wp, label='Plot 3')
        call fig%subplot_set_title(2, 1, 'Subplot 3')

        call fig%subplot_plot(2, 2, x, y*1.5_wp, label='Plot 4')
        call fig%subplot_set_title(2, 2, 'Subplot 4')

        call fig%savefig_with_status('/tmp/test_suptitle.png', status)

        if (status == 0) then
            inquire(file='/tmp/test_suptitle.png', exist=file_exists)
            if (file_exists) then
                print *, '  PASS: PNG file created with suptitle'
            else
                print *, '  FAIL: PNG file not found'
                passed = .false.
            end if
        else
            print *, '  FAIL: savefig returned error status'
            passed = .false.
        end if

        print *, 'Test completed'
    end subroutine test_suptitle_rendering

end program test_suptitle
