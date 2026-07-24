program test_boxplot_category_ticks
    !! A box plot's category axis is discrete: matplotlib's bxp places one tick
    !! per box and nowhere else. Leaving it to the linear locator produced ticks
    !! between the boxes -- three boxes at 1, 2, 3 were labelled
    !! 1.0, 1.5, 2.0, 2.5, 3.0, and the half-steps mark nothing.
    !!
    !! Oracle (matplotlib 3.11):
    !!   ax.boxplot([a, b, c]) -> ax.get_xticks() == [1, 2, 3]
    !!                            [t.get_text() ...] == ['1', '2', '3']

    use fortplot, only: figure_t, wp
    implicit none

    integer :: failures

    failures = 0

    call check_vertical_positions(failures)
    call check_labels_are_integral(failures)
    call check_fractional_position_label(failures)
    call check_explicit_ticks_win(failures)

    if (failures > 0) then
        print *, 'FAIL:', failures, 'assertion(s) failed'
        stop 1
    end if
    print *, 'PASS: boxplot pins the category axis to the box positions'

contains

    subroutine build_three_boxes(fig)
        type(figure_t), intent(out) :: fig
        real(wp) :: a(9), b(9), c(9)
        integer :: i

        do i = 1, 9
            a(i) = real(i, wp)
            b(i) = real(i, wp) + 1.0_wp
            c(i) = real(i, wp) + 2.0_wp
        end do

        call fig%initialize(640, 480)
        call fig%boxplot(a, position=1.0_wp, width=0.6_wp, label='A')
        call fig%boxplot(b, position=2.0_wp, width=0.6_wp, label='B')
        call fig%boxplot(c, position=3.0_wp, width=0.6_wp, label='C')
    end subroutine build_three_boxes

    subroutine check_vertical_positions(fails)
        integer, intent(inout) :: fails
        type(figure_t) :: fig
        integer :: i
        real(wp), parameter :: expected(3) = [1.0_wp, 2.0_wp, 3.0_wp]

        call build_three_boxes(fig)

        if (.not. fig%state%custom_xticks_set) then
            print *, 'FAIL: boxplot did not pin the x ticks'
            fails = fails + 1
            return
        end if
        if (size(fig%state%custom_xtick_positions) /= 3) then
            print *, 'FAIL: expected 3 ticks, got ', &
                size(fig%state%custom_xtick_positions)
            fails = fails + 1
            return
        end if
        do i = 1, 3
            if (abs(fig%state%custom_xtick_positions(i) - expected(i)) > 1.0e-9_wp) then
                print *, 'FAIL: tick', i, '=', &
                    fig%state%custom_xtick_positions(i), 'expected', expected(i)
                fails = fails + 1
            end if
        end do
    end subroutine check_vertical_positions

    subroutine check_labels_are_integral(fails)
        integer, intent(inout) :: fails
        type(figure_t) :: fig
        character(len=*), parameter :: expected(3) = ['1', '2', '3']
        integer :: i

        call build_three_boxes(fig)
        if (.not. allocated(fig%state%custom_xtick_labels)) then
            print *, 'FAIL: no tick labels allocated'
            fails = fails + 1
            return
        end if
        do i = 1, 3
            if (trim(fig%state%custom_xtick_labels(i)) /= expected(i)) then
                print *, 'FAIL: label', i, '= "', &
                    trim(fig%state%custom_xtick_labels(i)), '" expected "', &
                    expected(i), '"'
                fails = fails + 1
            end if
        end do
    end subroutine check_labels_are_integral

    subroutine check_fractional_position_label(fails)
        !! A fractional position keeps only the digits it needs: '0.5', not
        !! '0.5000' and not '1' from rounding.
        integer, intent(inout) :: fails
        type(figure_t) :: fig
        real(wp) :: a(5)
        integer :: i

        do i = 1, 5
            a(i) = real(i, wp)
        end do

        call fig%initialize(640, 480)
        call fig%boxplot(a, position=0.5_wp, width=0.3_wp, label='half')

        if (.not. allocated(fig%state%custom_xtick_labels)) then
            print *, 'FAIL: no tick labels for fractional position'
            fails = fails + 1
            return
        end if
        if (trim(fig%state%custom_xtick_labels(1)) /= '0.5') then
            print *, 'FAIL: fractional label = "', &
                trim(fig%state%custom_xtick_labels(1)), '" expected "0.5"'
            fails = fails + 1
        end if
    end subroutine check_fractional_position_label

    subroutine check_explicit_ticks_win(fails)
        !! An explicit set_xticks must survive a later boxplot on the same figure.
        integer, intent(inout) :: fails
        type(figure_t) :: fig
        real(wp) :: a(5)
        integer :: i

        do i = 1, 5
            a(i) = real(i, wp)
        end do

        call fig%initialize(640, 480)
        call fig%set_xticks([0.5_wp, 1.5_wp], ['lo', 'hi'])
        call fig%boxplot(a, position=1.0_wp, width=0.6_wp, label='A')

        if (size(fig%state%custom_xtick_positions) /= 2) then
            print *, 'FAIL: boxplot overwrote caller-supplied ticks'
            fails = fails + 1
            return
        end if
        if (trim(fig%state%custom_xtick_labels(1)) /= 'lo') then
            print *, 'FAIL: caller tick label lost, got "', &
                trim(fig%state%custom_xtick_labels(1)), '"'
            fails = fails + 1
        end if
    end subroutine check_explicit_ticks_win

end program test_boxplot_category_ticks
