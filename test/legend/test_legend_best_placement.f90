program test_legend_best_placement
    !! Issue #1951: default legend placement uses matplotlib 'best', scoring
    !! candidate corners by overlap with the plotted artists. Explicit loc must
    !! still pin exactly.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_legend_layout, only: choose_best_legend_position
    use fortplot_legend_state, only: legend_t, create_legend, &
                                     LEGEND_UPPER_LEFT, LEGEND_UPPER_RIGHT, &
                                     LEGEND_LOWER_LEFT, LEGEND_LOWER_RIGHT, &
                                     LEGEND_EAST, LEGEND_BEST
    implicit none

    integer :: failures

    failures = 0

    call test_data_in_upper_right_moves_legend(failures)
    call test_data_in_upper_left_moves_legend(failures)
    call test_explicit_loc_tokens_pin(failures)
    call test_best_token_sets_best(failures)

    if (failures == 0) then
        print *, "ALL BEST-PLACEMENT TESTS PASSED"
    else
        print *, "FAILURES:", failures
        stop 1
    end if

contains

    subroutine test_data_in_upper_right_moves_legend(fails)
        integer, intent(inout) :: fails
        character(len=16) :: labels(2)
        real(wp) :: ax(4), ay(4)
        integer :: pos

        labels = ["Series A        ", "Series B        "]
        ! Artists clustered in the upper-right of a 0..10 x 0..10 window.
        ax = [8.5_wp, 9.5_wp, 9.0_wp, 9.8_wp]
        ay = [8.5_wp, 9.5_wp, 9.0_wp, 9.8_wp]

        pos = choose_best_legend_position(labels, 10.0_wp, 10.0_wp, 2, ax, ay, &
                                          496, 369)
        if (pos == LEGEND_UPPER_RIGHT) then
            print *, "FAIL: best kept legend over upper-right data"
            fails = fails + 1
        else
            print *, "PASS: best avoided upper-right data, chose", pos
        end if
    end subroutine test_data_in_upper_right_moves_legend

    subroutine test_data_in_upper_left_moves_legend(fails)
        integer, intent(inout) :: fails
        character(len=16) :: labels(2)
        real(wp) :: ax(4), ay(4)
        integer :: pos

        labels = ["Series A        ", "Series B        "]
        ! Artists clustered in the upper-left corner.
        ax = [0.2_wp, 1.0_wp, 0.5_wp, 1.5_wp]
        ay = [8.5_wp, 9.5_wp, 9.0_wp, 9.8_wp]

        pos = choose_best_legend_position(labels, 10.0_wp, 10.0_wp, 2, ax, ay, &
                                          496, 369)
        if (pos == LEGEND_UPPER_LEFT) then
            print *, "FAIL: best kept legend over upper-left data"
            fails = fails + 1
        else
            print *, "PASS: best avoided upper-left data, chose", pos
        end if
    end subroutine test_data_in_upper_left_moves_legend

    subroutine test_explicit_loc_tokens_pin(fails)
        integer, intent(inout) :: fails
        type(legend_t) :: leg

        leg = create_legend()
        call leg%set_position("upper left")
        call expect(leg%position, LEGEND_UPPER_LEFT, "upper left", fails)
        call leg%set_position("upper right")
        call expect(leg%position, LEGEND_UPPER_RIGHT, "upper right", fails)
        call leg%set_position("lower left")
        call expect(leg%position, LEGEND_LOWER_LEFT, "lower left", fails)
        call leg%set_position("lower right")
        call expect(leg%position, LEGEND_LOWER_RIGHT, "lower right", fails)
        call leg%set_position("east")
        call expect(leg%position, LEGEND_EAST, "east", fails)
    end subroutine test_explicit_loc_tokens_pin

    subroutine test_best_token_sets_best(fails)
        integer, intent(inout) :: fails
        type(legend_t) :: leg

        leg = create_legend()
        call leg%set_position("best")
        call expect(leg%position, LEGEND_BEST, "best", fails)
    end subroutine test_best_token_sets_best

    subroutine expect(actual, expected, name, fails)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: name
        integer, intent(inout) :: fails

        if (actual == expected) then
            print *, "PASS: loc '"//name//"' pinned"
        else
            print *, "FAIL: loc '"//name//"' got", actual, "expected", expected
            fails = fails + 1
        end if
    end subroutine expect

end program test_legend_best_placement
