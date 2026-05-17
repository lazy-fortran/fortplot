program test_set_aspect
    use fortplot_figure, only: figure_t
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    call test_aspect_mode_defaults()
    call test_set_aspect_equal()
    call test_set_aspect_auto()
    call test_set_aspect_numeric()
    call test_set_aspect_invalid_ratio()
    call test_stateful_axis_interface()
    print *, "All set_aspect tests passed!"

contains

    subroutine test_aspect_mode_defaults()
        type(figure_t) :: fig

        if (fig%state%aspect_mode /= 'auto') then
            print *, "FAIL: Default aspect_mode should be 'auto'"
            print *, "Got: '", trim(fig%state%aspect_mode), "'"
            stop 1
        end if

        if (abs(fig%state%aspect_ratio - 1.0_wp) > 1.0e-10_wp) then
            print *, "FAIL: Default aspect_ratio should be 1.0"
            print *, "Got: ", fig%state%aspect_ratio
            stop 1
        end if
    end subroutine test_aspect_mode_defaults

    subroutine test_set_aspect_equal()
        type(figure_t) :: fig

        call fig%set_aspect('equal')

        if (fig%state%aspect_mode /= 'equal') then
            print *, "FAIL: aspect_mode should be 'equal'"
            print *, "Got: '", trim(fig%state%aspect_mode), "'"
            stop 1
        end if

        if (abs(fig%state%aspect_ratio - 1.0_wp) > 1.0e-10_wp) then
            print *, "FAIL: aspect_ratio should be 1.0 for 'equal'"
            print *, "Got: ", fig%state%aspect_ratio
            stop 1
        end if
    end subroutine test_set_aspect_equal

    subroutine test_set_aspect_auto()
        type(figure_t) :: fig

        call fig%set_aspect('equal')
        call fig%set_aspect('auto')

        if (fig%state%aspect_mode /= 'auto') then
            print *, "FAIL: aspect_mode should be 'auto'"
            print *, "Got: '", trim(fig%state%aspect_mode), "'"
            stop 1
        end if
    end subroutine test_set_aspect_auto

    subroutine test_set_aspect_numeric()
        type(figure_t) :: fig

        call fig%set_aspect(2.0_wp)

        if (fig%state%aspect_mode /= 'numeric') then
            print *, "FAIL: aspect_mode should be 'numeric'"
            print *, "Got: '", trim(fig%state%aspect_mode), "'"
            stop 1
        end if

        if (abs(fig%state%aspect_ratio - 2.0_wp) > 1.0e-10_wp) then
            print *, "FAIL: aspect_ratio should be 2.0"
            print *, "Got: ", fig%state%aspect_ratio
            stop 1
        end if

        call fig%set_aspect(0.5_wp)

        if (abs(fig%state%aspect_ratio - 0.5_wp) > 1.0e-10_wp) then
            print *, "FAIL: aspect_ratio should be 0.5"
            print *, "Got: ", fig%state%aspect_ratio
            stop 1
        end if
    end subroutine test_set_aspect_numeric

    subroutine test_set_aspect_invalid_ratio()
        type(figure_t) :: fig

        call fig%set_aspect(2.0_wp)
        call fig%set_aspect(-1.0_wp)

        if (abs(fig%state%aspect_ratio - 2.0_wp) > 1.0e-10_wp) then
            print *, "FAIL: aspect_ratio should remain 2.0 after invalid input"
            print *, "Got: ", fig%state%aspect_ratio
            stop 1
        end if

        call fig%set_aspect(0.0_wp)

        if (abs(fig%state%aspect_ratio - 2.0_wp) > 1.0e-10_wp) then
            print *, "FAIL: aspect_ratio should remain 2.0 after zero input"
            print *, "Got: ", fig%state%aspect_ratio
            stop 1
        end if
    end subroutine test_set_aspect_invalid_ratio

    subroutine test_stateful_axis_interface()
        use fortplot_matplotlib, only: axis, figure, savefig
        use fortplot_global, only: global_figure

        call figure()
        call axis('equal')

        if (global_figure%state%aspect_mode /= 'equal') then
            print *, "FAIL: Stateful axis('equal') should set aspect_mode"
            stop 1
        end if

        call axis(0.5_wp)

        if (global_figure%state%aspect_mode /= 'numeric') then
            print *, "FAIL: Stateful axis(0.5) should set aspect_mode to numeric"
            stop 1
        end if

        if (abs(global_figure%state%aspect_ratio - 0.5_wp) > 1.0e-10_wp) then
            print *, "FAIL: Stateful axis(0.5) should set aspect_ratio"
            stop 1
        end if

        call axis('auto')

        if (global_figure%state%aspect_mode /= 'auto') then
            print *, "FAIL: Stateful axis('auto') should reset aspect_mode"
            stop 1
        end if
    end subroutine test_stateful_axis_interface

end program test_set_aspect
