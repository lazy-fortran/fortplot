program test_xscale_yscale_api
    !! Adversarial tests for issue #1663: xscale/yscale pyplot-style wrappers
    !!
    !! These tests verify:
    !!   1. xscale/yscale exist as pyplot-style aliases (no set_ prefix)
    !!   2. linthresh is the canonical symlog parameter name
    !!   3. threshold is a deprecated alias that still works
    !!   4. All four scales ('linear', 'log', 'symlog', 'logit') are accepted
    !!   5. Edge cases: zero linthresh, negative linthresh, large linthresh
    !!   6. Both x and y axes work independently
    !!   7. base and linscale kwargs are accepted for symlog

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, xscale, yscale, set_xscale, set_yscale, plot, savefig, &
                        get_global_figure
    use fortplot_figure_core, only: figure_t

    implicit none

    call test_xscale_pyplot_alias()
    call test_yscale_pyplot_alias()
    call test_linthresh_canonical()
    call test_threshold_deprecated_alias()
    call test_all_four_scales()
    call test_symlog_kwargs_base_linscale()
    call test_edge_case_linthresh_zero()
    call test_edge_case_linthresh_negative()
    call test_edge_case_linthresh_large()
    call test_independent_axes()
    call test_xscale_via_figure_method()
    call test_yscale_via_figure_method()

    print *, "All xscale/yscale API tests passed."

contains

    subroutine test_xscale_pyplot_alias()
        !! REQ-001: xscale() must exist as a pyplot-style alias (no set_ prefix)
        type(figure_t), pointer :: f

        call figure()
        call xscale('log')

        f => get_global_figure()
        if (trim(f%state%xscale) /= 'log') then
            print *, "FAIL: xscale('log') did not set x-scale to 'log', got '", &
                trim(f%state%xscale), "'"
            stop 1
        end if
    end subroutine test_xscale_pyplot_alias

    subroutine test_yscale_pyplot_alias()
        !! REQ-001: yscale() must exist as a pyplot-style alias (no set_ prefix)
        type(figure_t), pointer :: f

        call figure()
        call yscale('log')

        f => get_global_figure()
        if (trim(f%state%yscale) /= 'log') then
            print *, "FAIL: yscale('log') did not set y-scale to 'log', got '", &
                trim(f%state%yscale), "'"
            stop 1
        end if
    end subroutine test_yscale_pyplot_alias

    subroutine test_linthresh_canonical()
        !! REQ-003: linthresh is the canonical symlog parameter name
        type(figure_t), pointer :: f

        call figure()
        call set_xscale('symlog', linthresh=0.5_wp)

        f => get_global_figure()
        if (trim(f%state%xscale) /= 'symlog') then
            print *, "FAIL: set_xscale('symlog', linthresh=...) did not set scale"
            stop 1
        end if
        if (abs(f%state%symlog_threshold - 0.5_wp) > 1.0e-12_wp) then
            print *, "FAIL: linthresh=0.5 not stored, got ", f%state%symlog_threshold
            stop 1
        end if
    end subroutine test_linthresh_canonical

    subroutine test_threshold_deprecated_alias()
        !! REQ-003: threshold keyword must still work (deprecated alias)
        type(figure_t), pointer :: f

        call figure()
        call set_xscale('symlog', threshold=2.0_wp)

        f => get_global_figure()
        if (abs(f%state%symlog_threshold - 2.0_wp) > 1.0e-12_wp) then
            print *, "FAIL: threshold=2.0 not honoured, got ", f%state%symlog_threshold
            stop 1
        end if
    end subroutine test_threshold_deprecated_alias

    subroutine test_all_four_scales()
        !! REQ-004: all four matplotlib scales must be accepted
        character(len=6), parameter :: scales(4) = [&
            'linear', 'log   ', 'symlog', 'logit '&
        ]
        type(figure_t), pointer :: f
        integer :: i

        do i = 1, 4
            call figure()
            call xscale(scales(i))
            call yscale(scales(i))

            f => get_global_figure()
            if (trim(f%state%xscale) /= trim(scales(i))) then
                print *, "FAIL: xscale('", trim(scales(i)), "') did not set x-scale"
                stop 1
            end if
            if (trim(f%state%yscale) /= trim(scales(i))) then
                print *, "FAIL: yscale('", trim(scales(i)), "') did not set y-scale"
                stop 1
            end if
        end do
    end subroutine test_all_four_scales

    subroutine test_symlog_kwargs_base_linscale()
        !! REQ-002: base and linscale kwargs must be accepted for symlog
        call figure()
        call set_xscale('symlog', linthresh=1.0_wp, base=2.0_wp, linscale=1.5_wp)
        call set_yscale('symlog', linthresh=1.0_wp, base=10.0_wp, linscale=2.0_wp)
        ! Verify via xscale/yscale aliases too
        call xscale('symlog', linthresh=0.1_wp, base=10.0_wp, linscale=1.0_wp)
        call yscale('symlog', linthresh=0.1_wp, base=10.0_wp, linscale=1.0_wp)
    end subroutine test_symlog_kwargs_base_linscale

    subroutine test_edge_case_linthresh_zero()
        !! Edge case: linthresh=0 should not crash (symlog with zero threshold)
        call figure()
        call set_xscale('symlog', linthresh=0.0_wp)
        call yscale('symlog', linthresh=0.0_wp)
    end subroutine test_edge_case_linthresh_zero

    subroutine test_edge_case_linthresh_negative()
        !! Edge case: negative linthresh should be accepted (matplotlib allows it)
        call figure()
        call set_xscale('symlog', linthresh=-1.0_wp)
        call yscale('symlog', linthresh=-1.0_wp)
    end subroutine test_edge_case_linthresh_negative

    subroutine test_edge_case_linthresh_large()
        !! Edge case: very large linthresh value
        call figure()
        call set_xscale('symlog', linthresh=1.0e10_wp)
        call yscale('symlog', linthresh=1.0e10_wp)
    end subroutine test_edge_case_linthresh_large

    subroutine test_independent_axes()
        !! Both axes must be settable independently
        type(figure_t), pointer :: f

        call figure()
        call xscale('log')
        call yscale('linear')

        f => get_global_figure()
        if (trim(f%state%xscale) /= 'log') then
            print *, "FAIL: x-scale should be 'log', got '", trim(f%state%xscale), "'"
            stop 1
        end if
        if (trim(f%state%yscale) /= 'linear') then
            print *, "FAIL: y-scale should be 'linear', got '", trim(f%state%yscale), "'"
            stop 1
        end if

        ! Swap
        call xscale('linear')
        call yscale('log')

        if (trim(f%state%xscale) /= 'linear') then
            print *, "FAIL: x-scale should be 'linear' after swap"
            stop 1
        end if
        if (trim(f%state%yscale) /= 'log') then
            print *, "FAIL: y-scale should be 'log' after swap"
            stop 1
        end if
    end subroutine test_independent_axes

    subroutine test_xscale_via_figure_method()
        !! OO-style: figure_t%set_xscale must still work
        type(figure_t), pointer :: f

        call figure()
        f => get_global_figure()

        call f%set_xscale('log')
        call f%set_yscale('symlog', 0.5_wp)

        if (trim(f%state%xscale) /= 'log') then
            print *, "FAIL: fig%set_xscale('log') did not work"
            stop 1
        end if
        if (trim(f%state%yscale) /= 'symlog') then
            print *, "FAIL: fig%set_yscale('symlog') did not work"
            stop 1
        end if
        if (abs(f%state%symlog_threshold - 0.5_wp) > 1.0e-12_wp) then
            print *, "FAIL: symlog_threshold not set via OO API"
            stop 1
        end if
    end subroutine test_xscale_via_figure_method

    subroutine test_yscale_via_figure_method()
        !! OO-style: yscale alias must also work via pyplot facade
        call figure()
        ! yscale() should delegate to set_yscale internally
        call yscale('logit')
    end subroutine test_yscale_via_figure_method

end program test_xscale_yscale_api
