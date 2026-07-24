program test_tick_locations_matplotlib_oracle
    !! Pin tick locations against matplotlib rather than against ourselves.
    !!
    !! Every expected array below is the literal output of
    !!
    !!     from matplotlib import ticker
    !!     loc = ticker.MaxNLocator(steps=[1, 2, 2.5, 5, 10], nbins=target-1)
    !!     [v for v in loc.tick_values(lo, hi) if lo <= v <= hi]
    !!
    !! on matplotlib 3.11. nbins is target-1 because find_nice_tick_locations
    !! takes a tick count while MaxNLocator takes an interval count; the
    !! existing test_maxnlocator_steps documents the same convention.
    !!
    !! test_maxnlocator_steps checks the chosen step. This checks the emitted
    !! tick positions, which is what a reader of the plot actually sees: a
    !! correct step placed off a wrong origin still mislabels every tick.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_tick_calculation, only: find_nice_tick_locations
    implicit none

    integer :: failures

    failures = 0

    call check([0.0_wp, 1.0_wp], 9, &
               [0.0_wp, 0.2_wp, 0.4_wp, 0.6_wp, 0.8_wp, 1.0_wp], failures)
    call check([0.0_wp, 10.0_wp], 9, &
               [0.0_wp, 2.0_wp, 4.0_wp, 6.0_wp, 8.0_wp, 10.0_wp], failures)
    call check([0.0_wp, 18.0_wp], 9, &
               [0.0_wp, 2.5_wp, 5.0_wp, 7.5_wp, 10.0_wp, 12.5_wp, 15.0_wp, &
                17.5_wp], failures)
    call check([0.8_wp, 3.2_wp], 9, &
               [1.0_wp, 1.5_wp, 2.0_wp, 2.5_wp, 3.0_wp], failures)
    call check([-1.0_wp, 1.0_wp], 9, &
               [-1.0_wp, -0.75_wp, -0.5_wp, -0.25_wp, 0.0_wp, 0.25_wp, &
                0.5_wp, 0.75_wp, 1.0_wp], failures)
    call check([0.0_wp, 5.0_wp], 9, &
               [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp], failures)
    call check([1.0_wp, 3.0_wp], 9, &
               [1.0_wp, 1.25_wp, 1.5_wp, 1.75_wp, 2.0_wp, 2.25_wp, 2.5_wp, &
                2.75_wp, 3.0_wp], failures)
    call check([0.0_wp, 0.9_wp], 9, &
               [0.0_wp, 0.2_wp, 0.4_wp, 0.6_wp, 0.8_wp], failures)
    ! Asymmetric span whose ticks must not run to the data edge.
    call check([-2.8_wp, 2.8_wp], 9, &
               [-2.0_wp, -1.0_wp, 0.0_wp, 1.0_wp, 2.0_wp], failures)
    call check([0.0_wp, 100.0_wp], 9, &
               [0.0_wp, 20.0_wp, 40.0_wp, 60.0_wp, 80.0_wp, 100.0_wp], failures)
    call check([0.0_wp, 1200.0_wp], 9, &
               [0.0_wp, 200.0_wp, 400.0_wp, 600.0_wp, 800.0_wp, 1000.0_wp, &
                1200.0_wp], failures)
    ! Small magnitudes: the step set applies per decade, not per absolute size.
    call check([0.0_wp, 0.05_wp], 9, &
               [0.0_wp, 0.01_wp, 0.02_wp, 0.03_wp, 0.04_wp, 0.05_wp], failures)
    ! Offset origin: ticks are multiples of the step, not of the data minimum.
    call check([2.0_wp, 3.0_wp], 9, &
               [2.0_wp, 2.2_wp, 2.4_wp, 2.6_wp, 2.8_wp, 3.0_wp], failures)
    ! Coarser requests.
    call check([0.0_wp, 7.0_wp], 5, &
               [0.0_wp, 2.0_wp, 4.0_wp, 6.0_wp], failures)
    call check([0.0_wp, 13.0_wp], 5, &
               [0.0_wp, 5.0_wp, 10.0_wp], failures)

    if (failures > 0) then
        print *, 'FAIL:', failures, 'range(s) disagree with matplotlib'
        stop 1
    end if
    print *, 'PASS: tick locations match matplotlib MaxNLocator'

contains

    subroutine check(bounds, target_ticks, expected, fails)
        real(wp), intent(in) :: bounds(2)
        integer, intent(in) :: target_ticks
        real(wp), intent(in) :: expected(:)
        integer, intent(inout) :: fails

        real(wp), parameter :: TOL = 1.0e-9_wp
        real(wp) :: nice_min, nice_max, step, ticks(32)
        real(wp) :: inside(32)
        integer :: n, n_inside, i

        call find_nice_tick_locations(bounds(1), bounds(2), target_ticks, &
                                      nice_min, nice_max, step, ticks, n)

        ! matplotlib's tick_values may return ticks outside the view; both it
        ! and this comparison keep only those the axis would actually draw.
        n_inside = 0
        do i = 1, n
            if (ticks(i) < bounds(1) - TOL) cycle
            if (ticks(i) > bounds(2) + TOL) cycle
            n_inside = n_inside + 1
            inside(n_inside) = ticks(i)
        end do

        if (n_inside /= size(expected)) then
            print *, 'FAIL: range [', bounds(1), ',', bounds(2), '] target', &
                target_ticks, 'gave', n_inside, 'ticks, matplotlib gives', &
                size(expected)
            print *, '  got     :', inside(1:n_inside)
            print *, '  expected:', expected
            fails = fails + 1
            return
        end if

        do i = 1, n_inside
            if (abs(inside(i) - expected(i)) > TOL) then
                print *, 'FAIL: range [', bounds(1), ',', bounds(2), &
                    '] tick', i, '=', inside(i), 'matplotlib gives', expected(i)
                fails = fails + 1
                return
            end if
        end do
    end subroutine check

end program test_tick_locations_matplotlib_oracle
