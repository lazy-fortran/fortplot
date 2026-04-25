program test_mathtext_power_of_ten
    !! Regression test for issue #1809: convert_power_of_ten outputs empty
    !! string for 10^0 instead of 1.

    use fortplot_ascii_mathtext, only: sanitize_ascii_text
    implicit none

    character(len=64) :: out
    integer :: olen
    integer :: fail_count

    fail_count = 0

    call check("$10^{0}$", "1", fail_count)
    call check("$-10^{0}$", "-1", fail_count)
    call check("$10^{3}$", "1e3", fail_count)
    call check("$-10^{3}$", "-1e3", fail_count)
    call check("$10^{-2}$", "1e-2", fail_count)
    call check("$10^{00}$", "1", fail_count)
    call check("$x10^{0}y$", "x1y", fail_count)
    call check("$10^{10}$", "1e10", fail_count)

    if (fail_count > 0) then
        print *, "FAIL:", fail_count, "assertion(s) failed"
        stop 1
    end if

    print *, "PASS: all sanitize_ascii_text mathtext cases correct"

contains

    subroutine check(input, expected, fails)
        character(len=*), intent(in) :: input, expected
        integer, intent(inout) :: fails

        call sanitize_ascii_text(input, out, olen)
        if (out(1:olen) /= expected) then
            fails = fails + 1
            print *, "FAIL: sanitize('" // trim(input) // &
                     "') = '" // out(1:olen) // "', expected '" // &
                     trim(expected) // "'"
        else
            print *, "PASS: '" // trim(input) // "' -> '" // out(1:olen) // "'"
        end if
    end subroutine check

end program test_mathtext_power_of_ten
