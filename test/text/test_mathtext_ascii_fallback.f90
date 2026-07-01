program test_mathtext_ascii_fallback
    !! Issue #2064: the ASCII/text fallback for math labels must stay readable,
    !! preserving grouping (sqrt(...), ^(...)), Greek transliteration with token
    !! boundaries, and factor boundaries instead of concatenating tokens.

    use fortplot_ascii_mathtext, only: sanitize_ascii_text
    implicit none

    character(len=*), parameter :: s2 = achar(194)//achar(178)
    character(len=256) :: out
    integer :: olen, fail_count

    fail_count = 0

    call sanitize_ascii_text( &
        'Gaussian: \rho(\xi) = e^{-\xi'//s2//'/2\sigma'//s2//'}/\sqrt{2\pi\sigma' &
        //s2//'}', out, olen)
    call assert_contains(out(1:olen), 'rho(xi)', fail_count)
    call assert_contains(out(1:olen), 'e^(', fail_count)
    call assert_contains(out(1:olen), 'sqrt(', fail_count)
    call assert_not_contains(out(1:olen), 'sqrt2pisigma2', fail_count)
    call assert_not_contains(out(1:olen), 'e^-xi2', fail_count)
    call assert_not_contains(out(1:olen), 'pisigma', fail_count)
    call assert_not_contains(out(1:olen), '\', fail_count)
    call assert_not_contains(out(1:olen), '{', fail_count)
    call assert_not_contains(out(1:olen), '}', fail_count)

    call sanitize_ascii_text('\sqrt{2\pi\sigma'//s2//'}', out, olen)
    call assert_contains(out(1:olen), 'sqrt(2 pi sigma^2)', fail_count)
    call assert_not_contains(out(1:olen), 'sqrt2', fail_count)

    call sanitize_ascii_text('\xi'//s2//' e^{-\xi}', out, olen)
    call assert_contains(out(1:olen), 'xi^2 e^(-xi)', fail_count)
    call assert_not_contains(out(1:olen), 'e^-xi', fail_count)

    call sanitize_ascii_text('Modified \Gamma: f(\xi) = \xi'//s2//' e^{-\xi}', &
                             out, olen)
    call assert_contains(out(1:olen), 'Gamma: f(xi) = xi^2 e^(-xi)', fail_count)

    ! Compact power-of-ten tick labels must keep working (issue #2058 / #1809).
    call sanitize_ascii_text('$10^{3}$', out, olen)
    call assert_equals(out(1:olen), '1e3', fail_count)
    call sanitize_ascii_text('$10^{-2}$', out, olen)
    call assert_equals(out(1:olen), '1e-2', fail_count)
    call sanitize_ascii_text('$10^{0}$', out, olen)
    call assert_equals(out(1:olen), '1', fail_count)

    if (fail_count > 0) then
        print *, 'FAIL:', fail_count, 'assertion(s) failed'
        stop 1
    end if

    print *, 'PASS: all ASCII math fallback readability cases correct'

contains

    subroutine assert_contains(text, needle, fails)
        character(len=*), intent(in) :: text, needle
        integer, intent(inout) :: fails

        if (index(text, needle) == 0) then
            fails = fails + 1
            print *, "FAIL: '"//text//"' should contain '"//needle//"'"
        end if
    end subroutine assert_contains

    subroutine assert_not_contains(text, needle, fails)
        character(len=*), intent(in) :: text, needle
        integer, intent(inout) :: fails

        if (index(text, needle) /= 0) then
            fails = fails + 1
            print *, "FAIL: '"//text//"' must not contain '"//needle//"'"
        end if
    end subroutine assert_not_contains

    subroutine assert_equals(text, expected, fails)
        character(len=*), intent(in) :: text, expected
        integer, intent(inout) :: fails

        if (text /= expected) then
            fails = fails + 1
            print *, "FAIL: got '"//text//"', expected '"//expected//"'"
        end if
    end subroutine assert_equals

end program test_mathtext_ascii_fallback
