program test_mathtext_literal_dollar
    !! A '$' only opens a math span when a second '$' closes it, matching
    !! matplotlib and the raster path in fortplot_text_layout. The text backend
    !! used to drop every '$' unconditionally, so a currency axis label such as
    !! 'Revenue (million $)' rendered as 'Revenue (million )' in .txt output
    !! while the same figure kept the sign in PNG and PDF.

    use fortplot_ascii_mathtext, only: sanitize_ascii_text
    implicit none

    character(len=64) :: out
    integer :: olen
    integer :: fail_count

    fail_count = 0

    ! Unpaired: the dollar is data, not markup.
    call check("Revenue (million $)", "Revenue (million $)", fail_count)
    call check("a $ b", "a $ b", fail_count)
    call check("$", "$", fail_count)
    call check("costs $5", "costs $5", fail_count)

    ! Paired: the delimiters are markup and are removed.
    call check("$x$", "x", fail_count)
    call check("a $x$ b", "a x b", fail_count)

    ! Paired span followed by a trailing unpaired delimiter.
    call check("$a$ b$", "a b$", fail_count)

    if (fail_count > 0) then
        print *, "FAIL:", fail_count, "assertion(s) failed"
        stop 1
    end if

    print *, "PASS: literal and paired dollar signs handled correctly"

contains

    subroutine check(input, expected, fails)
        character(len=*), intent(in) :: input, expected
        integer, intent(inout) :: fails

        call sanitize_ascii_text(input, out, olen)
        if (out(1:olen) /= expected) then
            fails = fails + 1
            print *, "FAIL: sanitize('"//trim(input)// &
                "') = '"//out(1:olen)//"', expected '"// &
                trim(expected)//"'"
        else
            print *, "PASS: '"//trim(input)//"' -> '"//out(1:olen)//"'"
        end if
    end subroutine check

end program test_mathtext_literal_dollar
