program test_text_helpers_mathtext_wrap
    use fortplot_text_helpers, only: prepare_mathtext_if_needed
    implicit none

    character(len=256) :: out
    integer :: n

    call prepare_mathtext_if_needed('x^3', out, n)
    if (out(1:n) /= '$x^3$') then
        print *, 'FAIL: x^3 should wrap to $x^3$'
        stop 1
    end if

    call prepare_mathtext_if_needed('x_1', out, n)
    if (out(1:n) /= '$x_1$') then
        print *, 'FAIL: x_1 should wrap to $x_1$'
        stop 1
    end if

    call prepare_mathtext_if_needed('$x^3$', out, n)
    if (out(1:n) /= '$x^3$') then
        print *, 'FAIL: $x^3$ should remain unchanged'
        stop 1
    end if

    call prepare_mathtext_if_needed('hello', out, n)
    if (out(1:n) /= 'hello') then
        print *, 'FAIL: plain text should remain unchanged'
        stop 1
    end if

    print *, 'PASS: prepare_mathtext_if_needed wraps as expected'
end program test_text_helpers_mathtext_wrap

