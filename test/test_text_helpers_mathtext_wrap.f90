program test_text_helpers_mathtext_wrap
    use fortplot_text_helpers, only: prepare_mathtext_if_needed
    implicit none

    character(len=256) :: out
    character(len=6)   :: small
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

    call prepare_mathtext_if_needed('price is $5 and cost^2', out, n)
    if (out(1:n) /= 'price is $5 and cost^2') then
        print *, 'FAIL: presence of $ should prevent auto-wrap'
        stop 1
    end if

    call prepare_mathtext_if_needed('', out, n)
    if (n /= 0) then
        print *, 'FAIL: empty input should yield zero length'
        stop 1
    end if

    ! Small-buffer truncation behavior: input requires 8 chars ('$x^1234$'),
    ! but buffer is only 6; ensure safe wrap with truncation and correct length
    small = ''
    call prepare_mathtext_if_needed('x^1234', small, n)
    if (n /= len(small)) then
        print *, 'FAIL: small buffer out_len mismatch'
        stop 1
    end if
    if (small(1:1) /= '$' .or. small(n:n) /= '$') then
        print *, 'FAIL: small buffer should still be wrapped with $ delimiters'
        stop 1
    end if

    print *, 'PASS: prepare_mathtext_if_needed wraps as expected'
end program test_text_helpers_mathtext_wrap
