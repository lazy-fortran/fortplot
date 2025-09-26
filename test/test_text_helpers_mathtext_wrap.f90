program test_text_helpers_mathtext_wrap
    use fortplot_text_helpers, only: prepare_mathtext_if_needed
    implicit none

    character(len=256) :: out
    character(len=4)   :: small
    integer :: n

    call prepare_mathtext_if_needed('x^3', out, n)
    if (out(1:n) /= 'x^3') then
        print *, 'FAIL: x^3 should remain literal without $ delimiters'
        stop 1
    end if

    call prepare_mathtext_if_needed('x_1', out, n)
    if (out(1:n) /= 'x_1') then
        print *, 'FAIL: x_1 should remain literal without $ delimiters'
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
        print *, 'FAIL: presence of literal $ should leave text unchanged'
        stop 1
    end if

    call prepare_mathtext_if_needed('', out, n)
    if (n /= 0) then
        print *, 'FAIL: empty input should yield zero length'
        stop 1
    end if

    ! Small-buffer behavior: ensure content truncates safely when buffer is too small
    small = ''
    call prepare_mathtext_if_needed('x^1234', small, n)
    if (n /= len(small)) then
        print *, 'FAIL: small buffer out_len should match buffer length'
        stop 1
    end if
    if (small(1:n) /= 'x^12') then
        print *, 'FAIL: small buffer should contain truncated literal input'
        stop 1
    end if

    print *, 'PASS: prepare_mathtext_if_needed preserves literal math markers as expected'
end program test_text_helpers_mathtext_wrap
