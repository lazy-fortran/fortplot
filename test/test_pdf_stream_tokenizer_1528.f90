program test_pdf_stream_tokenizer_1528
    use test_pdf_utils, only: pdf_next_token
    implicit none

    character(len=1), parameter :: bslash = achar(92)

    character(len=*), parameter :: hex_simple = '<48 65 6c6c6f>Tj'
    character(len=*), parameter :: tok_hex_simple = '<48 65 6c6c6f>'

    character(len=*), parameter :: hex_ws = '<48' // achar(10) // '65' // &
                                           achar(9) // '6c6c6f>Tj'
    character(len=*), parameter :: tok_hex_ws = '<48' // achar(10) // '65' // &
                                               achar(9) // '6c6c6f>'

    character(len=*), parameter :: lit_lf = '(a' // bslash // achar(10) // ')Tj'
    character(len=*), parameter :: tok_lit_lf = '(a' // bslash // achar(10) // &
                                                ')'

    character(len=*), parameter :: lit_crlf = '(a' // bslash // achar(13) // &
                                              achar(10) // ')Tj'
    character(len=*), parameter :: tok_lit_crlf = '(a' // bslash // achar(13) // &
                                                  achar(10) // ')'

    character(len=*), parameter :: lit_escapes = '(a' // bslash // '053' // &
                                                 bslash // '050b' // bslash // &
                                                 '051c' // bslash // 'n' // &
                                                 bslash // 'r' // bslash // 't' // &
                                                 bslash // 'b' // bslash // 'f)Tj'
    character(len=*), parameter :: tok_lit_escapes = '(a' // bslash // '053' // &
                                                     bslash // '050b' // &
                                                     bslash // '051c' // bslash // &
                                                     'n' // bslash // 'r' // &
                                                     bslash // 't' // bslash // &
                                                     'b' // bslash // 'f)'

    call assert_tokens2(hex_simple, tok_hex_simple, len(tok_hex_simple), 'Tj', 2)

    call assert_tokens2(hex_ws, tok_hex_ws, len(tok_hex_ws), 'Tj', 2)

    call assert_tokens2(lit_lf, tok_lit_lf, len(tok_lit_lf), 'Tj', 2)

    call assert_tokens2(lit_crlf, tok_lit_crlf, len(tok_lit_crlf), 'Tj', 2)

    call assert_tokens2(lit_escapes, tok_lit_escapes, len(tok_lit_escapes), 'Tj', 2)

    print *, 'PASS: PDF stream tokenizer handles PDF string edge cases'
contains
    subroutine assert_tokens2(text, exp1, exp1_len, exp2, exp2_len)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: exp1, exp2
        integer, intent(in) :: exp1_len, exp2_len

        character(len=512) :: token
        integer :: token_len
        logical :: has_token
        integer :: pos

        pos = 1
        call assert_next_token(text, pos, exp1, exp1_len)
        call assert_next_token(text, pos, exp2, exp2_len)

        call pdf_next_token(text, pos, token, token_len, has_token)
        if (has_token) then
            print *, 'FAIL: unexpected extra token'
            print *, 'FAIL: got=', token(1:min(token_len, len(token)))
            stop 1
        end if
    end subroutine assert_tokens2

    subroutine assert_next_token(text, pos, expected, expected_len)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        character(len=*), intent(in) :: expected
        integer, intent(in) :: expected_len

        character(len=512) :: token
        integer :: token_len
        logical :: has_token

        call pdf_next_token(text, pos, token, token_len, has_token)
        if (.not. has_token) then
            print *, 'FAIL: missing token'
            print *, 'FAIL: input=', text
            stop 1
        end if

        if (token_len /= expected_len) then
            print *, 'FAIL: token length mismatch'
            print *, 'FAIL: got=', token(1:min(token_len, len(token)))
            print *, 'FAIL: exp=', expected(1:expected_len)
            stop 1
        end if

        if (token(1:token_len) /= expected(1:expected_len)) then
            print *, 'FAIL: token mismatch'
            print *, 'FAIL: got=', token(1:token_len)
            print *, 'FAIL: exp=', expected(1:expected_len)
            stop 1
        end if
    end subroutine assert_next_token
end program test_pdf_stream_tokenizer_1528
