program test_pdf_stream_tokenizer_1524
    use test_pdf_utils, only: pdf_next_token
    implicit none

    call assert_tokens('Tf(Hello World)Tj', &
                       [character(len=64) :: 'Tf', '(Hello World)', 'Tj'])

    call assert_tokens('0 0 0 RG % comment' // new_line('a') // '1 0 0 RG', &
                       [character(len=64) :: '0', '0', '0', 'RG', '1', '0', '0', &
                        'RG'])

    call assert_tokens('[1 2 3]0 d', [character(len=64) :: '[', '1', '2', '3', &
                                       ']', '0', 'd'])

    call assert_tokens('<< /Length 5 >>', [character(len=64) :: '<<', '/Length', &
                                           '5', '>>'])

    call assert_tokens('<48656c6c6f>(a\)b(c\(d\))e)Tj', &
                       [character(len=64) :: '<48656c6c6f>', '(a\)b(c\(d\))e)', &
                        'Tj'])

    print *, 'PASS: PDF stream tokenizer handles delimiters, strings, comments'
contains
    subroutine assert_tokens(text, expected)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: expected(:)

        character(len=256) :: token
        integer :: token_len
        logical :: has_token
        integer :: pos
        integer :: i
        integer :: exp_len

        pos = 1
        do i = 1, size(expected)
            call pdf_next_token(text, pos, token, token_len, has_token)
            if (.not. has_token) then
                print *, 'FAIL: missing token idx=', i
                print *, 'FAIL: input=', text
                stop 1
            end if

            exp_len = len_trim(expected(i))
            if (token_len /= exp_len) then
                print *, 'FAIL: token length mismatch idx=', i
                print *, 'FAIL: got=', token(1:min(token_len, len(token)))
                print *, 'FAIL: exp=', expected(i)(1:exp_len)
                stop 1
            end if

            if (token(1:token_len) /= expected(i)(1:exp_len)) then
                print *, 'FAIL: token mismatch idx=', i
                print *, 'FAIL: got=', token(1:token_len)
                print *, 'FAIL: exp=', expected(i)(1:exp_len)
                stop 1
            end if
        end do

        call pdf_next_token(text, pos, token, token_len, has_token)
        if (has_token) then
            print *, 'FAIL: unexpected extra token'
            print *, 'FAIL: got=', token(1:min(token_len, len(token)))
            stop 1
        end if
    end subroutine assert_tokens
end program test_pdf_stream_tokenizer_1524
