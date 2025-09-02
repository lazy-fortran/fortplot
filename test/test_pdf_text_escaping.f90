! Test: PDF text escaping for parentheses in mixed-font rendering
program test_pdf_text_escaping
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, ylabel, savefig
    implicit none

    character(len=*), parameter :: out_pdf = 'build/test/output/pdf_label_parens.pdf'
    integer(kind=8) :: fsize
    character, allocatable :: data(:)
    integer :: unit, ios, i
    logical :: found_open, found_close

    call figure(figsize=[6.4_wp, 4.8_wp])
    call ylabel('sin(x)')
    call savefig(out_pdf)

    found_open = .false.
    found_close = .false.

    open(newunit=unit, file=out_pdf, access='stream', form='unformatted', status='old', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ', trim(out_pdf)
        stop 1
    end if

    inquire(unit=unit, size=fsize)
    if (fsize <= 0) then
        print *, 'FAIL: zero-size PDF'
        close(unit)
        stop 1
    end if

    allocate(character(len=1) :: data(fsize))
    read(unit, iostat=ios) data
    close(unit)
    if (ios /= 0) then
        print *, 'FAIL: cannot read PDF data'
        stop 1
    end if

    ! Scan contiguous data for patterns
    do i = 1, fsize-4
        if (.not. found_open) then
            if (data(i) == '(' .and. data(i+1) == '\' .and. data(i+2) == '(' .and. data(i+3) == ')' .and. data(i+4) == ' ') then
                found_open = .true.
            end if
        end if
        if (.not. found_close) then
            if (data(i) == '(' .and. data(i+1) == '\' .and. data(i+2) == ')' .and. data(i+3) == ')' .and. data(i+4) == ' ') then
                found_close = .true.
            end if
        end if
        if (found_open .and. found_close) exit
    end do

    if (.not. found_open) then
        print *, 'FAIL: missing escaped open parenthesis sequence (\() Tj in PDF stream'
        stop 1
    end if
    if (.not. found_close) then
        print *, 'FAIL: missing escaped close parenthesis sequence (\)) Tj in PDF stream'
        stop 1
    end if

    print *, 'PASS: PDF parentheses escaped in mixed-font text'
end program test_pdf_text_escaping
