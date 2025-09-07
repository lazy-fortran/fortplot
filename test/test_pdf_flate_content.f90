program test_pdf_flate_content
    !! Verify that PDF content streams are Flate-compressed (/Filter /FlateDecode)
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    character(len=*), parameter :: fn = 'test/output/test_pdf_flate_content.pdf'
    integer :: unit, ios
    character(len=8192) :: buf
    logical :: found
    character(len=8) :: env
    integer :: slen, s
    character(len=16) :: runner
    integer :: rlen, rs

    call figure()
    call plot([0.0_wp,1.0_wp],[0.0_wp,1.0_wp])
    call savefig(fn)

    found = .false.
    open(newunit=unit, file=fn, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ', fn
        stop 1
    end if
    do
        read(unit,'(A)', iostat=ios) buf
        if (ios /= 0) exit
        if (index(buf, '/Filter /FlateDecode') > 0) then
            found = .true.
            exit
        end if
    end do
    close(unit)

    if (.not. found) then
        ! If compression disabled explicitly for local test runs, treat as pass
        call get_environment_variable('FORTPLOT_PDF_COMPRESS', env, length=slen, status=s)
        if (s == 0 .and. slen > 0) then
            if (env(1:1) == '0') then
                print *, 'INFO: compression disabled via FORTPLOT_PDF_COMPRESS=0; skipping strict check'
                stop 0
            end if
        end if
        ! In CI on Windows, PDF writer disables compression to keep raw-PDF tests stable.
        call get_environment_variable('RUNNER_OS', runner, length=rlen, status=rs)
        if (rs == 0 .and. rlen >= 7) then
            if (runner(1:7) == 'Windows') then
                print *, 'INFO: Windows runner uses uncompressed streams; skipping strict Flate check'
                stop 0
            end if
        end if
        print *, 'FAIL: /Filter /FlateDecode not found in content stream'
        stop 2
    end if
    print *, 'PASS: PDF content stream is Flate-compressed'
end program test_pdf_flate_content
