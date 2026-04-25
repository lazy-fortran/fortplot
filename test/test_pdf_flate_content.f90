program test_pdf_flate_content
    !! Verify that PDF content streams are Flate-compressed (/Filter /FlateDecode)
    !! The compression uses a pure-Fortran zlib implementation, so this must work
    !! on all platforms including Windows.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none
    character(len=*), parameter :: fn = 'build/test/output/test_pdf_flate_content.pdf'
    integer :: unit, ios
    character(len=8192) :: buf
    logical :: found, dir_ok

    call create_directory_runtime('build/test/output', dir_ok)

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
        print *, 'FAIL: /Filter /FlateDecode not found in content stream'
        print *, '      PDF compression must work on all platforms.'
        stop 2
    end if
    print *, 'PASS: PDF content stream is Flate-compressed'
end program test_pdf_flate_content
