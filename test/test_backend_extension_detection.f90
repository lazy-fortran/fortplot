program test_backend_extension_detection
    !! Verify get_backend_from_filename maps common extensions correctly

    use fortplot_utils, only: get_backend_from_filename
    implicit none

    logical :: passed
    character(len=20) :: backend

    passed = .true.

    backend = get_backend_from_filename('output.txt')
    if (trim(backend) /= 'ascii') then
        print *, 'FAIL: expected ascii for .txt, got', trim(backend)
        passed = .false.
    end if

    backend = get_backend_from_filename('output.dat')
    if (trim(backend) /= 'ascii') then
        print *, 'FAIL: expected ascii for .dat, got', trim(backend)
        passed = .false.
    end if

    backend = get_backend_from_filename('OUTPUT.DAT')
    if (trim(backend) /= 'ascii') then
        print *, 'FAIL: expected ascii for uppercase .DAT, got', trim(backend)
        passed = .false.
    end if

    backend = get_backend_from_filename('plot.png')
    if (trim(backend) /= 'png') then
        print *, 'FAIL: expected png for .png, got', trim(backend)
        passed = .false.
    end if

    backend = get_backend_from_filename('plot.pdf')
    if (trim(backend) /= 'pdf') then
        print *, 'FAIL: expected pdf for .pdf, got', trim(backend)
        passed = .false.
    end if

    if (.not. passed) then
        stop 1
    else
        print *, 'Backend extension detection tests passed'
    end if

end program test_backend_extension_detection
