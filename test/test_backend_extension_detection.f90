program test_backend_extension_detection
    !! Verify get_backend_from_filename maps common extensions correctly

    use fortplot_utils, only: get_backend_from_filename
    implicit none

    logical :: passed

    passed = .true.

    call expect_backend('output.txt', 'ascii')
    call expect_backend('output.dat', 'ascii')
    call expect_backend('OUTPUT.DAT', 'ascii')
    call expect_backend('plot.png', 'png')
    call expect_backend('plot.pdf', 'pdf')
    call expect_backend('plot', 'png')
    call expect_backend('plot.unknown', 'png')

    if (.not. passed) then
        error stop 'backend extension detection regression'
    else
        print *, 'Backend extension detection tests passed'
    end if

contains

    subroutine expect_backend(filename, expected)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: expected
        character(len=20) :: backend

        backend = get_backend_from_filename(filename)
        if (trim(backend) /= trim(expected)) then
            print *, 'FAIL:', trim(filename), 'expected', trim(expected), 'got', trim(backend)
            passed = .false.
        end if
    end subroutine expect_backend

end program test_backend_extension_detection
