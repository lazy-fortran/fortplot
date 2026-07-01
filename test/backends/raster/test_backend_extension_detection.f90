program test_backend_extension_detection
    !! Verify backend naming: file-extension resolution, the text/ascii
    !! alias normalization, and that both names initialize the text backend.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_utils, only: get_backend_from_filename, normalize_backend_name
    use fortplot, only: figure_t
    use fortplot_system_runtime, only: create_directory_runtime, delete_file_runtime
    implicit none

    logical :: passed

    passed = .true.

    ! .txt/.dat and the "terminal" sink resolve to the text backend name.
    call expect_backend('output.txt', 'text')
    call expect_backend('output.dat', 'text')
    call expect_backend('OUTPUT.DAT', 'text')
    call expect_backend('terminal', 'text')
    call expect_backend('plot.png', 'png')
    call expect_backend('plot.pdf', 'pdf')
    call expect_backend('plot.svg', 'svg')
    call expect_backend('output.SVG', 'svg')
    call expect_backend('plot', 'png')
    call expect_backend('plot.unknown', 'png')

    ! 'text' is the preferred name; 'ascii' is a compatibility alias. Both
    ! normalize to the internal 'ascii' charset canvas.
    call expect_normalized('text', 'ascii')
    call expect_normalized('TEXT', 'ascii')
    call expect_normalized('ascii', 'ascii')
    call expect_normalized('ASCII', 'ascii')
    call expect_normalized('png', 'png')
    call expect_normalized('pdf', 'pdf')
    ! Only 'text' aliases to the ASCII charset; unsupported names pass through
    ! unchanged so existing invalid-backend handling still rejects them.
    call expect_normalized('terminal', 'terminal')

    call expect_text_backend_init('text')
    call expect_text_backend_init('ascii')

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
            print *, 'FAIL:', trim(filename), 'expected', trim(expected), &
                'got', trim(backend)
            passed = .false.
        end if
    end subroutine expect_backend

    subroutine expect_normalized(name, expected)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: expected
        character(len=20) :: canonical

        canonical = normalize_backend_name(name)
        if (trim(canonical) /= trim(expected)) then
            print *, 'FAIL normalize:', trim(name), 'expected', trim(expected), &
                'got', trim(canonical)
            passed = .false.
        end if
    end subroutine expect_normalized

    subroutine expect_text_backend_init(backend_name)
        !! Initializing with 'text' or 'ascii' selects the same text backend
        !! and writes a non-empty .txt plot.
        character(len=*), intent(in) :: backend_name
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        integer :: i, file_size
        logical :: dir_ok, file_exists, deleted
        character(len=256) :: outfile

        outfile = 'build/test/output/test_text_backend_'//trim(backend_name)//'.txt'
        call create_directory_runtime('build/test/output', dir_ok)

        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i, wp)
        end do

        call fig%initialize(80, 24, backend=backend_name)
        if (trim(fig%state%backend_name) /= 'ascii') then
            print *, 'FAIL init: backend=', trim(backend_name), &
                ' canonical=', trim(fig%state%backend_name)
            passed = .false.
        end if

        call fig%add_plot(x, y, label='line')
        call fig%savefig(trim(outfile))

        inquire (file=trim(outfile), exist=file_exists, size=file_size)
        if (.not. file_exists) then
            print *, 'FAIL init: no .txt file for backend=', trim(backend_name)
            passed = .false.
        else if (file_size <= 0) then
            print *, 'FAIL init: empty .txt file for backend=', trim(backend_name)
            passed = .false.
        end if

        call delete_file_runtime(trim(outfile), deleted)
    end subroutine expect_text_backend_init

end program test_backend_extension_detection
