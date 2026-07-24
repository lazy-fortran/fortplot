program test_doc_processing_output
    use fortplot_documentation, only: get_example_count, get_example_dir, get_example_name, &
                                       write_generated_outputs, PATH_MAX_LEN
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    !! A synthetic example name, not a real one. Pointing the media stubs at the
    !! real 'basic_plots' example overwrote that example's simple_plot.txt with
    !! fixture text and truncated its PNG and PDF to zero bytes, and `make doc`
    !! then published the corrupted media. write_generated_outputs derives the
    !! scanned directory purely from the example name, so an invented name
    !! exercises the same code path against a private scratch directory.
    character(len=*), parameter :: PROBE = 'doc_output_probe'
    character(len=*), parameter :: PROBE_OUT = 'output/example/fortran/' // PROBE

    integer :: n, unit_out, ios
    character(len=PATH_MAX_LEN) :: dir, name, out_file

    ! Basic processing metadata sanity
    n = get_example_count()
    call assert_true('example_count_positive', n > 0)

    call get_example_dir(1, dir)
    call get_example_name(1, name)
    call assert_true('example_dir_nonempty', len_trim(dir) > 0)
    call assert_true('example_name_nonempty', len_trim(name) > 0)

    ! Generate output markdown for an example with known outputs
    name = PROBE
    dir  = 'example/fortran/' // PROBE
    out_file = 'build/test/output/test_doc_generated_outputs.md'

    call ensure_parent_dir_exists()
    call ensure_media_stub_files()

    open(newunit=unit_out, file=trim(out_file), status='replace', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot create output file: ', trim(out_file)
        stop 1
    end if

    call write_generated_outputs(unit_out, dir, name)
    close(unit_out)

    call assert_file_contains(out_file, '### Simple Plot')
    call assert_file_contains(out_file, '../../media/examples/' // PROBE // '/simple_plot.png')
    call assert_file_contains(out_file, &
        '[Download PDF](../../media/examples/' // PROBE // '/simple_plot.pdf)')
    ! The text rendering is folded so the image stays above the fold, but it must
    ! still be present and HTML-escaped rather than dropped or fenced.
    call assert_file_contains(out_file, '<details>')
    call assert_file_contains(out_file, '<summary>Text backend output</summary>')
    call assert_file_contains(out_file, '<pre><code>')
    call assert_file_contains(out_file, 'simple &lt;ascii&gt; &amp; content')
    call assert_file_contains(out_file, '</code></pre>')
    call assert_file_contains(out_file, '</details>')
    call assert_file_contains(out_file, &
        '[Download text](../../media/examples/' // PROBE // '/simple_plot.txt)')
    call assert_file_not_contains(out_file, '```')

    print *, 'Doc processing/output tests passed'

contains

    subroutine assert_true(name, cond)
        character(len=*), intent(in) :: name
        logical, intent(in) :: cond
        if (.not. cond) then
            print *, 'FAIL:', trim(name)
            stop 1
        end if
    end subroutine assert_true

    subroutine assert_file_contains(path, needle)
        character(len=*), intent(in) :: path, needle
        character(len=1024) :: line
        integer :: u, ios
        logical :: found
        found = .false.
        open(newunit=u, file=trim(path), status='old', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open file:', trim(path)
            stop 1
        end if
        do
            read(u, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, trim(needle)) > 0) then
                found = .true.
                exit
            end if
        end do
        close(u)
        if (.not. found) then
            print *, 'FAIL: file missing expected content:'
            print *, '  file  :', trim(path)
            print *, '  needle:', trim(needle)
            stop 1
        end if
    end subroutine assert_file_contains

    subroutine assert_file_not_contains(path, needle)
        character(len=*), intent(in) :: path, needle
        character(len=1024) :: line
        integer :: u, ios
        logical :: found
        found = .false.
        open(newunit=u, file=trim(path), status='old', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open file:', trim(path)
            stop 1
        end if
        do
            read(u, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, trim(needle)) > 0) then
                found = .true.
                exit
            end if
        end do
        close(u)
        if (found) then
            print *, 'FAIL: file contains unexpected content:'
            print *, '  file  :', trim(path)
            print *, '  needle:', trim(needle)
            stop 1
        end if
    end subroutine assert_file_not_contains

    subroutine ensure_parent_dir_exists()
        logical :: ok
        call create_directory_runtime('build/test/output', ok)
        if (.not. ok) then
            print *, 'FAIL: cannot create build/test/output directory'
            stop 1
        end if
    end subroutine ensure_parent_dir_exists

    subroutine ensure_media_stub_files()
        !! Ensure required example media files exist so scanning is deterministic
        logical :: ok
        integer :: u, ios
        
        call create_directory_runtime(PROBE_OUT, ok)
        if (.not. ok) then
            print *, 'FAIL: cannot create ', PROBE_OUT
            stop 1
        end if

        ! Create minimal ASCII file used in the generated output
        open(newunit=u, file=PROBE_OUT // '/simple_plot.txt', &
             status='unknown', action='write', iostat=ios)
        if (ios == 0) then
            write(u, '(A)') 'simple <ascii> & content'
            close(u)
        else
            print *, 'FAIL: cannot create simple_plot.txt'
            stop 1
        end if
        
        ! Touch PNG and PDF so add_if_exists picks them up
        call touch_empty_file(PROBE_OUT // '/simple_plot.png')
        call touch_empty_file(PROBE_OUT // '/simple_plot.pdf')
    end subroutine ensure_media_stub_files

    subroutine touch_empty_file(path)
        character(len=*), intent(in) :: path
        integer :: u, ios
        open(newunit=u, file=trim(path), status='unknown', action='write', iostat=ios)
        if (ios == 0) then
            close(u)
        else
            print *, 'FAIL: cannot create file: ', trim(path)
            stop 1
        end if
    end subroutine touch_empty_file

end program test_doc_processing_output
