program test_doc_processing_output
    use fortplot_documentation, only: get_example_count, get_example_dir, get_example_name, process_example, &
                                       write_generated_outputs, PATH_MAX_LEN
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

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
    name = 'basic_plots'
    dir  = 'example/fortran/basic_plots'
    out_file = 'build/test/output/test_doc_generated_basic_plots.md'

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
    call assert_file_contains(out_file, '../../media/examples/basic_plots/simple_plot.png')
    call assert_file_contains(out_file, '[Download PDF](../../media/examples/basic_plots/simple_plot.pdf)')
    call assert_file_contains(out_file, 'ASCII output:')
    call assert_file_contains(out_file, 'simple ascii content')

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
        
        call create_directory_runtime('output/example/fortran/basic_plots', ok)
        if (.not. ok) then
            print *, 'FAIL: cannot create output/example/fortran/basic_plots'
            stop 1
        end if
        
        ! Create minimal ASCII file used in the generated output
        open(newunit=u, file='output/example/fortran/basic_plots/simple_plot.txt', &
             status='unknown', action='write', iostat=ios)
        if (ios == 0) then
            write(u, '(A)') 'simple ascii content'
            close(u)
        else
            print *, 'FAIL: cannot create simple_plot.txt'
            stop 1
        end if
        
        ! Touch PNG and PDF so add_if_exists picks them up
        call touch_empty_file('output/example/fortran/basic_plots/simple_plot.png')
        call touch_empty_file('output/example/fortran/basic_plots/simple_plot.pdf')
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
