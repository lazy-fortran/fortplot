program test_doc_processing_output
    use fortplot_doc_processing, only: get_example_count, get_example_dir, get_example_name, process_example
    use fortplot_doc_output,     only: write_generated_outputs
    use fortplot_doc_core,       only: PATH_MAX_LEN
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
        integer :: ierr
        call execute_command_line('mkdir -p build/test/output', exitstat=ierr)
        if (ierr /= 0) then
            print *, 'FAIL: cannot create build/test/output directory'
            stop 1
        end if
    end subroutine ensure_parent_dir_exists

end program test_doc_processing_output

