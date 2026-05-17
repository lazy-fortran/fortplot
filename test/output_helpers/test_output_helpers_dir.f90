program test_output_helpers_dir
    use, intrinsic :: iso_fortran_env, only: error_unit
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    character(len=:), allocatable :: output_dir
    character(len=:), allocatable :: sentinel_path
    integer :: io_stat
    integer :: unit

    call ensure_test_output_dir('output_helpers_dir', output_dir)

    if (.not. (index(output_dir, 'build/test/output') == 1 .or. &
               index(output_dir, 'build\\test\\output') == 1)) then
        write (error_unit, '(A)') &
            'ERROR: ensure_test_output_dir did not prefer build/test/output/'
        write (error_unit, '(A)') 'Path: '//trim(output_dir)
        error stop 1
    end if

    sentinel_path = output_dir//'fortplot_test_sentinel.tmp'
    open (newunit=unit, file=sentinel_path, status='replace', action='write', &
          iostat=io_stat)
    if (io_stat /= 0) then
        write (error_unit, '(A,I0)') &
            'ERROR: failed to create sentinel file in output directory, iostat: ', &
            io_stat
        write (error_unit, '(A)') 'Path: '//trim(sentinel_path)
        error stop 1
    end if

    write (unit, '(A)') 'sentinel'
    close (unit, status='delete', iostat=io_stat)
    if (io_stat /= 0) then
        write (error_unit, '(A,I0)') &
            'ERROR: failed to remove sentinel file, iostat: ', io_stat
        write (error_unit, '(A)') 'Path: '//trim(sentinel_path)
        error stop 1
    end if

    print *, 'PASS: test_output_helpers_dir'
end program test_output_helpers_dir
