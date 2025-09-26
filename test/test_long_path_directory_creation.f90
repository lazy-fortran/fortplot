program test_long_path_directory_creation
    use fortplot_utils, only: ensure_directory_exists
    use fortplot_file_operations, only: create_directory_runtime
    implicit none

    character(len=:), allocatable :: base_dir
    character(len=:), allocatable :: segment
    character(len=:), allocatable :: long_dir_runtime
    character(len=:), allocatable :: ensure_dir
    character(len=:), allocatable :: file_path
    character(len=64) :: suffix_buffer
    integer :: i, num_segments, clock_count
    logical :: exists, success

    segment = 'segblock'
    num_segments = 80

    call system_clock(count=clock_count)
    write(suffix_buffer, '(I0)') clock_count
    base_dir = 'build/test/output/long_path_dir_' // trim(suffix_buffer)

    long_dir_runtime = base_dir
    do i = 1, num_segments
        long_dir_runtime = trim(long_dir_runtime) // '/' // segment
    end do
    long_dir_runtime = trim(long_dir_runtime) // '/runtime_destination'

    if (len_trim(long_dir_runtime) <= 512) then
        print *, 'TEST SETUP ERROR: long_dir_runtime length', &
                 len_trim(long_dir_runtime), 'must exceed 512'
        stop 1
    end if

    call create_directory_runtime(trim(long_dir_runtime), success)
    if (.not. success) then
        print *, 'FAIL: create_directory_runtime rejected long directory', &
                 ' of length', len_trim(long_dir_runtime)
        stop 1
    end if

    ensure_dir = trim(long_dir_runtime) // '/ensure_target'
    file_path = trim(ensure_dir) // '/plot.png'

    if (len_trim(ensure_dir) <= 512) then
        print *, 'TEST SETUP ERROR: ensure_dir length', &
                 len_trim(ensure_dir), 'must exceed 512'
        stop 1
    end if

    inquire(file=trim(ensure_dir)//'/.', exist=exists)
    if (exists) then
        print *, 'TEST SETUP ERROR: ensure_dir unexpectedly exists before test'
        stop 1
    end if

    call ensure_directory_exists(file_path)

    inquire(file=trim(ensure_dir)//'/.', exist=exists)
    if (.not. exists) then
        print *, 'FAIL: ensure_directory_exists did not create long directory', &
                 ' of length', len_trim(ensure_dir)
        stop 1
    else
        print *, 'PASS: ensure_directory_exists created long directory', &
                 ' of length', len_trim(ensure_dir)
    end if

end program test_long_path_directory_creation
