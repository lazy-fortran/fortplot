program test_long_path_directory_creation
    use fortplot_utils, only: ensure_directory_exists
    use fortplot_file_operations, only: create_directory_runtime
    implicit none

    character(len=:), allocatable :: base_dir
    character(len=:), allocatable :: segment
    character(len=:), allocatable :: long_dir
    character(len=:), allocatable :: file_path
    integer :: i, num_segments
    logical :: exists, success

    base_dir = 'build/test/output/long_path_dir'
    segment = 'segblock'
    num_segments = 80

    long_dir = base_dir
    do i = 1, num_segments
        long_dir = trim(long_dir) // '/' // segment
    end do
    long_dir = trim(long_dir) // '/final_destination'

    file_path = trim(long_dir) // '/plot.png'

    if (len_trim(long_dir) <= 512) then
        print *, 'TEST SETUP ERROR: long_dir length', len_trim(long_dir), 'must exceed 512'
        stop 1
    end if

    call create_directory_runtime(trim(long_dir), success)
    if (.not. success) then
        print *, 'FAIL: create_directory_runtime rejected long directory', &
                 ' of length', len_trim(long_dir)
        stop 1
    end if

    call ensure_directory_exists(file_path)

    inquire(file=trim(long_dir)//'/.' , exist=exists)
    if (.not. exists) then
        print *, 'FAIL: ensure_directory_exists did not create long directory', &
                 ' of length', len_trim(long_dir)
        stop 1
    else
        print *, 'PASS: ensure_directory_exists created long directory', &
                 ' of length', len_trim(long_dir)
    end if

end program test_long_path_directory_creation
