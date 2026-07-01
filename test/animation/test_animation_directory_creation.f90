program test_animation_directory_creation
    use fortplot_utils, only: ensure_directory_exists
    use fortplot_file_operations, only: check_directory_exists
    implicit none

    character(len=*), parameter :: out_file = 'output/example/fortran/animation/animation.mp4'
    character(len=*), parameter :: out_dir  = 'output/example/fortran/animation'
    logical :: exists

    print *, 'TEST: Animation output directory creation'

    call ensure_directory_exists(out_file)

    call check_directory_exists(out_dir, exists)
    if (.not. exists) then
        print *, 'ERROR: Expected directory not created: ', trim(out_dir)
        stop 1
    end if

    print *, 'PASS: Directory exists for animation outputs: ', trim(out_dir)
end program test_animation_directory_creation

