program test_animation_directory_creation
    use fortplot_utils, only: ensure_directory_exists
    use fortplot_file_operations, only: check_directory_exists
    implicit none

    !! Scratch path, not the real animation example's output directory: test
    !! artifacts belong under build/test/output/, and output/example/fortran/ is
    !! copied wholesale into the published documentation media.
    character(len=*), parameter :: out_dir  = 'build/test/output/anim_dir_probe'
    character(len=*), parameter :: out_file = out_dir // '/animation.mp4'
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

