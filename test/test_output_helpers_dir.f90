program test_output_helpers_dir
    use, intrinsic :: iso_fortran_env, only: error_unit
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    character(len=:), allocatable :: output_dir
    logical :: exists

    call ensure_test_output_dir('output_helpers_dir', output_dir)

    if (.not. (index(output_dir, 'build/test/output') == 1 .or. &
               index(output_dir, 'build\\test\\output') == 1)) then
        write (error_unit, '(A)') &
            'ERROR: ensure_test_output_dir did not prefer build/test/output/'
        write (error_unit, '(A)') 'Path: '//trim(output_dir)
        error stop 1
    end if

    inquire (file=trim(output_dir)//'.', exist=exists)
    if (.not. exists) then
        write (error_unit, '(A)') 'ERROR: output directory does not exist'
        write (error_unit, '(A)') 'Path: '//trim(output_dir)
        error stop 1
    end if

    print *, 'PASS: test_output_helpers_dir'
end program test_output_helpers_dir
