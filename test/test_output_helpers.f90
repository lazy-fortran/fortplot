module test_output_helpers
    !! Utilities for directing test artifact output into structured directories

    use fortplot_system_runtime, only: create_directory_runtime
    use, intrinsic :: iso_fortran_env, only: error_unit

    implicit none
    private

    character(len=*), parameter :: base_output_dir = 'test/output/'

    public :: ensure_test_output_dir

contains

    subroutine ensure_test_output_dir(subdir, dir)
        !! Ensure a test output directory exists and return its normalized path
        character(len=*), intent(in) :: subdir
        character(len=:), allocatable, intent(out) :: dir
        character(len=:), allocatable :: target
        logical :: ok
        integer :: last

        target = base_output_dir
        if (len_trim(subdir) > 0) then
            target = target // trim(subdir)
        end if

        last = len_trim(target)
        if (last == 0) then
            write(error_unit, '(A)') 'ERROR: base output directory path is empty'
            error stop 1
        end if

        if (target(last:last) /= '/') then
            target = target // '/'
        end if

        call create_directory_runtime(target, ok)
        if (.not. ok) then
            write(error_unit, '(A)') &
                'ERROR: failed to create test output directory: ' // trim(target)
            error stop 1
        end if

        dir = target
    end subroutine ensure_test_output_dir

end module test_output_helpers
