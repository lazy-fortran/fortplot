module test_utilities_secure
    !! Secure test utilities module
    !! Provides safe alternatives to execute_command_line for test cleanup
    use fortplot_system_secure, only: delete_file_secure
    implicit none
    private

    public :: cleanup_test_file, cleanup_test_files

contains

    subroutine cleanup_test_file(filename)
        !! Securely delete a test file
        character(len=*), intent(in) :: filename
        logical :: success
        
        success = delete_file_secure(filename)
        if (.not. success) then
            ! In tests, we don't want to fail on cleanup issues
            ! Just print a warning
            print *, "Warning: Could not clean up test file: ", trim(filename)
        end if
    end subroutine cleanup_test_file

    subroutine cleanup_test_files(filenames)
        !! Securely delete multiple test files
        character(len=*), intent(in) :: filenames(:)
        integer :: i
        
        do i = 1, size(filenames)
            call cleanup_test_file(filenames(i))
        end do
    end subroutine cleanup_test_files

end module test_utilities_secure