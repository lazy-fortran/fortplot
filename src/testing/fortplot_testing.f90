module fortplot_testing
    !! Testing utilities module with proper error handling
    use fortplot_errors, only: fortplot_error_t, SUCCESS, ERROR_INVALID_INPUT
    implicit none
    
    private
    public :: assert_file_exists, assert_equals, assert_true
    public :: test_result_t
    
    !> Test result type for better test management
    type :: test_result_t
        logical :: passed = .true.
        character(len=256) :: message = ""
        integer :: error_count = 0
    contains
        procedure :: fail
        procedure :: get_status
    end type test_result_t
    
contains

    subroutine assert_file_exists(filename, test_result)
        character(len=*), intent(in) :: filename
        type(test_result_t), intent(inout), optional :: test_result
        logical :: exists
        
        inquire(file=filename, exist=exists)
        if (.not. exists) then
            if (present(test_result)) then
                call test_result%fail("File '" // trim(filename) // "' does not exist")
            else
                write(*, '(A,A,A)') "ASSERTION FAILED: File '", trim(filename), "' does not exist"
                ! Legacy behavior: continue with error output instead of stop
            end if
        end if
    end subroutine assert_file_exists
    
    subroutine assert_equals(actual, expected, tolerance, message, test_result)
        use iso_fortran_env, only: wp => real64
        real(wp), intent(in) :: actual, expected
        real(wp), intent(in), optional :: tolerance
        character(len=*), intent(in), optional :: message
        type(test_result_t), intent(inout), optional :: test_result
        real(wp) :: tol
        character(len=100) :: msg
        character(len=256) :: error_msg
        
        tol = 1e-6_wp
        if (present(tolerance)) tol = tolerance
        
        msg = "Values not equal"
        if (present(message)) msg = message
        
        if (abs(actual - expected) > tol) then
            write(error_msg, '(A," - Expected: ",G0,", Actual: ",G0)') trim(msg), expected, actual
            if (present(test_result)) then
                call test_result%fail(error_msg)
            else
                write(*, '(A,A)') "ASSERTION FAILED: ", trim(error_msg)
                ! Legacy behavior: continue with error output instead of stop
            end if
        end if
    end subroutine assert_equals
    
    subroutine assert_true(condition, message, test_result)
        logical, intent(in) :: condition
        character(len=*), intent(in), optional :: message
        type(test_result_t), intent(inout), optional :: test_result
        character(len=100) :: msg
        
        msg = "Condition is false"
        if (present(message)) msg = message
        
        if (.not. condition) then
            if (present(test_result)) then
                call test_result%fail(msg)
            else
                write(*, '(A,A)') "ASSERTION FAILED: ", trim(msg)
                ! Legacy behavior: continue with error output instead of stop
            end if
        end if
    end subroutine assert_true
    
    !> Mark test as failed with error message
    subroutine fail(this, message)
        class(test_result_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        
        this%passed = .false.
        this%error_count = this%error_count + 1
        if (len_trim(this%message) == 0) then
            this%message = trim(message)
        else
            this%message = trim(this%message) // "; " // trim(message)
        end if
    end subroutine fail
    
    !> Get test status as integer code
    function get_status(this) result(status)
        class(test_result_t), intent(in) :: this
        integer :: status
        
        if (this%passed) then
            status = SUCCESS
        else
            status = ERROR_INVALID_INPUT  ! Generic error for test failures
        end if
    end function get_status

end module fortplot_testing