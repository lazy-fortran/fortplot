module fortplot_testing
    !! Testing utilities module
    implicit none
    
    private
    public :: assert_file_exists, assert_equals, assert_true
    
contains

    subroutine assert_file_exists(filename)
        character(len=*), intent(in) :: filename
        logical :: exists
        
        inquire(file=filename, exist=exists)
        if (.not. exists) then
            write(*, '(A,A,A)') "ASSERTION FAILED: File '", trim(filename), "' does not exist"
            stop 1
        end if
    end subroutine assert_file_exists
    
    subroutine assert_equals(actual, expected, tolerance, message)
        use iso_fortran_env, only: wp => real64
        real(wp), intent(in) :: actual, expected
        real(wp), intent(in), optional :: tolerance
        character(len=*), intent(in), optional :: message
        real(wp) :: tol
        character(len=100) :: msg
        
        tol = 1e-6_wp
        if (present(tolerance)) tol = tolerance
        
        msg = "Values not equal"
        if (present(message)) msg = message
        
        if (abs(actual - expected) > tol) then
            write(*, '(A,A)') "ASSERTION FAILED: ", trim(msg)
            write(*, '(A,G0,A,G0)') "  Expected: ", expected, ", Actual: ", actual
            stop 1
        end if
    end subroutine assert_equals
    
    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in), optional :: message
        character(len=100) :: msg
        
        msg = "Condition is false"
        if (present(message)) msg = message
        
        if (.not. condition) then
            write(*, '(A,A)') "ASSERTION FAILED: ", trim(msg)
            stop 1
        end if
    end subroutine assert_true

end module fortplot_testing