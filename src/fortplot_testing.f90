module fortplot_testing
    !! Simple testing utilities for fortplotlib
    implicit none
    
    private
    public :: assert_true, assert_false, assert_equals
    
contains
    
    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        
        if (.not. condition) then
            print *, "ASSERTION FAILED: ", trim(message)
            error stop 1
        end if
    end subroutine assert_true
    
    subroutine assert_false(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        
        if (condition) then
            print *, "ASSERTION FAILED: ", trim(message)
            error stop 1
        end if
    end subroutine assert_false
    
    subroutine assert_equals(expected, actual, message)
        real(8), intent(in) :: expected, actual
        character(len=*), intent(in) :: message
        real(8), parameter :: tolerance = 1.0d-10
        
        if (abs(expected - actual) > tolerance) then
            print *, "ASSERTION FAILED: ", trim(message)
            print *, "  Expected: ", expected
            print *, "  Actual:   ", actual
            error stop 1
        end if
    end subroutine assert_equals
    
end module fortplot_testing