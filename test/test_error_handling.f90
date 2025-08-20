program test_error_handling
    !! Comprehensive test suite for error handling architecture
    !! Tests that hard stops have been replaced with proper error returns
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_errors
    use fortplot_pcolormesh
    use fortplot_testing
    implicit none
    
    integer :: test_count, passed_count
    logical :: all_tests_passed
    
    test_count = 0
    passed_count = 0 
    all_tests_passed = .true.
    
    print *, "=========================================="
    print *, "Comprehensive Error Handling Tests"
    print *, "=========================================="
    print *, ""
    
    call test_error_type_functionality(all_tests_passed, test_count, passed_count)
    call test_pcolormesh_error_handling(all_tests_passed, test_count, passed_count)
    call test_validation_error_handling(all_tests_passed, test_count, passed_count)
    call test_testing_module_error_handling(all_tests_passed, test_count, passed_count)
    
    print *, ""
    print *, "=========================================="
    write(*, '(A,I0,A,I0,A)') "Results: ", passed_count, "/", test_count, " tests passed"
    
    if (all_tests_passed) then
        print *, "All error handling tests PASSED"
        print *, "✓ Hard stops successfully replaced with proper error handling"
        stop 0
    else
        print *, "Some error handling tests FAILED"
        print *, "✗ Error handling implementation has issues"
        stop 1
    end if

contains

    !> Test the basic error type functionality
    subroutine test_error_type_functionality(all_passed, test_count, passed_count)
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, passed_count
        
        type(fortplot_error_t) :: error
        character(len=256) :: msg
        
        print *, "Test Category: Error Type Functionality"
        print *, "-------------------------------------"
        
        ! Test error creation and status
        test_count = test_count + 1
        call error%set_error(ERROR_DIMENSION_MISMATCH, "Test error message")
        if (error%is_error() .and. error%status == ERROR_DIMENSION_MISMATCH) then
            print *, "  PASS: Error creation and status check"
            passed_count = passed_count + 1
        else
            print *, "  FAIL: Error creation or status check failed"
            all_passed = .false.
        end if
        
        ! Test error clearing
        test_count = test_count + 1
        call error%clear_error()
        if (.not. error%is_error() .and. error%status == SUCCESS) then
            print *, "  PASS: Error clearing"
            passed_count = passed_count + 1
        else
            print *, "  FAIL: Error clearing failed"
            all_passed = .false.
        end if
        
        ! Test error message retrieval
        test_count = test_count + 1
        msg = get_error_message(ERROR_INVALID_INPUT)
        if (len_trim(msg) > 0) then
            print *, "  PASS: Error message retrieval"
            passed_count = passed_count + 1
        else
            print *, "  FAIL: Error message retrieval failed"
            all_passed = .false.
        end if
        
        print *, ""
    end subroutine test_error_type_functionality

    !> Test pcolormesh error handling (replacing hard stops)
    subroutine test_pcolormesh_error_handling(all_passed, test_count, passed_count)
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, passed_count
        
        type(pcolormesh_t) :: pcm
        type(fortplot_error_t) :: error
        real(wp), allocatable :: x(:), y(:), c(:,:)
        
        print *, "Test Category: Pcolormesh Error Handling"
        print *, "---------------------------------------"
        
        ! Setup test data with intentional dimension mismatch
        allocate(x(3), y(4), c(2,3))  ! x should be size 4 (nx+1), y should be size 3 (ny+1)
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        c = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp], [2, 3])
        
        ! Test dimension mismatch error (should NOT crash with hard stop)
        test_count = test_count + 1
        call pcm%initialize_regular_grid(x, y, c, error=error)
        if (error%is_error() .and. error%status == ERROR_DIMENSION_MISMATCH) then
            print *, "  PASS: Regular grid dimension mismatch handled gracefully"
            passed_count = passed_count + 1
        else
            print *, "  FAIL: Regular grid should have returned dimension mismatch error"
            all_passed = .false.
        end if
        
        ! Test with correct dimensions (should succeed)
        test_count = test_count + 1
        deallocate(x)
        allocate(x(4))  ! Correct size: nx+1 = 4
        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        deallocate(y)
        allocate(y(3))  ! Correct size: ny+1 = 3  
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        
        call pcm%initialize_regular_grid(x, y, c, error=error)
        if (.not. error%is_error()) then
            print *, "  PASS: Correct dimensions handled successfully"
            passed_count = passed_count + 1
        else
            print *, "  FAIL: Correct dimensions should not produce error"
            all_passed = .false.
        end if
        
        print *, ""
    end subroutine test_pcolormesh_error_handling

    !> Test validation error handling 
    subroutine test_validation_error_handling(all_passed, test_count, passed_count)
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, passed_count
        
        type(fortplot_error_t) :: error
        real(wp), allocatable :: x(:), y(:), c(:,:)
        
        print *, "Test Category: Validation Error Handling"
        print *, "---------------------------------------"
        
        ! Setup invalid test data
        allocate(x(3), y(3), c(2,3))  ! x,y should be size 4 for c(2,3)
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        c = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp], [2, 3])
        
        ! Test validation with dimension mismatch
        test_count = test_count + 1
        call validate_pcolormesh_grid(x, y, c, error)
        if (error%is_error() .and. error%status == ERROR_DIMENSION_MISMATCH) then
            print *, "  PASS: Grid validation dimension mismatch handled"
            passed_count = passed_count + 1
        else
            print *, "  FAIL: Grid validation should detect dimension mismatch"
            all_passed = .false.
        end if
        
        print *, ""
    end subroutine test_validation_error_handling

    !> Test testing module error handling
    subroutine test_testing_module_error_handling(all_passed, test_count, passed_count)
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, passed_count
        
        type(test_result_t) :: test_result
        
        print *, "Test Category: Testing Module Error Handling"
        print *, "-------------------------------------------"
        
        ! Test that assertion failures are captured instead of causing hard stops
        test_count = test_count + 1
        call assert_true(.false., "Intentional test failure", test_result)
        if (.not. test_result%passed .and. test_result%error_count > 0) then
            print *, "  PASS: Assert failure captured without hard stop"
            passed_count = passed_count + 1
        else
            print *, "  FAIL: Assert failure should be captured"
            all_passed = .false.
        end if
        
        ! Test successful assertion
        test_count = test_count + 1
        test_result = test_result_t()  ! Reset
        call assert_true(.true., "Should pass", test_result)
        if (test_result%passed .and. test_result%error_count == 0) then
            print *, "  PASS: Successful assertion works correctly"
            passed_count = passed_count + 1
        else
            print *, "  FAIL: Successful assertion should not fail"
            all_passed = .false.
        end if
        
        print *, ""
    end subroutine test_testing_module_error_handling

end program test_error_handling