program test_pcolormesh_430_regression
    !! Regression test for issue #430 - segmentation fault fix
    !! Verifies that all previously crashing scenarios now handle gracefully
    
    use fortplot_pcolormesh, only: pcolormesh_t
    use fortplot_errors, only: fortplot_error_t
    use iso_fortran_env, only: wp => real64, error_unit
    implicit none
    
    logical :: all_tests_passed
    integer :: total_tests, passed_tests
    
    all_tests_passed = .true.
    total_tests = 0
    passed_tests = 0
    
    print *, "=== REGRESSION TEST: Issue #430 Segmentation Fault ==="
    print *, ""
    print *, "Testing scenarios that previously caused segmentation fault"
    print *, "All tests should now handle gracefully with proper error messages"
    print *, ""
    
    ! Test original bug scenario
    call test_original_bug_scenario()
    
    ! Test unallocated array access
    call test_unallocated_array_scenarios()
    
    ! Test boundary conditions  
    call test_boundary_conditions()
    
    ! Test successful operations still work
    call test_successful_operations()
    
    print *, ""
    print *, "=== REGRESSION TEST SUMMARY ==="
    write(*, '(A, I0, A, I0, A)') "Passed: ", passed_tests, "/", total_tests, " tests"
    
    if (all_tests_passed) then
        print *, ""
        print *, "SUCCESS: Issue #430 segmentation fault has been COMPLETELY FIXED!"
        print *, "All previously crashing scenarios now handle gracefully."
        stop 0
    else
        print *, ""
        print *, "FAILURE: Some tests failed - segmentation fault not completely fixed!"
        stop 1
    end if
    
contains

    subroutine run_test(test_name, test_result, description)
        character(len=*), intent(in) :: test_name, description
        logical, intent(in) :: test_result
        
        total_tests = total_tests + 1
        
        if (test_result) then
            passed_tests = passed_tests + 1
            print *, "✓ PASS: " // trim(test_name)
        else
            all_tests_passed = .false.
            print *, "✗ FAIL: " // trim(test_name) // " - " // trim(description)
        end if
    end subroutine run_test
    
    subroutine test_original_bug_scenario()
        !! Test the exact scenario from issue #430 bug report
        type(pcolormesh_t) :: mesh
        real(wp) :: x_coords(6), y_coords(5), c_data(5,4)
        type(fortplot_error_t) :: error
        logical :: test_result
        integer :: i, j
        
        print *, "1. Testing original bug scenario: x(6), y(5), c(5,4)"
        
        ! Initialize the problematic data from bug report
        do i = 1, 6
            x_coords(i) = real(i-1, wp)
        end do
        
        do i = 1, 5  
            y_coords(i) = real(i-1, wp)
        end do
        
        do i = 1, 5
            do j = 1, 4
                c_data(i, j) = real(i * j, wp)
            end do
        end do
        
        ! This should fail gracefully, NOT crash
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        
        test_result = error%is_error()  ! Should have caught the dimension mismatch
        call run_test("Original segfault case", test_result, &
            "Should catch dimension mismatch x(6) with c(5,4)")
            
        ! Verify specific error message
        test_result = index(error%message, "x_coords size must be nx+1") > 0
        call run_test("Correct error message", test_result, &
            "Error message should mention x_coords size requirement")
    end subroutine test_original_bug_scenario
    
    subroutine test_unallocated_array_scenarios()
        !! Test scenarios with unallocated arrays
        type(pcolormesh_t) :: mesh
        real(wp) :: x_quad(4), y_quad(4)
        logical :: test_result
        
        print *, ""
        print *, "2. Testing unallocated array access scenarios"
        
        ! Test get_data_range on empty mesh
        call mesh%get_data_range()
        test_result = (mesh%vmin == 0.0_wp .and. mesh%vmax == 1.0_wp)
        call run_test("Unallocated get_data_range", test_result, &
            "Should set safe defaults for unallocated c_values")
        
        ! Test get_quad_vertices on empty mesh
        call mesh%get_quad_vertices(1, 1, x_quad, y_quad)
        test_result = all(x_quad == 0.0_wp) .and. all(y_quad == 0.0_wp)
        call run_test("Unallocated get_quad_vertices", test_result, &
            "Should return zeros for unallocated vertex arrays")
        
        ! Test with zero-size arrays
        allocate(mesh%c_values(0, 0))
        call mesh%get_data_range()
        test_result = (mesh%vmin == 0.0_wp .and. mesh%vmax == 1.0_wp)
        call run_test("Zero-size array handling", test_result, &
            "Should handle zero-size arrays safely")
    end subroutine test_unallocated_array_scenarios
    
    subroutine test_boundary_conditions()
        !! Test edge cases and boundary conditions
        type(pcolormesh_t) :: mesh
        real(wp) :: x_quad(4), y_quad(4)
        logical :: test_result
        
        print *, ""
        print *, "3. Testing boundary conditions and edge cases"
        
        ! Setup small mesh for boundary testing
        mesh%nx = 1
        mesh%ny = 1
        allocate(mesh%x_vertices(2, 2))
        allocate(mesh%y_vertices(2, 2))
        allocate(mesh%c_values(1, 1))
        
        mesh%x_vertices = reshape([0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp], [2, 2])
        mesh%y_vertices = reshape([0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp], [2, 2])
        mesh%c_values(1, 1) = 5.0_wp
        
        ! Test valid access
        call mesh%get_quad_vertices(1, 1, x_quad, y_quad)
        test_result = (x_quad(1) == 0.0_wp)  ! Should work
        call run_test("Valid boundary access", test_result, &
            "Should allow valid quad access in small mesh")
        
        ! Test out-of-bounds access
        call mesh%get_quad_vertices(2, 2, x_quad, y_quad)  ! Invalid
        test_result = all(x_quad == 0.0_wp) .and. all(y_quad == 0.0_wp)
        call run_test("Out-of-bounds access", test_result, &
            "Should return zeros for invalid indices")
        
        ! Test negative indices
        call mesh%get_quad_vertices(-1, -1, x_quad, y_quad)
        test_result = all(x_quad == 0.0_wp) .and. all(y_quad == 0.0_wp)
        call run_test("Negative index handling", test_result, &
            "Should handle negative indices safely")
    end subroutine test_boundary_conditions
    
    subroutine test_successful_operations()
        !! Verify that normal operations still work correctly
        type(pcolormesh_t) :: mesh
        real(wp) :: x_coords(3), y_coords(3), c_data(2,2)
        real(wp) :: x_quad(4), y_quad(4)
        type(fortplot_error_t) :: error
        logical :: test_result
        
        print *, ""
        print *, "4. Testing successful operations after safety fixes"
        
        ! Initialize valid data
        x_coords = [0.0_wp, 1.0_wp, 2.0_wp]
        y_coords = [0.0_wp, 1.0_wp, 2.0_wp]
        c_data = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], [2, 2])
        
        ! This should succeed
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        test_result = .not. error%is_error()
        call run_test("Valid initialization", test_result, &
            "Should succeed with correct dimensions")
        
        ! Test data range computation
        test_result = (mesh%vmin == 1.0_wp .and. mesh%vmax == 4.0_wp)
        call run_test("Data range computation", test_result, &
            "Should compute correct data range")
        
        ! Test quad vertices retrieval
        call mesh%get_quad_vertices(1, 1, x_quad, y_quad)
        test_result = (abs(x_quad(1) - 0.0_wp) < 1e-10_wp)
        call run_test("Quad vertices retrieval", test_result, &
            "Should retrieve correct quad vertices")
        
        ! Test all four corners of first quad
        test_result = (abs(x_quad(2) - 1.0_wp) < 1e-10_wp .and. &  ! bottom-right
                      abs(x_quad(3) - 1.0_wp) < 1e-10_wp .and. &  ! top-right  
                      abs(x_quad(4) - 0.0_wp) < 1e-10_wp)         ! top-left
        call run_test("Complete quad geometry", test_result, &
            "Should have correct geometry for all quad vertices")
    end subroutine test_successful_operations

end program test_pcolormesh_430_regression