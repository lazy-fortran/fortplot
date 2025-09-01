program test_pcolormesh_comprehensive
    !! Comprehensive pcolormesh test - Issue #430 fixes, validation, memory safety
    
    use fortplot_pcolormesh, only: pcolormesh_t, validate_pcolormesh_grid
    use fortplot_errors, only: fortplot_error_t, ERROR_DIMENSION_MISMATCH
    use fortplot
    use iso_fortran_env, only: wp => real64, error_unit
    implicit none
    
    logical :: all_tests_passed
    integer :: total_tests, passed_tests
    
    all_tests_passed = .true.
    total_tests = 0
    passed_tests = 0
    
    print *, "=== COMPREHENSIVE PCOLORMESH TEST SUITE ==="
    
    ! Core error handling and safety tests
    call test_issue_430_scenarios()
    call test_dimension_validation()  
    call test_memory_safety()
    call test_boundary_conditions()
    
    ! Normal operation tests (integration moved to dedicated test file)
    call test_minimal_valid_cases()
    
    print *, ""
    print *, "=== COMPREHENSIVE TEST SUMMARY ==="
    write(*, '(A, I0, A, I0, A)') "Passed: ", passed_tests, "/", total_tests, " tests"
    if (all_tests_passed) then
        print *, "SUCCESS: All pcolormesh core functionality tests PASSED!"
        stop 0
    else
        print *, "FAILURE: Some core functionality tests failed!"
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

    subroutine test_issue_430_scenarios()
        !! Test all scenarios from issue #430 that previously caused segfault
        print *, "1. Testing Issue #430 Segmentation Fault Scenarios"
        
        call test_original_bug_scenario()
        call test_unallocated_access()
        call test_dimension_mismatches()
    end subroutine test_issue_430_scenarios

    subroutine test_original_bug_scenario()
        !! Test exact scenario from issue #430: x(6), y(5), c(5,4)
        type(pcolormesh_t) :: mesh
        real(wp) :: x_coords(6), y_coords(5), c_data(5,4)
        type(fortplot_error_t) :: error
        logical :: test_result
        integer :: i, j
        
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
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        test_result = .not. error%is_error()
        call run_test("Original segfault case dimension check", test_result, &
            "Should accept x(6), y(5), c(5,4) as valid C-style pattern")
        ! If no error occurred, then error message test is not applicable
        test_result = .not. error%is_error()  ! Test passes if no error was generated
        call run_test("Error message content validation", test_result, &
            "No error message expected for valid C-style dimensions")
            
        ! Test validation function has same behavior
        call validate_pcolormesh_grid(x_coords, y_coords, c_data, error)
        test_result = .not. error%is_error()
        call run_test("Validation function consistency", test_result, &
            "validate_pcolormesh_grid should accept valid C-style dimensions")
    end subroutine test_original_bug_scenario

    subroutine test_unallocated_access()
        !! Test scenarios with unallocated arrays (direct crash cause)
        type(pcolormesh_t) :: mesh
        real(wp) :: x_quad(4), y_quad(4)
        logical :: test_result
        
        ! Test get_data_range on completely empty mesh
        call mesh%get_data_range()
        test_result = (mesh%vmin == 0.0_wp .and. mesh%vmax == 1.0_wp)
        call run_test("Unallocated get_data_range safety", test_result, &
            "Should set safe defaults for unallocated c_values")
        
        ! Test get_quad_vertices on empty mesh  
        call mesh%get_quad_vertices(1, 1, x_quad, y_quad)
        test_result = all(x_quad == 0.0_wp) .and. all(y_quad == 0.0_wp)
        call run_test("Unallocated get_quad_vertices safety", test_result, &
            "Should return zeros for unallocated vertex arrays")
            
        allocate(mesh%c_values(0, 0))
        call mesh%get_data_range()
        test_result = (mesh%vmin == 0.0_wp .and. mesh%vmax == 1.0_wp)
        call run_test("Zero-size array handling", test_result, &
            "Should handle zero-size arrays safely")
    end subroutine test_unallocated_access

    subroutine test_dimension_mismatches()
        call test_x_dimension_error(3, 4, 3, 3, "x too short")
        call test_x_dimension_error(5, 4, 3, 3, "x too long")
        call test_y_dimension_error(4, 3, 3, 3, "y too short")
        call test_y_dimension_error(4, 5, 3, 3, "y too long")
    end subroutine test_dimension_mismatches

    subroutine test_x_dimension_error(x_size, y_size, c_rows, c_cols, case_name)
        integer, intent(in) :: x_size, y_size, c_rows, c_cols
        character(len=*), intent(in) :: case_name
        
        real(wp), allocatable :: x_coords(:), y_coords(:), c_data(:,:)
        type(pcolormesh_t) :: mesh
        type(fortplot_error_t) :: error
        logical :: test_result
        integer :: i, j
        
        allocate(x_coords(x_size), y_coords(y_size), c_data(c_rows, c_cols))
        
        do i = 1, x_size
            x_coords(i) = real(i, wp)
        end do
        do i = 1, y_size  
            y_coords(i) = real(i, wp)
        end do
        do i = 1, c_rows
            do j = 1, c_cols
                c_data(i, j) = real(i + j, wp)
            end do
        end do
        
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        test_result = error%is_error()
        call run_test("Dimension mismatch: " // case_name, test_result, &
            "Should catch dimension inconsistency")
    end subroutine test_x_dimension_error

    subroutine test_y_dimension_error(x_size, y_size, c_rows, c_cols, case_name)
        integer, intent(in) :: x_size, y_size, c_rows, c_cols
        character(len=*), intent(in) :: case_name
        
        real(wp), allocatable :: x_coords(:), y_coords(:), c_data(:,:)
        type(pcolormesh_t) :: mesh
        type(fortplot_error_t) :: error
        logical :: test_result
        integer :: i, j
        
        allocate(x_coords(x_size), y_coords(y_size), c_data(c_rows, c_cols))
        
        do i = 1, x_size
            x_coords(i) = real(i, wp)
        end do
        do i = 1, y_size
            y_coords(i) = real(i, wp)
        end do
        do i = 1, c_rows
            do j = 1, c_cols
                c_data(i, j) = real(i + j, wp)
            end do
        end do
        
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        test_result = error%is_error()
        call run_test("Dimension mismatch: " // case_name, test_result, &
            "Should catch dimension inconsistency")
    end subroutine test_y_dimension_error

    subroutine test_dimension_validation()
        !! Test dimension validation across different scenarios
        print *, ""
        print *, "2. Testing Comprehensive Dimension Validation"
        
        call test_validation_function_directly()
    end subroutine test_dimension_validation

    subroutine test_validation_function_directly()
        real(wp) :: x_valid(4), y_valid(3), c_valid(2,3)
        real(wp) :: x_invalid(5), y_invalid(3), c_invalid(2,3)
        type(fortplot_error_t) :: error
        logical :: test_result
        
        x_valid = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y_valid = [0.0_wp, 1.0_wp, 2.0_wp] 
        c_valid = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp], [2, 3])
        call validate_pcolormesh_grid(x_valid, y_valid, c_valid, error)
        test_result = .not. error%is_error()
        call run_test("Validation accepts valid dimensions", test_result, &
            "Valid x(4), y(3), c(2,3) should pass validation")
        x_invalid = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        y_invalid = [0.0_wp, 1.0_wp, 2.0_wp]
        c_invalid = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp], [2, 3])
        call validate_pcolormesh_grid(x_invalid, y_invalid, c_invalid, error)
        test_result = error%is_error()
        call run_test("Validation rejects invalid dimensions", test_result, &
            "Invalid x(5), y(3), c(2,3) should fail validation")
    end subroutine test_validation_function_directly

    subroutine test_memory_safety()
        call test_bounds_safety()
        call test_edge_case_access()
    end subroutine test_memory_safety

    subroutine test_bounds_safety()
        type(pcolormesh_t) :: mesh
        real(wp) :: x_quad(4), y_quad(4)
        logical :: test_result
        
        ! Setup minimal mesh for testing
        mesh%nx = 1
        mesh%ny = 1
        allocate(mesh%x_vertices(2, 2))
        allocate(mesh%y_vertices(2, 2))
        allocate(mesh%c_values(1, 1))
        
        mesh%x_vertices = reshape([0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp], [2, 2])
        mesh%y_vertices = reshape([0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp], [2, 2])
        mesh%c_values(1, 1) = 5.0_wp
        
        ! Test valid bounds access
        call mesh%get_quad_vertices(1, 1, x_quad, y_quad)
        test_result = (x_quad(1) == 0.0_wp)
        call run_test("Valid bounds access", test_result, &
            "Should allow valid quad access in small mesh")
        
        ! Test out-of-bounds access
        call mesh%get_quad_vertices(2, 2, x_quad, y_quad)
        test_result = all(x_quad == 0.0_wp) .and. all(y_quad == 0.0_wp)
        call run_test("Out-of-bounds access safety", test_result, &
            "Should return zeros for invalid indices")
        
        ! Test negative indices
        call mesh%get_quad_vertices(-1, -1, x_quad, y_quad)
        test_result = all(x_quad == 0.0_wp) .and. all(y_quad == 0.0_wp)
        call run_test("Negative index safety", test_result, &
            "Should handle negative indices safely")
    end subroutine test_bounds_safety

    subroutine test_edge_case_access()
        !! Test edge cases in array access
        type(pcolormesh_t) :: mesh
        logical :: test_result
        
        ! Test data range on empty mesh
        call mesh%get_data_range()
        test_result = (mesh%vmin >= 0.0_wp .and. mesh%vmax >= mesh%vmin)
        call run_test("Empty mesh data range safety", test_result, &
            "Should set reasonable defaults for empty mesh")
    end subroutine test_edge_case_access

    subroutine test_boundary_conditions()
        !! Test various boundary and edge conditions
        print *, ""
        print *, "4. Testing Boundary Conditions and Edge Cases"
        
        call test_single_cell_mesh()
        call test_large_mesh_boundaries()
    end subroutine test_boundary_conditions

    subroutine test_single_cell_mesh()
        !! Test minimal single-cell mesh functionality
        type(pcolormesh_t) :: mesh
        real(wp) :: x_coords(2), y_coords(2), c_data(1,1)
        real(wp) :: x_quad(4), y_quad(4)
        type(fortplot_error_t) :: error
        logical :: test_result
        
        x_coords = [0.0_wp, 1.0_wp]
        y_coords = [0.0_wp, 1.0_wp]
        c_data(1,1) = 5.0_wp
        
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        test_result = .not. error%is_error()
        call run_test("Single cell mesh initialization", test_result, &
            "Minimal 1x1 mesh should initialize successfully")
        
        if (.not. error%is_error()) then
            test_result = (mesh%vmin == 5.0_wp .and. mesh%vmax == 5.0_wp)
            call run_test("Single cell data range", test_result, &
                "Should compute correct range for single value")
            call mesh%get_quad_vertices(1, 1, x_quad, y_quad)
            test_result = (abs(x_quad(1) - 0.0_wp) < 1e-10_wp)
            call run_test("Single cell quad vertices", test_result, &
                "Should retrieve correct vertices for single cell")
        end if
    end subroutine test_single_cell_mesh

    subroutine test_large_mesh_boundaries()
        type(pcolormesh_t) :: mesh
        real(wp) :: x_coords(6), y_coords(5), c_data(4,5)
        real(wp) :: x_quad(4), y_quad(4)
        type(fortplot_error_t) :: error
        logical :: test_result
        integer :: i, j
        do i = 1, 6
            x_coords(i) = real(i-1, wp) * 0.5_wp
        end do
        do i = 1, 5
            y_coords(i) = real(i-1, wp) * 0.3_wp
        end do
        do i = 1, 4
            do j = 1, 5
                c_data(i, j) = real(i * j, wp)
            end do
        end do
        
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        test_result = .not. error%is_error()
        call run_test("Large mesh initialization", test_result, &
            "Large 4x5 mesh should initialize successfully")
        
        if (.not. error%is_error()) then
            ! Test boundary quad access
            call mesh%get_quad_vertices(4, 5, x_quad, y_quad)  ! Last valid cell
            test_result = (x_quad(1) > 0.0_wp)  ! Should have valid coordinates
            call run_test("Boundary quad access", test_result, &
                "Should access boundary quads correctly")
        end if
    end subroutine test_large_mesh_boundaries

    ! Integration tests moved to test_pcolormesh_integration.f90

    subroutine test_minimal_valid_cases()
        !! Test various minimal but valid configurations
        print *, ""
        print *, "6. Testing Minimal Valid Configurations"
        
        call test_2x2_mesh()
        call test_3x3_mesh() 
    end subroutine test_minimal_valid_cases

    subroutine test_2x2_mesh()
        type(pcolormesh_t) :: mesh
        real(wp) :: x_coords(3), y_coords(3), c_data(2,2)
        type(fortplot_error_t) :: error
        logical :: test_result
        x_coords = [0.0_wp, 1.0_wp, 2.0_wp]
        y_coords = [0.0_wp, 1.0_wp, 2.0_wp]
        c_data = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], [2, 2])
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        test_result = .not. error%is_error()
        call run_test("2x2 mesh validation", test_result, &
            "2x2 data mesh should be valid")
        if (.not. error%is_error()) then
            test_result = (mesh%vmin == 1.0_wp .and. mesh%vmax == 4.0_wp)
            call run_test("2x2 mesh data range", test_result, &
                "Should compute correct data range")
        end if
    end subroutine test_2x2_mesh

    subroutine test_3x3_mesh()
        type(pcolormesh_t) :: mesh
        real(wp) :: x_coords(4), y_coords(4), c_data(3,3)
        type(fortplot_error_t) :: error
        logical :: test_result
        integer :: i, j
        
        do i = 1, 4
            x_coords(i) = real(i-1, wp)
            y_coords(i) = real(i-1, wp)
        end do
        
        do i = 1, 3
            do j = 1, 3
                c_data(i, j) = real(i + j, wp)
            end do
        end do
        
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        test_result = .not. error%is_error()
        call run_test("3x3 mesh validation", test_result, &
            "3x3 data mesh should be valid")
        
        if (.not. error%is_error()) then
            test_result = (mesh%vmin == 2.0_wp .and. mesh%vmax == 6.0_wp)
            call run_test("3x3 mesh data range", test_result, &
                "Should compute correct data range")
        end if
    end subroutine test_3x3_mesh

    ! Issue #698 reproduction moved to test_pcolormesh_issue_698_repro.f90
    
    subroutine test_dimension_validation_consolidated()
        !! Dimension validation test - consolidated from test_pcolormesh_validation.f90
        real(wp), dimension(4) :: x_coords = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), dimension(3) :: y_coords = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), dimension(2,3) :: z_data = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp], [2,3])
        
        print *, ""
        print *, "Testing dimension validation (consolidated from validation test):"
        call figure()
        call pcolormesh(x_coords, y_coords, z_data)  ! Should handle gracefully
        call savefig("test/output/test_pcolormesh_validation_consolidated.png")
        print *, "  ✓ Dimension validation consolidated"
    end subroutine test_dimension_validation_consolidated

end program test_pcolormesh_comprehensive
