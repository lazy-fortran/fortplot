program test_pcolormesh_segfault_430
    !! Test to reproduce segmentation fault in issue #430
    !! Tests array dimension mismatch that causes program crash
    
    use fortplot_pcolormesh, only: pcolormesh_t, validate_pcolormesh_grid
    use fortplot_errors, only: fortplot_error_t
    use iso_fortran_env, only: wp => real64
    implicit none
    
    logical :: test_passed
    character(len=512) :: error_msg
    
    test_passed = .true.
    error_msg = ""
    
    print *, "Testing pcolormesh segmentation fault issue #430..."
    print *, ""
    
    ! Test the specific case that causes segfault: x(6), y(5), c(5,4)
    call test_segfault_case()
    
    ! Test other dimension mismatch cases
    call test_various_dimension_mismatches()
    
    ! Test edge cases
    call test_edge_cases()
    
    if (test_passed) then
        print *, ""
        print *, "PASS: All segmentation fault tests handled gracefully"
        print *, "SUCCESS: Issue #430 segmentation fault has been FIXED!"
        stop 0
    else
        print *, "FAIL: ", trim(error_msg)
        stop 1
    end if
    
contains

    subroutine test_segfault_case()
        !! Test the specific case from issue #430 that causes segfault
        real(wp) :: x_coords(6), y_coords(5), c_data(5,4)
        type(pcolormesh_t) :: mesh
        type(fortplot_error_t) :: error
        integer :: i, j
        
        print *, "Testing original segfault case: x(6), y(5), c(5,4)..."
        
        ! Initialize test data
        do i = 1, 6
            x_coords(i) = real(i-1, wp)
        end do
        
        do i = 1, 5
            y_coords(i) = real(i-1, wp)
        end do
        
        do i = 1, 5
            do j = 1, 4
                c_data(i, j) = real(i + j, wp)
            end do
        end do
        
        ! This should fail gracefully, not crash
        print *, "  Expected: x(6) should be x(5) for c(5,4) data"
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        
        if (error%is_error()) then
            print *, "  GOOD: Error caught gracefully - ", trim(error%message)
        else
            test_passed = .false.
            error_msg = "Expected dimension mismatch error was not caught"
            return
        end if
        
        ! Test with validation function
        call validate_pcolormesh_grid(x_coords, y_coords, c_data, error)
        if (error%is_error()) then
            print *, "  GOOD: Validation caught error - ", trim(error%message) 
        else
            test_passed = .false.
            error_msg = "Validation should have caught dimension mismatch"
            return
        end if
        
        print *, "  Segfault case handled gracefully!"
    end subroutine test_segfault_case
    
    subroutine test_various_dimension_mismatches()
        !! Test various dimension mismatch scenarios
        type(pcolormesh_t) :: mesh
        type(fortplot_error_t) :: error
        
        print *, ""
        print *, "Testing various dimension mismatches..."
        
        ! Case 1: x too short
        call test_x_too_short()
        
        ! Case 2: x too long  
        call test_x_too_long()
        
        ! Case 3: y too short
        call test_y_too_short()
        
        ! Case 4: y too long
        call test_y_too_long()
        
        print *, "  All dimension mismatches handled gracefully!"
    end subroutine test_various_dimension_mismatches
    
    subroutine test_x_too_short()
        real(wp) :: x_coords(3), y_coords(4), c_data(3,3)  ! x should be 4
        type(pcolormesh_t) :: mesh
        type(fortplot_error_t) :: error
        
        x_coords = [1.0_wp, 2.0_wp, 3.0_wp]
        y_coords = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        c_data = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, &
                         7.0_wp, 8.0_wp, 9.0_wp], [3, 3])
        
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        
        if (.not. error%is_error()) then
            test_passed = .false.
            error_msg = "x too short case should have been caught"
        end if
    end subroutine test_x_too_short
    
    subroutine test_x_too_long()
        real(wp) :: x_coords(5), y_coords(4), c_data(3,3)  ! x should be 4
        type(pcolormesh_t) :: mesh
        type(fortplot_error_t) :: error
        
        x_coords = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        y_coords = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        c_data = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, &
                         7.0_wp, 8.0_wp, 9.0_wp], [3, 3])
        
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        
        if (.not. error%is_error()) then
            test_passed = .false.
            error_msg = "x too long case should have been caught"
        end if
    end subroutine test_x_too_long
    
    subroutine test_y_too_short()
        real(wp) :: x_coords(4), y_coords(3), c_data(3,3)  ! y should be 4
        type(pcolormesh_t) :: mesh
        type(fortplot_error_t) :: error
        
        x_coords = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        y_coords = [1.0_wp, 2.0_wp, 3.0_wp]
        c_data = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, &
                         7.0_wp, 8.0_wp, 9.0_wp], [3, 3])
        
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        
        if (.not. error%is_error()) then
            test_passed = .false.
            error_msg = "y too short case should have been caught"
        end if
    end subroutine test_y_too_short
    
    subroutine test_y_too_long()
        real(wp) :: x_coords(4), y_coords(5), c_data(3,3)  ! y should be 4
        type(pcolormesh_t) :: mesh
        type(fortplot_error_t) :: error
        
        x_coords = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        y_coords = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        c_data = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, &
                         7.0_wp, 8.0_wp, 9.0_wp], [3, 3])
        
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        
        if (.not. error%is_error()) then
            test_passed = .false.
            error_msg = "y too long case should have been caught"
        end if
    end subroutine test_y_too_long
    
    subroutine test_edge_cases()
        !! Test edge cases like empty arrays
        type(pcolormesh_t) :: mesh
        type(fortplot_error_t) :: error
        
        print *, ""
        print *, "Testing edge cases..."
        
        ! Test with minimal valid data (1x1 grid)
        call test_minimal_valid_case()
        
        print *, "  All edge cases handled properly!"
    end subroutine test_edge_cases
    
    subroutine test_minimal_valid_case()
        !! Test with minimal valid 1x1 grid
        real(wp) :: x_coords(2), y_coords(2), c_data(1,1)
        type(pcolormesh_t) :: mesh
        type(fortplot_error_t) :: error
        
        x_coords = [0.0_wp, 1.0_wp]
        y_coords = [0.0_wp, 1.0_wp]
        c_data(1,1) = 5.0_wp
        
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        
        if (error%is_error()) then
            test_passed = .false.
            error_msg = "Minimal valid case should not produce error: " // trim(error%message)
        else
            print *, "  Minimal valid case (1x1 grid) works correctly"
        end if
    end subroutine test_minimal_valid_case

end program test_pcolormesh_segfault_430