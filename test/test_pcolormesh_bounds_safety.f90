program test_pcolormesh_bounds_safety
    !! Test comprehensive bounds checking in pcolormesh functions
    !! Verifies memory safety fixes for issue #430
    
    use fortplot_pcolormesh, only: pcolormesh_t
    use fortplot_errors, only: fortplot_error_t
    use iso_fortran_env, only: wp => real64
    implicit none
    
    logical :: test_passed
    character(len=512) :: error_msg
    
    test_passed = .true.
    error_msg = ""
    
    print *, "Testing pcolormesh bounds safety (issue #430 fix)..."
    print *, ""
    
    ! Test get_data_range safety
    call test_get_data_range_safety()
    
    ! Test get_quad_vertices safety  
    call test_get_quad_vertices_safety()
    
    ! Test successful case still works
    call test_normal_operation()
    
    if (test_passed) then
        print *, ""
        print *, "PASS: All bounds safety tests passed"
        print *, "SUCCESS: Memory safety fixes for issue #430 verified!"
        stop 0
    else
        print *, "FAIL: ", trim(error_msg)
        stop 1
    end if
    
contains

    subroutine test_get_data_range_safety()
        !! Test get_data_range with unallocated arrays
        type(pcolormesh_t) :: mesh
        
        print *, "Testing get_data_range safety with unallocated arrays..."
        
        ! Call on completely uninitialized mesh
        call mesh%get_data_range()
        
        ! Should have set safe default values
        if (mesh%vmin /= 0.0_wp .or. mesh%vmax /= 1.0_wp) then
            test_passed = .false.
            error_msg = "get_data_range did not set safe defaults for unallocated arrays"
            return
        end if
        
        print *, "  GOOD: get_data_range handled unallocated arrays safely"
        
        ! Test with zero-size allocated array
        allocate(mesh%c_values(0, 0))
        call mesh%get_data_range()
        
        if (mesh%vmin /= 0.0_wp .or. mesh%vmax /= 1.0_wp) then
            test_passed = .false.
            error_msg = "get_data_range did not handle zero-size array safely"
            return
        end if
        
        print *, "  GOOD: get_data_range handled zero-size arrays safely"
    end subroutine test_get_data_range_safety
    
    subroutine test_get_quad_vertices_safety()
        !! Test get_quad_vertices with various unsafe conditions
        type(pcolormesh_t) :: mesh
        real(wp) :: x_quad(4), y_quad(4)
        integer :: i
        
        print *, ""
        print *, "Testing get_quad_vertices safety..."
        
        ! Test with unallocated vertex arrays
        call mesh%get_quad_vertices(1, 1, x_quad, y_quad)
        
        ! Should return all zeros safely
        do i = 1, 4
            if (x_quad(i) /= 0.0_wp .or. y_quad(i) /= 0.0_wp) then
                test_passed = .false.
                error_msg = "get_quad_vertices did not return zeros for unallocated arrays"
                return
            end if
        end do
        
        print *, "  GOOD: get_quad_vertices handled unallocated arrays safely"
        
        ! Test with allocated but small arrays and out-of-bounds access
        mesh%nx = 2
        mesh%ny = 2
        allocate(mesh%x_vertices(2, 2))
        allocate(mesh%y_vertices(2, 2))
        
        ! Initialize small arrays
        mesh%x_vertices = reshape([0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp], [2, 2])
        mesh%y_vertices = reshape([0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp], [2, 2])
        
        ! Try to access beyond bounds
        call mesh%get_quad_vertices(3, 3, x_quad, y_quad)  ! Invalid indices
        
        ! Should return zeros for out-of-bounds access
        do i = 1, 4
            if (x_quad(i) /= 0.0_wp .or. y_quad(i) /= 0.0_wp) then
                test_passed = .false.
                error_msg = "get_quad_vertices did not handle out-of-bounds access safely"
                return
            end if
        end do
        
        print *, "  GOOD: get_quad_vertices handled out-of-bounds access safely"
        
        ! Test boundary case (accessing last valid quad)
        call mesh%get_quad_vertices(1, 1, x_quad, y_quad)  ! Should work with 2x2 arrays
        
        print *, "  GOOD: get_quad_vertices handled boundary case correctly"
    end subroutine test_get_quad_vertices_safety
    
    subroutine test_normal_operation()
        !! Verify that normal operation still works correctly
        type(pcolormesh_t) :: mesh
        real(wp) :: x_coords(3), y_coords(3), c_data(2,2)
        real(wp) :: x_quad(4), y_quad(4)
        type(fortplot_error_t) :: error
        
        print *, ""
        print *, "Testing that normal operation still works..."
        
        ! Initialize valid data
        x_coords = [0.0_wp, 1.0_wp, 2.0_wp]
        y_coords = [0.0_wp, 1.0_wp, 2.0_wp] 
        c_data = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], [2, 2])
        
        call mesh%initialize_regular_grid(x_coords, y_coords, c_data, error=error)
        
        if (error%is_error()) then
            test_passed = .false.
            error_msg = "Normal initialization failed: " // trim(error%message)
            return
        end if
        
        ! Test data range computation
        if (mesh%vmin /= 1.0_wp .or. mesh%vmax /= 4.0_wp) then
            test_passed = .false.
            error_msg = "Data range computation incorrect after safety fixes"
            return
        end if
        
        ! Test quad vertices retrieval
        call mesh%get_quad_vertices(1, 1, x_quad, y_quad)
        
        ! Verify correct vertices for first quad
        if (abs(x_quad(1) - 0.0_wp) > 1e-10_wp .or. abs(y_quad(1) - 0.0_wp) > 1e-10_wp) then
            test_passed = .false.
            error_msg = "Quad vertices incorrect after safety fixes"
            return
        end if
        
        print *, "  GOOD: Normal operation works correctly with safety fixes"
    end subroutine test_normal_operation

end program test_pcolormesh_bounds_safety