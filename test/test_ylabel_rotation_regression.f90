program test_ylabel_rotation_regression
    use fortplot
    use fortplot_bitmap, only: rotate_bitmap_90_ccw, rotate_bitmap_90_cw
    use iso_fortran_env, only: error_unit
    implicit none
    
    logical :: test_passed
    character(len=256) :: error_msg
    
    test_passed = .true.
    error_msg = ''
    
    call test_rotation_logic()
    call test_ylabel_rendering()
    
    if (test_passed) then
        print *, "SUCCESS: All ylabel rotation tests passed"
        stop 0
    else
        write(error_unit, *) "FAILURE: ", trim(error_msg)
        stop 1
    end if
    
contains
    
    subroutine test_rotation_logic()
        ! Test the rotation functions directly
        integer(1) :: src(3, 3, 1), dst_ccw(3, 3, 1), dst_cw(3, 3, 1)
        integer :: i, j
        
        ! Create test pattern:
        ! 1 2 3
        ! 4 5 6  
        ! 7 8 9
        do j = 1, 3
            do i = 1, 3
                src(i, j, 1) = int((j-1)*3 + i, 1)
            end do
        end do
        
        ! Test counter-clockwise rotation
        call rotate_bitmap_90_ccw(src, dst_ccw, 3, 3)
        
        ! Expected CCW result (90° counter-clockwise):
        ! 3 6 9
        ! 2 5 8
        ! 1 4 7
        if (dst_ccw(1, 1, 1) /= 3 .or. dst_ccw(2, 1, 1) /= 6 .or. dst_ccw(3, 1, 1) /= 9 .or. &
            dst_ccw(1, 2, 1) /= 2 .or. dst_ccw(2, 2, 1) /= 5 .or. dst_ccw(3, 2, 1) /= 8 .or. &
            dst_ccw(1, 3, 1) /= 1 .or. dst_ccw(2, 3, 1) /= 4 .or. dst_ccw(3, 3, 1) /= 7) then
            test_passed = .false.
            error_msg = "CCW rotation incorrect"
            return
        end if
        
        ! Test clockwise rotation
        call rotate_bitmap_90_cw(src, dst_cw, 3, 3)
        
        ! Expected CW result (90° clockwise):
        ! 7 4 1
        ! 8 5 2
        ! 9 6 3
        if (dst_cw(1, 1, 1) /= 7 .or. dst_cw(2, 1, 1) /= 4 .or. dst_cw(3, 1, 1) /= 1 .or. &
            dst_cw(1, 2, 1) /= 8 .or. dst_cw(2, 2, 1) /= 5 .or. dst_cw(3, 2, 1) /= 2 .or. &
            dst_cw(1, 3, 1) /= 9 .or. dst_cw(2, 3, 1) /= 6 .or. dst_cw(3, 3, 1) /= 3) then
            test_passed = .false.
            error_msg = "CW rotation incorrect"
            return
        end if
        
        print *, "Rotation logic test: PASSED"
    end subroutine test_rotation_logic
    
    subroutine test_ylabel_rendering()
        ! Test ylabel rendering in actual plot
        real(8) :: x(10), y(10)
        integer :: i
        logical :: file_exists
        
        ! Create simple data
        do i = 1, 10
            x(i) = real(i, 8)
            y(i) = real(i*i, 8)
        end do
        
        ! Generate plot with ylabel
        call figure()
        call plot(x, y)
        call xlabel("Horizontal X Label")
        call ylabel("Vertical Y Label")
        call title("Y-Label Rotation Regression Test")
        call savefig("test_ylabel_regression.png")
        
        ! Check file was created
        inquire(file="test_ylabel_regression.png", exist=file_exists)
        if (.not. file_exists) then
            test_passed = .false.
            error_msg = "Failed to create PNG with ylabel"
            return
        end if
        
        print *, "Y-label rendering test: PASSED"
        print *, "Created test_ylabel_regression.png - ylabel should be rotated 90° CCW"
    end subroutine test_ylabel_rendering
    
end program test_ylabel_rotation_regression