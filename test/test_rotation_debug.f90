program test_rotation_debug
    use fortplot_bitmap, only: rotate_bitmap_90_ccw, rotate_bitmap_90_cw
    implicit none
    
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
    
    print *, "Source matrix:"
    do j = 1, 3
        write(*, '(3I4)') (src(i, j, 1), i = 1, 3)
    end do
    
    ! Test counter-clockwise rotation
    call rotate_bitmap_90_ccw(src, dst_ccw, 3, 3)
    
    print *, ""
    print *, "After CCW rotation (should be rotated 90° counter-clockwise):"
    do j = 1, 3
        write(*, '(3I4)') (dst_ccw(i, j, 1), i = 1, 3)
    end do
    
    ! Test clockwise rotation
    call rotate_bitmap_90_cw(src, dst_cw, 3, 3)
    
    print *, ""
    print *, "After CW rotation (should be rotated 90° clockwise):"
    do j = 1, 3
        write(*, '(3I4)') (dst_cw(i, j, 1), i = 1, 3)
    end do
    
end program test_rotation_debug