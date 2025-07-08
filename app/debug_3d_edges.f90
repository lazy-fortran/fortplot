program debug_3d_edges
    use iso_fortran_env, only: wp => real64
    use fortplot_3d_axes
    use fortplot_projection
    implicit none
    
    real(wp) :: corners_3d(3,8), corners_2d(2,8)
    real(wp) :: azim, elev, dist
    integer :: i
    integer, parameter :: edges(2,12) = reshape([ &
        1,2, 2,3, 3,4, 4,1, &  ! Bottom face edges (1-4)
        5,6, 6,7, 7,8, 8,5, &  ! Top face edges (5-8)
        1,5, 2,6, 3,7, 4,8  &  ! Vertical edges (9-12)
        ], [2,12])
    
    print *, "3D Box Edge Analysis"
    print *, "===================="
    
    ! Create unit cube
    call create_3d_axis_corners(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, corners_3d)
    
    print *, "Corner definitions:"
    print *, "1: (0,0,0) - origin"
    print *, "2: (1,0,0) - x-axis end"
    print *, "3: (1,1,0) - xy corner"
    print *, "4: (0,1,0) - y-axis end"
    print *, "5: (0,0,1) - z-axis end"
    print *, "6: (1,0,1) - xz corner"
    print *, "7: (1,1,1) - xyz corner"
    print *, "8: (0,1,1) - yz corner"
    print *, ""
    
    print *, "Edge definitions:"
    do i = 1, 12
        print '(A,I2,A,I1,A,I1)', "Edge ", i, ": corner ", edges(1,i), " to ", edges(2,i)
    end do
    print *, ""
    
    ! Project corners
    call get_default_view_angles(azim, elev, dist)
    call project_3d_corners_to_2d(corners_3d, azim, elev, dist, corners_2d)
    
    print *, "With matplotlib view (azim=-60°, elev=30°):"
    print *, "Projected 2D positions:"
    do i = 1, 8
        print '(A,I1,A,2F8.3)', "Corner ", i, ": ", corners_2d(:,i)
    end do
    
    ! Analyze which edges should be visible
    print *, ""
    print *, "For matplotlib-style 3D axes:"
    print *, "- Front edges (bold): X-axis (1->2), Y-axis (1->4), Z-axis (1->5)"
    print *, "- These are edges: 1, 4, 9"
    print *, "- Back edges (thin): edges 6, 7, 11"
    print *, "- Hidden edges: all others"
    
end program debug_3d_edges