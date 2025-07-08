program debug_3d_projection
    use iso_fortran_env, only: wp => real64
    use fortplot_projection
    use fortplot_3d_axes
    implicit none
    
    real(wp) :: x3d(8), y3d(8), z3d(8), x2d(8), y2d(8)
    real(wp) :: azim, elev, dist
    real(wp) :: corners_3d(3,8), corners_2d(2,8)
    integer :: i
    
    print *, "Debug 3D Projection:"
    print *, "==================="
    
    ! Get default view angles
    call get_default_view_angles(azim, elev, dist)
    print *, "Azimuth (deg):", azim * 180.0_wp / 3.14159_wp
    print *, "Elevation (deg):", elev * 180.0_wp / 3.14159_wp
    print *, "Distance:", dist
    print *, ""
    
    ! Test simple unit cube corners
    call create_3d_axis_corners(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, corners_3d)
    
    print *, "3D Corners:"
    do i = 1, 8
        print '(A,I1,A,3F8.3)', "Corner ", i, ": ", corners_3d(:,i)
    end do
    print *, ""
    
    ! Extract arrays for projection
    x3d = corners_3d(1,:)
    y3d = corners_3d(2,:)
    z3d = corners_3d(3,:)
    
    ! Project to 2D
    call project_3d_to_2d(x3d, y3d, z3d, azim, elev, dist, x2d, y2d)
    
    print *, "2D Projected Corners:"
    do i = 1, 8
        print '(A,I1,A,2F8.3)', "Corner ", i, ": ", x2d(i), y2d(i)
    end do
    print *, ""
    
    print *, "Projection bounds:"
    print *, "X range:", minval(x2d), "to", maxval(x2d), "spread:", maxval(x2d) - minval(x2d)
    print *, "Y range:", minval(y2d), "to", maxval(y2d), "spread:", maxval(y2d) - minval(y2d)
    
    ! Test individual axis points
    print *, ""
    print *, "Testing axis endpoints:"
    
    ! X-axis: (0,0,0) to (1,0,0)
    x3d(1:2) = [0.0_wp, 1.0_wp]
    y3d(1:2) = [0.0_wp, 0.0_wp]
    z3d(1:2) = [0.0_wp, 0.0_wp]
    call project_3d_to_2d(x3d(1:2), y3d(1:2), z3d(1:2), azim, elev, dist, x2d(1:2), y2d(1:2))
    print *, "X-axis: (0,0,0) ->", x2d(1), y2d(1), " to (1,0,0) ->", x2d(2), y2d(2)
    
    ! Y-axis: (0,0,0) to (0,1,0)
    x3d(1:2) = [0.0_wp, 0.0_wp]
    y3d(1:2) = [0.0_wp, 1.0_wp]
    z3d(1:2) = [0.0_wp, 0.0_wp]
    call project_3d_to_2d(x3d(1:2), y3d(1:2), z3d(1:2), azim, elev, dist, x2d(1:2), y2d(1:2))
    print *, "Y-axis: (0,0,0) ->", x2d(1), y2d(1), " to (0,1,0) ->", x2d(2), y2d(2)
    
    ! Z-axis: (0,0,0) to (0,0,1)
    x3d(1:2) = [0.0_wp, 0.0_wp]
    y3d(1:2) = [0.0_wp, 0.0_wp]
    z3d(1:2) = [0.0_wp, 1.0_wp]
    call project_3d_to_2d(x3d(1:2), y3d(1:2), z3d(1:2), azim, elev, dist, x2d(1:2), y2d(1:2))
    print *, "Z-axis: (0,0,0) ->", x2d(1), y2d(1), " to (0,0,1) ->", x2d(2), y2d(2)
    
end program debug_3d_projection