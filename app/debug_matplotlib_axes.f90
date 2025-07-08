program debug_matplotlib_axes
    use iso_fortran_env, only: wp => real64
    use fortplot_3d_axes
    use fortplot_projection
    implicit none
    
    real(wp) :: corners_3d(3,8), corners_2d(2,8)
    real(wp) :: azim, elev, dist
    integer :: i
    
    print *, "Matplotlib-style 3D axes analysis"
    print *, "================================="
    
    ! Create unit cube
    call create_3d_axis_corners(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, corners_3d)
    
    ! Project corners
    call get_default_view_angles(azim, elev, dist)
    call project_3d_corners_to_2d(corners_3d, azim, elev, dist, corners_2d)
    
    print *, ""
    print *, "With azim=-60°, elev=30°, matplotlib draws:"
    print *, ""
    print *, "X-axis: NOT from origin, but the back bottom edge"
    print *, "        This is corner 3 to corner 2 (back of bottom face)"
    print *, ""
    print *, "Y-axis: NOT from origin, but the left bottom edge" 
    print *, "        This is corner 4 to corner 3 (left of bottom face)"
    print *, ""
    print *, "Z-axis: NOT from origin, but the back left vertical edge"
    print *, "        This is corner 3 to corner 7 (back left vertical)"
    print *, ""
    print *, "So all three axes meet at corner 3, not corner 1!"
    
end program debug_matplotlib_axes