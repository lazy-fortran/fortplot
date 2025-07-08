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
    print *, "Looking at actual matplotlib output:"
    print *, ""
    print *, "X-axis: runs along back bottom edge"
    print *, "        This is corner 4 (0,1,0) to corner 3 (1,1,0)"
    print *, ""
    print *, "Y-axis: runs along left bottom edge" 
    print *, "        This is corner 1 (0,0,0) to corner 4 (0,1,0)"
    print *, ""
    print *, "Z-axis: runs up the back-left vertical edge"
    print *, "        This is corner 4 (0,1,0) to corner 8 (0,1,1)"
    print *, ""
    print *, "So all three axes meet at corner 4!"
    
end program debug_matplotlib_axes