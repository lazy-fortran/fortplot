program test_3d_axes
    use fortplot_projection
    use fortplot_3d_axes
    use fortplot_testing
    use iso_fortran_env, only: wp => real64
    implicit none
    
    call test_3d_axis_corners()
    call test_3d_axis_projection()
    call test_3d_tick_positions()
    
    print *, "All 3D axes tests passed!"
    
contains

    subroutine test_3d_axis_corners()
        real(wp) :: corners_3d(3,8), corners_2d(2,8)
        real(wp) :: azim, elev, dist
        real(wp) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        ! Define 3D bounding box
        x_min = -1.0_wp; x_max = 1.0_wp
        y_min = -1.0_wp; y_max = 1.0_wp  
        z_min = -1.0_wp; z_max = 1.0_wp
        
        call get_default_view_angles(azim, elev, dist)
        call create_3d_axis_corners(x_min, x_max, y_min, y_max, z_min, z_max, corners_3d)
        call project_3d_corners_to_2d(corners_3d, azim, elev, dist, corners_2d)
        
        ! Check we have 8 corners
        call assert_equals(real(size(corners_3d, 2), wp), 8.0_wp, 1e-6_wp, "Should have 8 corners")
        
        ! Check corners are within reasonable bounds after projection
        call assert_true(maxval(corners_2d(1,:)) > minval(corners_2d(1,:)), "X range should be positive")
        call assert_true(maxval(corners_2d(2,:)) > minval(corners_2d(2,:)), "Y range should be positive")
    end subroutine

    subroutine test_3d_axis_projection()
        real(wp) :: axis_lines_3d(3,6), axis_lines_2d(2,6)
        real(wp) :: azim, elev, dist
        
        call get_default_view_angles(azim, elev, dist)
        call create_3d_axis_lines(-1.0_wp, 1.0_wp, -1.0_wp, 1.0_wp, -1.0_wp, 1.0_wp, axis_lines_3d)
        call project_3d_axis_lines(axis_lines_3d, azim, elev, dist, axis_lines_2d)
        
        ! Should have 6 points (3 axes × 2 points each)
        call assert_equals(real(size(axis_lines_3d, 2), wp), 6.0_wp, 1e-6_wp, "Should have 6 axis points")
        
        ! Check projection worked
        call assert_true(abs(axis_lines_2d(1,1)) < 10.0_wp, "Projected coordinates should be reasonable")
    end subroutine

    subroutine test_3d_tick_positions()
        real(wp) :: tick_3d(3,10), tick_2d(2,10)
        real(wp) :: azim, elev, dist
        integer :: n_ticks
        
        call get_default_view_angles(azim, elev, dist)
        call create_3d_tick_positions(-1.0_wp, 1.0_wp, -1.0_wp, 1.0_wp, -1.0_wp, 1.0_wp, &
                                    tick_3d, n_ticks)
        
        call assert_true(n_ticks > 0, "Should have some tick marks")
        call assert_true(n_ticks <= 10, "Should not exceed array size")
    end subroutine

end program test_3d_axes