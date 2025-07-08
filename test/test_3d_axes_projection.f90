program test_3d_axes_projection
    !! Test 3D axes projection and rendering
    use iso_fortran_env, only: wp => real64
    use fortplot_3d_axes
    use fortplot_projection
    implicit none
    
    call test_axis_corners_creation()
    call test_projection_transformation()
    call test_axis_lines_creation()
    call test_proper_3d_box_structure()
    
    print *, "All 3D axes projection tests passed!"
    
contains

    subroutine test_axis_corners_creation()
        real(wp) :: corners_3d(3,8)
        real(wp) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        x_min = 0.0_wp; x_max = 1.0_wp
        y_min = 0.0_wp; y_max = 1.0_wp
        z_min = 0.0_wp; z_max = 1.0_wp
        
        call create_3d_axis_corners(x_min, x_max, y_min, y_max, z_min, z_max, corners_3d)
        
        ! Check corner 1: (x_min, y_min, z_min)
        if (abs(corners_3d(1,1) - x_min) > 1e-10_wp) error stop "Corner 1 x wrong"
        if (abs(corners_3d(2,1) - y_min) > 1e-10_wp) error stop "Corner 1 y wrong"
        if (abs(corners_3d(3,1) - z_min) > 1e-10_wp) error stop "Corner 1 z wrong"
        
        ! Check corner 7: (x_max, y_max, z_max)
        if (abs(corners_3d(1,7) - x_max) > 1e-10_wp) error stop "Corner 7 x wrong"
        if (abs(corners_3d(2,7) - y_max) > 1e-10_wp) error stop "Corner 7 y wrong"
        if (abs(corners_3d(3,7) - z_max) > 1e-10_wp) error stop "Corner 7 z wrong"
        
        print *, "✓ Axis corners creation test passed"
    end subroutine test_axis_corners_creation
    
    subroutine test_projection_transformation()
        real(wp) :: x3d(1), y3d(1), z3d(1), x2d(1), y2d(1)
        real(wp) :: azim, elev, dist
        
        ! Test projection of origin
        x3d(1) = 0.0_wp
        y3d(1) = 0.0_wp
        z3d(1) = 0.0_wp
        
        call get_default_view_angles(azim, elev, dist)
        call project_3d_to_2d(x3d, y3d, z3d, azim, elev, dist, x2d, y2d)
        
        ! Origin should project to (0, 0) in default view
        if (abs(x2d(1)) > 0.1_wp) error stop "Origin x projection wrong"
        if (abs(y2d(1)) > 0.1_wp) error stop "Origin y projection wrong"
        
        print *, "✓ Projection transformation test passed"
    end subroutine test_projection_transformation
    
    subroutine test_axis_lines_creation()
        real(wp) :: axis_lines_3d(3,6)
        real(wp) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        x_min = 0.0_wp; x_max = 1.0_wp
        y_min = 0.0_wp; y_max = 1.0_wp
        z_min = 0.0_wp; z_max = 1.0_wp
        
        call create_3d_axis_lines(x_min, x_max, y_min, y_max, z_min, z_max, axis_lines_3d)
        
        ! Check X-axis line start and end
        if (abs(axis_lines_3d(1,1) - x_min) > 1e-10_wp) error stop "X-axis start x wrong"
        if (abs(axis_lines_3d(1,2) - x_max) > 1e-10_wp) error stop "X-axis end x wrong"
        if (abs(axis_lines_3d(2,1) - y_min) > 1e-10_wp) error stop "X-axis start y wrong"
        if (abs(axis_lines_3d(3,1) - z_min) > 1e-10_wp) error stop "X-axis start z wrong"
        
        ! Check Y-axis line
        if (abs(axis_lines_3d(2,4) - y_max) > 1e-10_wp) error stop "Y-axis end y wrong"
        
        ! Check Z-axis line
        if (abs(axis_lines_3d(3,6) - z_max) > 1e-10_wp) error stop "Z-axis end z wrong"
        
        print *, "✓ Axis lines creation test passed"
    end subroutine test_axis_lines_creation
    
    subroutine test_proper_3d_box_structure()
        !! Test that we create a proper 3D bounding box, not just axes
        real(wp) :: corners_3d(3,8), corners_2d(2,8)
        real(wp) :: azim, elev, dist
        real(wp) :: x_min, x_max, y_min, y_max, z_min, z_max
        integer :: i
        
        x_min = -1.0_wp; x_max = 1.0_wp
        y_min = -1.0_wp; y_max = 1.0_wp
        z_min = -1.0_wp; z_max = 1.0_wp
        
        call create_3d_axis_corners(x_min, x_max, y_min, y_max, z_min, z_max, corners_3d)
        call get_default_view_angles(azim, elev, dist)
        call project_3d_corners_to_2d(corners_3d, azim, elev, dist, corners_2d)
        
        ! Check that projected corners form a reasonable shape
        ! With default view angles, we should see separation in x and y
        if (maxval(corners_2d(1,:)) - minval(corners_2d(1,:)) < 1.0_wp) then
            error stop "Projected box has insufficient x spread"
        end if
        if (maxval(corners_2d(2,:)) - minval(corners_2d(2,:)) < 1.0_wp) then
            error stop "Projected box has insufficient y spread"
        end if
        
        print *, "✓ Proper 3D box structure test passed"
    end subroutine test_proper_3d_box_structure
    
end program test_3d_axes_projection