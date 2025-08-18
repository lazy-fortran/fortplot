program test_projection
    use fortplot_projection
    use fortplot_testing
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp), parameter :: PI = 3.14159265358979323846_wp
    
    call test_projection_matrix_creation()
    call test_3d_to_2d_transformation()
    call test_default_view_angles()
    call test_coordinate_bounds()
    
    print *, "All projection tests passed!"
    
contains

    subroutine test_projection_matrix_creation()
        real(wp) :: proj_matrix(4,4)
        real(wp) :: azim, elev, dist
        
        ! Test with default matplotlib values
        azim = -60.0_wp * PI / 180.0_wp
        elev = 30.0_wp * PI / 180.0_wp
        dist = 10.0_wp
        
        call create_projection_matrix(azim, elev, dist, proj_matrix)
        
        ! Check matrix is not identity
        call assert_true(abs(proj_matrix(1,1) - 1.0_wp) > 0.01_wp, &
                        "Projection matrix should not be identity")
    end subroutine

    subroutine test_3d_to_2d_transformation()
        real(wp) :: x3d(3), y3d(3), z3d(3)
        real(wp) :: x2d(3), y2d(3)
        real(wp) :: azim, elev, dist
        
        ! Test points: origin and unit vectors
        x3d = [0.0_wp, 1.0_wp, 0.0_wp]
        y3d = [0.0_wp, 0.0_wp, 1.0_wp]
        z3d = [0.0_wp, 0.0_wp, 0.0_wp]
        
        ! Default view
        azim = -60.0_wp * PI / 180.0_wp
        elev = 30.0_wp * PI / 180.0_wp
        dist = 10.0_wp
        
        call project_3d_to_2d(x3d, y3d, z3d, azim, elev, dist, x2d, y2d)
        
        ! Debug output
        write(*, '(A, 3F10.6)') "X-axis point (1,0,0) -> ", x2d(2), y2d(2), 0.0_wp
        write(*, '(A, 3F10.6)') "Y-axis point (0,1,0) -> ", x2d(3), y2d(3), 0.0_wp
        
        ! Origin should stay at origin
        call assert_equals(x2d(1), 0.0_wp, 1e-6_wp, "Origin x should be 0")
        call assert_equals(y2d(1), 0.0_wp, 1e-6_wp, "Origin y should be 0")
        
        ! X-axis point should be transformed
        call assert_true(abs(x2d(2)) > 0.01_wp, "X-axis point should move")
    end subroutine

    subroutine test_default_view_angles()
        real(wp) :: azim, elev, dist
        
        call get_default_view_angles(azim, elev, dist)
        
        call assert_equals(azim, -60.0_wp * PI / 180.0_wp, 1e-6_wp, &
                          "Default azimuth should be -60 degrees")
        call assert_equals(elev, 30.0_wp * PI / 180.0_wp, 1e-6_wp, &
                          "Default elevation should be 30 degrees")
        call assert_equals(dist, 10.0_wp, 1e-6_wp, &
                          "Default distance should be 10")
    end subroutine
    
    subroutine test_coordinate_bounds()
        real(wp) :: x3d(100), y3d(100), z3d(100)
        real(wp) :: x2d(100), y2d(100)
        real(wp) :: x2d_min, x2d_max, y2d_min, y2d_max
        real(wp) :: azim, elev, dist
        integer :: i
        
        ! Create a cube of points
        do i = 1, 100
            x3d(i) = mod(i-1, 10) - 4.5_wp
            y3d(i) = mod((i-1)/10, 10) - 4.5_wp
            z3d(i) = (i-1) / 100.0_wp * 9.0_wp - 4.5_wp
        end do
        
        ! Project with default view
        call get_default_view_angles(azim, elev, dist)
        call project_3d_to_2d(x3d, y3d, z3d, azim, elev, dist, x2d, y2d)
        
        ! Check bounds are reasonable
        x2d_min = minval(x2d)
        x2d_max = maxval(x2d)
        y2d_min = minval(y2d)
        y2d_max = maxval(y2d)
        
        call assert_true(x2d_max > x2d_min, "X range should be positive")
        call assert_true(y2d_max > y2d_min, "Y range should be positive")
        call assert_true(abs(x2d_max - x2d_min) < 20.0_wp, "X range should be reasonable")
        call assert_true(abs(y2d_max - y2d_min) < 20.0_wp, "Y range should be reasonable")
    end subroutine

end program test_projection