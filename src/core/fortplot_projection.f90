module fortplot_projection
    !! 3D to 2D projection module for rendering 3D plots in 2D backends
    !! 
    !! Based on matplotlib's implementation with default viewing angles:
    !! - azimuth: -60 degrees
    !! - elevation: 30 degrees  
    !! - distance: 10 units
    !! - perspective projection with focal_length = 1
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: project_3d_to_2d, get_default_view_angles, create_projection_matrix
    
    real(wp), parameter :: PI = 3.14159265358979323846_wp
    
contains

    subroutine get_default_view_angles(azim, elev, dist)
        !! Get default viewing angles matching matplotlib
        real(wp), intent(out) :: azim, elev, dist
        
        azim = -60.0_wp * PI / 180.0_wp  ! -60 degrees
        elev = 30.0_wp * PI / 180.0_wp   ! 30 degrees  
        dist = 10.0_wp                   ! 10 units from origin
    end subroutine get_default_view_angles
    
    subroutine create_projection_matrix(azim, elev, dist, proj_matrix)
        !! Create 4x4 projection matrix for 3D to 2D transformation
        real(wp), intent(in) :: azim, elev, dist
        real(wp), intent(out) :: proj_matrix(4,4)
        
        real(wp) :: cos_azim, sin_azim, cos_elev, sin_elev
        real(wp) :: rotation(3,3)
        
        ! Initialize matrix
        proj_matrix = 0.0_wp
        
        ! Calculate trig functions
        cos_azim = cos(azim)
        sin_azim = sin(azim)
        cos_elev = cos(elev)
        sin_elev = sin(elev)
        
        ! Create rotation matrix (simplified approach)
        ! First rotate around z-axis (azimuth), then around x-axis (elevation)
        rotation(1,1) = cos_azim
        rotation(1,2) = sin_azim * sin_elev
        rotation(1,3) = sin_azim * cos_elev
        
        rotation(2,1) = -sin_azim
        rotation(2,2) = cos_azim * sin_elev
        rotation(2,3) = cos_azim * cos_elev
        
        rotation(3,1) = 0.0_wp
        rotation(3,2) = -cos_elev
        rotation(3,3) = sin_elev
        
        ! Build 4x4 projection matrix
        proj_matrix(1:3,1:3) = rotation
        proj_matrix(4,4) = 1.0_wp
        
        ! No perspective for now - orthographic projection
        ! This simplifies the math and avoids the scaling issues
    end subroutine create_projection_matrix

    subroutine project_3d_to_2d(x3d, y3d, z3d, azim, elev, dist, x2d, y2d)
        !! Project 3D coordinates to 2D using orthographic projection
        real(wp), intent(in) :: x3d(:), y3d(:), z3d(:)
        real(wp), intent(in) :: azim, elev, dist
        real(wp), intent(out) :: x2d(size(x3d)), y2d(size(x3d))
        
        real(wp) :: cos_azim, sin_azim, cos_elev, sin_elev
        real(wp) :: x_rot, y_rot, z_rot
        integer :: i, n
        
        n = size(x3d)
        
        ! Calculate trig functions
        cos_azim = cos(azim)
        sin_azim = sin(azim)
        cos_elev = cos(elev)
        sin_elev = sin(elev)
        
        ! Transform each point using simplified rotation
        do i = 1, n
            ! Rotate around z-axis (azimuth)
            x_rot = x3d(i) * cos_azim - y3d(i) * sin_azim
            y_rot = x3d(i) * sin_azim + y3d(i) * cos_azim
            z_rot = z3d(i)
            
            ! Rotate around x-axis (elevation)
            ! Project to 2D (drop the rotated z component)
            x2d(i) = x_rot
            y2d(i) = y_rot * sin_elev + z_rot * cos_elev
        end do
    end subroutine project_3d_to_2d

    subroutine matrix_multiply(A, B, C)
        !! Multiply two 4x4 matrices: C = A * B
        real(wp), intent(in) :: A(4,4), B(4,4)
        real(wp), intent(out) :: C(4,4)
        integer :: i, j, k
        
        do i = 1, 4
            do j = 1, 4
                C(i,j) = 0.0_wp
                do k = 1, 4
                    C(i,j) = C(i,j) + A(i,k) * B(k,j)
                end do
            end do
        end do
    end subroutine matrix_multiply

    subroutine matrix_vector_multiply(A, v, result)
        !! Multiply 4x4 matrix by 4-element vector: result = A * v
        real(wp), intent(in) :: A(4,4), v(4)
        real(wp), intent(out) :: result(4)
        integer :: i, j
        
        do i = 1, 4
            result(i) = 0.0_wp
            do j = 1, 4
                result(i) = result(i) + A(i,j) * v(j)
            end do
        end do
    end subroutine matrix_vector_multiply

end module fortplot_projection