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
    public :: project_3d_to_2d, get_default_view_angles

    real(wp), parameter :: PI = 3.14159265358979323846_wp

contains

    subroutine get_default_view_angles(azim, elev, dist)
        !! Get default viewing angles matching matplotlib
        real(wp), intent(out) :: azim, elev, dist

        azim = -60.0_wp*PI/180.0_wp  ! -60 degrees
        elev = 30.0_wp*PI/180.0_wp   ! 30 degrees
        dist = 10.0_wp                   ! 10 units from origin
    end subroutine get_default_view_angles

    subroutine project_3d_to_2d(x3d, y3d, z3d, azim, elev, dist, x2d, y2d, depth)
        !! Project 3D coordinates to 2D using orthographic projection.
        !!
        !! The optional depth output is the camera-space depth (distance toward
        !! the viewer) under the same rotation used for the screen coordinates:
        !! larger values are closer to the front. Callers that omit depth are
        !! unaffected, so existing behavior is preserved.
        real(wp), contiguous, intent(in) :: x3d(:), y3d(:), z3d(:)
        real(wp), intent(in) :: azim, elev, dist
        real(wp), intent(out) :: x2d(size(x3d)), y2d(size(x3d))
        real(wp), intent(out), optional :: depth(size(x3d))

        real(wp) :: cos_azim, sin_azim, cos_elev, sin_elev
        real(wp) :: planar
        integer :: i, n

        n = size(x3d)

        ! Calculate trig functions
        cos_azim = cos(azim)
        sin_azim = sin(azim)
        cos_elev = cos(elev)
        sin_elev = sin(elev)

        ! matplotlib mplot3d orthographic view (z is up). Camera looks at the
        ! origin from azimuth `azim`, elevation `elev`; world z maps to
        ! screen-vertical with +cos(elev), so a surface peak points up.
        !   screen-right u = ( sin a, -cos a, 0)
        !   screen-up    v = (-cos a sin e, -sin a sin e, cos e)
        !   view dir     n = ( cos a cos e,  sin a cos e, sin e)  (toward viewer)
        do i = 1, n
            planar = x3d(i)*cos_azim + y3d(i)*sin_azim
            x2d(i) = x3d(i)*sin_azim - y3d(i)*cos_azim
            y2d(i) = -planar*sin_elev + z3d(i)*cos_elev
            ! Camera depth, larger = nearer the viewer (same convention as the
            ! surface renderer's mean_view_depth).
            if (present(depth)) depth(i) = planar*cos_elev + z3d(i)*sin_elev
        end do
    end subroutine project_3d_to_2d

end module fortplot_projection
