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
    public :: projected_box_metrics, map_projected_to_axes

    real(wp), parameter :: PI = 3.14159265358979323846_wp
    ! Fraction of the data window the projected cube fills. matplotlib mplot3d
    ! leaves a margin around the projected box; this matches that framing while
    ! preserving the projected aspect ratio.
    real(wp), parameter :: BOX_FILL_FRACTION = 0.9_wp

    type, public :: projected_axes_map_t
        !! Aspect-preserving mapping from projected 2D coordinates into the data
        !! window. A single shared pixel scale keeps the projected box from being
        !! independently stretched on x and y, so the cube stays tilted with the
        !! correct proportions in every backend.
        real(wp) :: proj_cx = 0.0_wp, proj_cy = 0.0_wp  ! projected box center
        real(wp) :: data_cx = 0.0_wp, data_cy = 0.0_wp  ! data window center
        real(wp) :: scale_x = 1.0_wp, scale_y = 1.0_wp  ! data units per proj unit
    end type projected_axes_map_t

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

    subroutine projected_box_metrics(azim, elev, dist, x_min, x_max, y_min, &
                                     y_max, width_px_per_data, height_px_per_data, &
                                     map)
        !! Build the aspect-preserving projection map for one 3D axes box.
        !!
        !! Projects the unit cube, then chooses a single pixel scale so the
        !! projected box fits the data window in pixel space (the window spans
        !! width_px_per_data*range_x by height_px_per_data*range_y pixels) while
        !! preserving the projected aspect ratio and centering the box.
        real(wp), intent(in) :: azim, elev, dist
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(in) :: width_px_per_data, height_px_per_data
        type(projected_axes_map_t), intent(out) :: map

        real(wp) :: x_cube(8), y_cube(8), z_cube(8)
        real(wp) :: x_proj(8), y_proj(8)
        real(wp) :: proj_x_min, proj_x_max, proj_y_min, proj_y_max
        real(wp) :: proj_x_span, proj_y_span
        real(wp) :: range_x, range_y, win_w_px, win_h_px, scale_px

        x_cube = [0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, &
                  0.0_wp]
        y_cube = [0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, &
                  1.0_wp]
        z_cube = [0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, &
                  1.0_wp]
        call project_3d_to_2d(x_cube, y_cube, z_cube, azim, elev, dist, &
                              x_proj, y_proj)

        proj_x_min = minval(x_proj)
        proj_x_max = maxval(x_proj)
        proj_y_min = minval(y_proj)
        proj_y_max = maxval(y_proj)
        proj_x_span = max(1.0e-12_wp, proj_x_max - proj_x_min)
        proj_y_span = max(1.0e-12_wp, proj_y_max - proj_y_min)

        map%proj_cx = 0.5_wp*(proj_x_min + proj_x_max)
        map%proj_cy = 0.5_wp*(proj_y_min + proj_y_max)
        map%data_cx = 0.5_wp*(x_min + x_max)
        map%data_cy = 0.5_wp*(y_min + y_max)

        range_x = max(1.0e-12_wp, x_max - x_min)
        range_y = max(1.0e-12_wp, y_max - y_min)
        win_w_px = max(1.0e-12_wp, abs(width_px_per_data)*range_x)
        win_h_px = max(1.0e-12_wp, abs(height_px_per_data)*range_y)

        ! Single pixel scale that fits the projected box in both directions,
        ! preserving the projected aspect ratio.
        scale_px = BOX_FILL_FRACTION*min(win_w_px/proj_x_span, &
                                         win_h_px/proj_y_span)

        ! Convert that shared pixel scale back into per-axis data units so the
        ! backend pixel transform reproduces the chosen pixel scale exactly.
        map%scale_x = scale_px/max(1.0e-12_wp, abs(width_px_per_data))
        map%scale_y = scale_px/max(1.0e-12_wp, abs(height_px_per_data))
    end subroutine projected_box_metrics

    elemental subroutine map_projected_to_axes(map, x_proj, y_proj, x_out, y_out)
        !! Apply an aspect-preserving projection map to one projected point.
        type(projected_axes_map_t), intent(in) :: map
        real(wp), intent(in) :: x_proj, y_proj
        real(wp), intent(out) :: x_out, y_out

        x_out = map%data_cx + (x_proj - map%proj_cx)*map%scale_x
        y_out = map%data_cy + (y_proj - map%proj_cy)*map%scale_y
    end subroutine map_projected_to_axes

end module fortplot_projection
