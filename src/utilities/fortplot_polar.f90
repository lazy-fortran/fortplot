module fortplot_polar
    !! Polar coordinate transformation and utilities
    !!
    !! Provides coordinate transforms between polar (theta, r) and Cartesian (x, y)
    !! and utilities for polar plot rendering including angular tick generation.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: polar_to_cartesian, polar_to_cartesian_arrays
    public :: cartesian_to_polar, normalize_angle
    public :: compute_angular_ticks, compute_radial_ticks
    public :: format_angle_label

    real(wp), parameter, public :: PI = 3.14159265358979323846_wp
    real(wp), parameter, public :: TWO_PI = 6.28318530717958647693_wp
    real(wp), parameter, public :: DEG_TO_RAD = PI/180.0_wp
    real(wp), parameter, public :: RAD_TO_DEG = 180.0_wp/PI

contains

    pure subroutine polar_to_cartesian(theta, r, x, y, theta_offset, clockwise)
        !! Convert single polar coordinate to Cartesian
        !! theta: angle in radians
        !! r: radius
        !! theta_offset: angular offset (default: pi/2 = 90 deg, 0 at top)
        !! clockwise: if true, angles increase clockwise
        real(wp), intent(in) :: theta, r
        real(wp), intent(out) :: x, y
        real(wp), intent(in), optional :: theta_offset
        logical, intent(in), optional :: clockwise

        real(wp) :: effective_theta, offset
        logical :: cw

        offset = 0.5_wp*PI  ! Default: 0 deg at top (mathematical convention)
        if (present(theta_offset)) offset = theta_offset

        cw = .false.
        if (present(clockwise)) cw = clockwise

        if (cw) then
            effective_theta = offset - theta
        else
            effective_theta = offset + theta
        end if

        x = r*cos(effective_theta)
        y = r*sin(effective_theta)
    end subroutine polar_to_cartesian

    pure subroutine polar_to_cartesian_arrays(theta, r, x, y, theta_offset, clockwise)
        !! Convert arrays of polar coordinates to Cartesian
        real(wp), intent(in) :: theta(:), r(:)
        real(wp), intent(out) :: x(:), y(:)
        real(wp), intent(in), optional :: theta_offset
        logical, intent(in), optional :: clockwise

        integer :: i, n

        n = min(size(theta), size(r))
        do i = 1, n
            call polar_to_cartesian(theta(i), r(i), x(i), y(i), theta_offset, &
                                    clockwise)
        end do
    end subroutine polar_to_cartesian_arrays

    pure subroutine cartesian_to_polar(x, y, theta, r)
        !! Convert Cartesian to polar coordinates
        !! Returns theta in [0, 2*pi) range
        real(wp), intent(in) :: x, y
        real(wp), intent(out) :: theta, r

        r = sqrt(x*x + y*y)
        theta = atan2(y, x)
        if (theta < 0.0_wp) theta = theta + TWO_PI
    end subroutine cartesian_to_polar

    pure function normalize_angle(theta) result(normalized)
        !! Normalize angle to [0, 2*pi) range
        real(wp), intent(in) :: theta
        real(wp) :: normalized

        normalized = modulo(theta, TWO_PI)
    end function normalize_angle

    pure subroutine compute_angular_ticks(n_ticks, tick_angles, tick_labels)
        !! Compute angular tick positions and labels for polar plot
        !! Returns angles in radians and labels in degrees
        integer, intent(in) :: n_ticks
        real(wp), intent(out) :: tick_angles(:)
        character(len=*), intent(out) :: tick_labels(:)

        integer :: i, deg
        real(wp) :: step

        if (n_ticks <= 0) return

        step = TWO_PI/real(n_ticks, wp)
        do i = 1, min(n_ticks, size(tick_angles))
            tick_angles(i) = real(i - 1, wp)*step
            deg = nint(tick_angles(i)*RAD_TO_DEG)
            if (deg >= 360) deg = deg - 360
            tick_labels(i) = format_angle_label(deg)
        end do
    end subroutine compute_angular_ticks

    pure subroutine compute_radial_ticks(r_min, r_max, n_ticks, tick_values)
        !! Compute radial tick positions for polar plot
        real(wp), intent(in) :: r_min, r_max
        integer, intent(in) :: n_ticks
        real(wp), intent(out) :: tick_values(:)

        integer :: i
        real(wp) :: step

        if (n_ticks <= 0) return

        step = (r_max - r_min)/real(n_ticks, wp)
        do i = 1, min(n_ticks, size(tick_values))
            tick_values(i) = r_min + real(i, wp)*step
        end do
    end subroutine compute_radial_ticks

    pure function format_angle_label(degrees) result(label)
        !! Format angle in degrees as a label string
        integer, intent(in) :: degrees
        character(len=8) :: label

        write (label, '(I0, A)') degrees, ' deg'
    end function format_angle_label

end module fortplot_polar
