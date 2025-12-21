module fortplot_contour_level_calculation
    !! Default contour level generation compatible with matplotlib MaxNLocator

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: compute_default_contour_levels

contains

    subroutine compute_default_contour_levels(z_min, z_max, levels)
        real(wp), intent(in) :: z_min, z_max
        real(wp), allocatable, intent(out) :: levels(:)

        integer, parameter :: N_STEPS = 10
        real(wp), parameter :: steps(N_STEPS) = [ &
                               1.0_wp, 1.5_wp, 2.0_wp, 2.5_wp, 3.0_wp, 4.0_wp, &
                               5.0_wp, 6.0_wp, &
                               8.0_wp, 10.0_wp]

        real(wp), parameter :: threshold = 100.0_wp
        integer, parameter :: nbins = 8

        real(wp) :: vmin, vmax
        real(wp) :: dv, meanv
        real(wp) :: scale, offset
        real(wp) :: vmin_s, vmax_s

        real(wp) :: ext_steps(2*N_STEPS)
        real(wp) :: steps_scaled(2*N_STEPS)
        real(wp) :: raw_step
        integer :: istep
        real(wp) :: step

        real(wp) :: best_vmin
        integer :: low, high
        integer :: i, n
        real(wp), allocatable :: tmp(:)

        vmin = min(z_min, z_max)
        vmax = max(z_min, z_max)
        dv = abs(vmax - vmin)

        if (dv <= 0.0_wp) then
            allocate (levels(1))
            levels(1) = vmin
            return
        end if

        meanv = 0.5_wp*(vmax + vmin)

        if (abs(meanv)/dv < threshold) then
            offset = 0.0_wp
        else
            offset = sign(10.0_wp**floor(log10(abs(meanv))), meanv)
        end if

        scale = 10.0_wp**floor(log10(dv/real(nbins, wp)))

        vmin_s = vmin - offset
        vmax_s = vmax - offset

        call build_extended_steps(steps, ext_steps)
        steps_scaled = ext_steps*scale

        raw_step = (vmax_s - vmin_s)/real(nbins, wp)

        istep = size(steps_scaled)
        do i = 1, size(steps_scaled)
            if (steps_scaled(i) >= raw_step) then
                istep = i
                exit
            end if
        end do

        step = steps_scaled(istep)
        best_vmin = floor(vmin_s/step)*step

        low = edge_le(vmin_s - best_vmin, step, offset)
        high = edge_ge(vmax_s - best_vmin, step, offset)

        n = max(0, high - low + 1)
        if (n < 3) then
            allocate (levels(3))
            levels = [vmin, 0.5_wp*(vmin + vmax), vmax]
            return
        end if

        allocate (tmp(n))
        do i = 1, n
            tmp(i) = real(low + i - 1, wp)*step + best_vmin + offset
        end do

        call move_alloc(tmp, levels)

    end subroutine compute_default_contour_levels

    subroutine build_extended_steps(steps, extended)
        real(wp), intent(in) :: steps(:)
        real(wp), intent(out) :: extended(:)

        integer :: n, i

        n = size(steps)
        if (size(extended) /= 2*n) then
            error stop
        end if

        do i = 1, n - 1
            extended(i) = 0.1_wp*steps(i)
        end do
        do i = 1, n
            extended(n - 1 + i) = steps(i)
        end do
        extended(2*n) = 10.0_wp*steps(2)
    end subroutine build_extended_steps

    pure integer function edge_le(x, step, offset) result(n)
        real(wp), intent(in) :: x, step, offset
        real(wp) :: d, m

        d = floor(x/step)
        m = x - d*step
        if (closeto(m/step, 1.0_wp, step, offset)) then
            n = int(d) + 1
        else
            n = int(d)
        end if
    end function edge_le

    pure integer function edge_ge(x, step, offset) result(n)
        real(wp), intent(in) :: x, step, offset
        real(wp) :: d, m

        d = floor(x/step)
        m = x - d*step
        if (closeto(m/step, 0.0_wp, step, offset)) then
            n = int(d)
        else
            n = int(d) + 1
        end if
    end function edge_ge

    pure logical function closeto(ms, edge, step, offset) result(is_close)
        real(wp), intent(in) :: ms, edge, step, offset
        real(wp) :: tol, digits

        if (abs(offset) > 0.0_wp) then
            digits = log10(abs(offset)/step)
            tol = max(1.0e-10_wp, 10.0_wp**(digits - 12.0_wp))
            tol = min(0.4999_wp, tol)
        else
            tol = 1.0e-10_wp
        end if

        is_close = abs(ms - edge) < tol
    end function closeto

end module fortplot_contour_level_calculation

