program test_3d_animation_precision_warning_2052
    !! Regression for issue #2052: the 3D animation demo's Lissajous curve is
    !! ordinary bounded data and must not be flagged as too close for reliable
    !! rendering. Genuinely collapsed coordinate arrays must still be flagged.
    use iso_fortran_env, only: wp => real64
    use fortplot_coordinate_validation, only: has_machine_precision_issues
    use fortplot_3d_plots, only: add_3d_plot
    use fortplot_figure_core, only: figure_t
    implicit none

    integer, parameter :: n_points = 200
    real(wp) :: xs(n_points), ys(n_points), zs(n_points)
    real(wp) :: base(8)
    type(figure_t) :: fig
    real(wp) :: t
    integer :: i

    call build_lissajous_curve(xs, ys, zs)

    if (has_machine_precision_issues(xs, ys)) then
        error stop 'Lissajous 3D curve wrongly flagged as precision-unreliable'
    end if

    call fig%initialize()
    call add_3d_plot(fig, xs, ys, zs, label='Lissajous 3D', linestyle='-')
    if (fig%plot_count /= 1) then
        error stop 'add_3d_plot did not store the Lissajous curve'
    end if

    do i = 1, size(base)
        base(i) = 1.0_wp + real(i - 1, wp) * epsilon(1.0_wp)
    end do
    if (.not. has_machine_precision_issues(base, base)) then
        error stop 'near-identical coordinate array not flagged as unreliable'
    end if

    print *, 'PASS: 3D animation precision warning regression (#2052)'

contains

    subroutine build_lissajous_curve(x, y, z)
        real(wp), intent(out) :: x(:), y(:), z(:)
        integer :: j

        do j = 1, size(x)
            t = real(j - 1, wp) * 2.0_wp * 3.141592653589793_wp &
                / real(size(x) - 1, wp)
            x(j) = sin(3.0_wp * t)
            y(j) = cos(2.0_wp * t)
            z(j) = sin(t) * cos(t)
        end do
    end subroutine build_lissajous_curve

end program test_3d_animation_precision_warning_2052
