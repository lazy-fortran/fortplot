program test_streamline_integrator_resize
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_streamline_integrator, only: integration_params_t, dopri5_integrate
    implicit none

    type(integration_params_t) :: params
    real(wp), allocatable :: path_x(:), path_y(:), times(:)
    integer :: n_points, n_acc, n_rej, i
    logical :: success

    ! Configure params to force a resize during the loop
    params%rtol = 1.0e-6_wp
    params%atol = 1.0e-9_wp
    params%h_initial = 0.1_wp
    params%h_min = 1.0e-8_wp
    params%h_max = 0.1_wp
    params%max_steps = 2   ! Small preallocation so n_points > array_size triggers resize
    params%safety_factor = 0.9_wp
    params%max_factor = 5.0_wp
    params%min_factor = 0.2_wp

    call dopri5_integrate(0.0_wp, 0.0_wp, 0.0_wp, 0.25_wp, &
                          u_const, v_const, params, &
                          path_x, path_y, times, n_points, n_acc, n_rej, success)

    ! We expect at least 3 points (initial + two accepted steps),
    ! which forces a resize from initial size 2 to larger size.
    if (n_points < 3) then
        print *, 'ERROR: Expected at least 3 points, got', n_points
        stop 1
    end if

    if (size(path_x) /= n_points .or. size(path_y) /= n_points .or. size(times) /= n_points) then
        print *, 'ERROR: Trimmed arrays do not match n_points'
        stop 1
    end if

    ! Monotonic increase in x due to u=1, v=0
    if (any([(path_x(i) < path_x(i-1), i=2,n_points)])) then
        print *, 'ERROR: x not monotonic increasing after integration'
        stop 1
    end if

    ! No need to assert success here; with max_steps=2 and t_final=0.25, success may be false.

contains
    real(wp) function u_const(x, y) result(u)
        real(wp), intent(in) :: x, y
        u = 1.0_wp
    end function u_const

    real(wp) function v_const(x, y) result(v)
        real(wp), intent(in) :: x, y
        v = 0.0_wp
    end function v_const

end program test_streamline_integrator_resize
