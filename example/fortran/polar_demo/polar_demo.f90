program polar_demo
    !! Demonstrates polar plot styling with color, linestyle, and marker support
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure, polar, title, legend, savefig_with_status
    use fortplot_errors, only: SUCCESS
    implicit none

    call render_polar_examples()

contains

    subroutine render_polar_examples()
        integer, parameter :: n = 360
        real(wp) :: theta(n), radius_primary(n), radius_secondary(n)
        integer :: i, status
        logical :: ok

        do i = 1, n
            theta(i) = 2.0_wp * acos(-1.0_wp) * real(i - 1, wp) / real(n, wp)
            radius_primary(i) = 1.0_wp + 0.3_wp * sin(4.0_wp * theta(i))
            radius_secondary(i) = 0.6_wp + 0.2_wp * cos(3.0_wp * theta(i))
        end do

        call figure()
        call polar(theta, radius_primary, fmt='r-', label='primary rose', &
                   linestyle='--', marker='o', color='red')
        call polar(theta, radius_secondary, label='secondary petals', &
                   linestyle='-.', marker='s', color='#1f77b4')
        call title('polar_demo: custom colors, markers, and linestyles')
        call legend()

        ok = .true.
        call savefig_with_status('output/example/fortran/polar_demo/' // &
                                 'polar_demo.png', status)
        if (status /= SUCCESS) ok = .false.
        call savefig_with_status('output/example/fortran/polar_demo/' // &
                                 'polar_demo.txt', status)
        if (status /= SUCCESS) ok = .false.
        if (.not. ok) then
            print *, 'WARNING: polar_demo failed to write one or more outputs'
        end if
    end subroutine render_polar_examples

end program polar_demo
