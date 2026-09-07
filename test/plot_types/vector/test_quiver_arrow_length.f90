program test_quiver_arrow_length
    !! Explicit scale is inverse, and UV direction is measured on screen.
    !! Emit polygons and backend artifacts for the actual Matplotlib oracle.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, quiver, xlabel, ylabel, title, savefig, &
                        xlim, ylim, set_xscale
    use fortplot_plot_data, only: plot_data_t
    use fortplot_quiver_geometry, only: prepare_quiver_geometry, quiver_arrow_vertices
    use fortplot_test_helpers, only: test_initialize_environment, test_get_temp_path
    implicit none
    integer, parameter :: n = 100
    type(plot_data_t) :: plot
    real(wp) :: x(n), y(n), u(n), v(n), lengths(n), angles(n), first_lengths(n)
    real(wp) :: width, vx(8), vy(8), bounds(4), canvas(2), scales(9), opacity
    integer :: i, j, k, case_id, unit
    character(len=1) :: tag
    character(len=6) :: axis_scale
    character(len=:), allocatable :: prefix

    call test_initialize_environment('quiver_matplotlib')
    prefix = test_get_temp_path('quiver_oracle_')
    k = 0
    do j = 1, 10
        do i = 1, 10
            k = k + 1
            x(k) = -2.0_wp + 4.0_wp*real(i - 1, wp)/9.0_wp
            y(k) = -2.0_wp + 4.0_wp*real(j - 1, wp)/9.0_wp
        end do
    end do
    u = -y
    v = x
    u(n) = 0.0_wp
    v(n) = 0.0_wp
    plot%x = x
    plot%y = y
    plot%quiver_u = u
    plot%quiver_v = v
    bounds = [-2.2_wp, 2.2_wp, -2.2_wp, 2.2_wp]
    canvas = [620.0_wp, 462.0_wp]
    scales = [0.0_wp, 35.0_wp, 70.0_wp, 35.0_wp, 1.0_wp, 35.0_wp, &
              35.0_wp, 1.0_wp, 1.0_wp]
    open (newunit=unit, file=prefix//'vertices.dat', status='replace')
    do case_id = 1, 9
        plot%x = x
        plot%y = y
        bounds = [-2.2_wp, 2.2_wp, -2.2_wp, 2.2_wp]
        axis_scale = 'linear'
        opacity = 1.0_wp
        if (case_id == 7) opacity = 0.45_wp
        plot%quiver_scale = scales(case_id)
        plot%quiver_width = 0.0_wp
        plot%quiver_headwidth = 3.0_wp
        plot%quiver_headlength = 5.0_wp
        plot%quiver_pivot = 'tail'
        plot%quiver_units = 'width'
        plot%quiver_scale_units = ''
        plot%quiver_angles = 'uv'
        select case (case_id)
        case (4)
            plot%quiver_width = 0.01_wp
            plot%quiver_headwidth = 5.0_wp
            plot%quiver_headlength = 7.0_wp
            plot%quiver_pivot = 'middle'
        case (5)
            plot%quiver_angles = 'xy'
            plot%quiver_scale_units = 'xy'
        case (6)
            plot%quiver_width = 2.0_wp
            plot%quiver_units = 'dots'
            plot%quiver_angles = 'xy'
            plot%quiver_pivot = 'tip'
        case (8, 9)
            plot%x = 10.0_wp**((x + 2.0_wp)/2.0_wp)
            plot%y = y + 2.0_wp
            bounds = [0.0_wp, 2.0_wp, 0.0_wp, 10.0_wp]
            axis_scale = 'log'
            plot%quiver_units = 'x'
            if (case_id == 9) plot%quiver_units = 'xy'
            plot%quiver_width = 0.2_wp
        end select
        call prepare_quiver_geometry(plot, bounds, canvas, 100.0_wp, &
                                     axis_scale, 'linear', 1.0_wp, &
                                     lengths, angles, width)
        if (case_id == 2) first_lengths = lengths
        if (case_id == 3) then
            if (maxval(abs(lengths - first_lengths/2.0_wp)) > 1.0e-12_wp) &
                error stop 'doubling quiver scale must halve displayed lengths'
        end if
        if (case_id <= 4) then
            if (abs(angles(1) + acos(-1.0_wp)/4.0_wp) > 1.0e-12_wp) &
                error stop 'uv direction must remain minus 45 degrees'
        end if
        do i = 1, n
            call quiver_arrow_vertices(lengths(i), angles(i), width, &
                                       plot%quiver_headwidth, plot%quiver_headlength, &
                                       plot%quiver_pivot, vx, vy)
            do j = 1, 8
                write (unit, '(3(I0,1X),2(ES24.16,1X))') case_id, i, j, vx(j), vy(j)
            end do
        end do
        write (tag, '(I1)') case_id
        call figure(figsize=[8.0_wp, 6.0_wp])
        if (case_id == 1) then
            call quiver(x, y, u, v, color=[31.0_wp, 119.0_wp, 180.0_wp]/255.0_wp)
        else
            call quiver(plot%x, plot%y, u, v, scale=plot%quiver_scale, &
                        width=plot%quiver_width, headwidth=plot%quiver_headwidth, &
                        headlength=plot%quiver_headlength, units=plot%quiver_units, &
                        angles=plot%quiver_angles, &
                        scale_units=plot%quiver_scale_units, &
                        pivot=plot%quiver_pivot, alpha=opacity, &
                        color=[31.0_wp, 119.0_wp, 180.0_wp]/255.0_wp)
        end if
        if (case_id >= 8) then
            call set_xscale('log')
            call xlim(1.0_wp, 100.0_wp)
            call ylim(0.0_wp, 10.0_wp)
        end if
        call xlabel('X')
        call ylabel('Y')
        call title('Matplotlib quiver oracle')
        call savefig(prefix//tag//'.png')
        call savefig(prefix//tag//'.pdf')
        call savefig(prefix//tag//'.svg')
    end do
    close (unit)
    print *, 'PASS: quiver inverse scale and screen-space direction'
end program test_quiver_arrow_length
