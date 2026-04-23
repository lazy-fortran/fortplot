program test_matplotlib_api_alignment
    !! Regression tests for matplotlib-compatible API alignment
    !!
    !! Covers issues #1654, #1655, #1656, #1657, #1663, #1665, #1666, #1670, #1698.
    !!
    !! Each block exercises a facade change so the signature, default, alias,
    !! or silent-acceptance contract cannot regress without the test failing.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_matplotlib, only: figure, plot, legend, grid, subplots, &
                                   set_xscale, set_yscale, xscale, yscale, &
                                   contour, contourf, contour_filled, &
                                   add_contourf, add_contour, add_contour_filled, &
                                   pcolormesh, add_pcolormesh, add_surface, &
                                   axhline, axvline, hlines, vlines, &
                                   get_global_figure
    use fortplot_figure_core, only: figure_t

    implicit none

    call check_figure_default_figsize()
    call check_plot_kwargs()
    call check_reflines_facade()
    call check_contourf_alias()
    call check_scale_aliases()
    call check_legend_kwargs()
    call check_subplots_returns_axes()
    call check_cmap_aliases()
    call check_grid_visible_alias()

    print *, "All matplotlib API alignment tests passed."

contains

    subroutine check_figure_default_figsize()
        !! Issue #1655: default figsize must match matplotlib's (6.4, 4.8)
        type(figure_t), pointer :: f

        call figure()
        f => get_global_figure()
        if (f%get_width() /= 640 .or. f%get_height() /= 480) then
            print *, "FAIL: default figsize pixels expected 640x480, got ", &
                f%get_width(), "x", f%get_height()
            stop 1
        end if
    end subroutine check_figure_default_figsize

    subroutine check_plot_kwargs()
        !! Issue #1654: plot() must accept color, linewidth, marker, markersize
        real(wp) :: x(3), y(3)
        real(wp) :: custom_color(3)
        integer :: idx

        x = [0.0_wp, 1.0_wp, 2.0_wp]
        y = [0.0_wp, 1.0_wp, 0.0_wp]
        custom_color = [0.75_wp, 0.25_wp, 0.10_wp]

        call figure()
        call plot(x, y, label="line", linestyle='-', color=custom_color, &
                  linewidth=2.5_wp, marker='o', markersize=7.0_wp)

        if (.not. allocated(fig%plots)) then
            print *, "FAIL: plot() did not allocate plot storage"
            stop 1
        end if

        idx = fig%plot_count
        if (any(abs(fig%plots(idx)%color - custom_color) > 1.0e-12_wp)) then
            print *, "FAIL: plot() did not store custom color"
            stop 1
        end if
        if (abs(fig%plots(idx)%line_width - 2.5_wp) > 1.0e-12_wp) then
            print *, "FAIL: plot() did not store linewidth"
            stop 1
        end if
        if (.not. allocated(fig%plots(idx)%marker)) then
            print *, "FAIL: plot() did not store marker"
            stop 1
        end if
        if (trim(fig%plots(idx)%marker) /= 'o') then
            print *, "FAIL: plot() marker mismatch"
            stop 1
        end if
        if (abs(fig%plots(idx)%scatter_size_default - 7.0_wp) > 1.0e-12_wp) then
            print *, "FAIL: plot() did not store markersize"
            stop 1
        end if
    end subroutine check_plot_kwargs

    subroutine check_reflines_facade()
        !! Issue #1656: axhline/axvline/hlines/vlines wired into facade
        real(wp) :: y_positions(3)
        integer :: baseline

        call figure()
        baseline = fig%plot_count

        call axhline(1.0_wp, color='red', linestyle='--', label='hline')
        if (fig%plot_count /= baseline + 1) then
            print *, "FAIL: axhline did not add a plot"
            stop 1
        end if

        call axvline(2.0_wp, color='blue', linestyle=':', label='vline')
        if (fig%plot_count /= baseline + 2) then
            print *, "FAIL: axvline did not add a plot"
            stop 1
        end if

        y_positions = [0.0_wp, 0.5_wp, 1.0_wp]
        call hlines(y_positions, xmin=0.0_wp, xmax=3.0_wp, colors='green')
        if (fig%plot_count < baseline + 3) then
            print *, "FAIL: hlines did not add plots"
            stop 1
        end if

        call vlines(y_positions, ymin=0.0_wp, ymax=3.0_wp, colors='orange')
        if (fig%plot_count < baseline + 4) then
            print *, "FAIL: vlines did not add plots"
            stop 1
        end if
    end subroutine check_reflines_facade

    subroutine check_contourf_alias()
        !! Issue #1657: contourf must be a working matplotlib alias
        real(wp) :: x(4), y(4), z(4, 4)
        integer :: baseline, i, j

        do i = 1, 4
            x(i) = real(i - 1, wp)
            y(i) = real(i - 1, wp)
        end do
        do j = 1, 4
            do i = 1, 4
                z(i, j) = real(i + j, wp)
            end do
        end do

        call figure()
        baseline = fig%plot_count
        call contourf(x, y, z)
        if (fig%plot_count /= baseline + 1) then
            print *, "FAIL: contourf did not add a plot"
            stop 1
        end if

        ! Backward-compatible name
        call contour_filled(x, y, z)
        if (fig%plot_count /= baseline + 2) then
            print *, "FAIL: contour_filled alias regressed"
            stop 1
        end if

        call add_contourf(x, y, z)
        if (fig%plot_count /= baseline + 3) then
            print *, "FAIL: add_contourf did not add a plot"
            stop 1
        end if
    end subroutine check_contourf_alias

    subroutine check_scale_aliases()
        !! Issue #1663: xscale/yscale aliases must work with linthresh
        call figure()
        call xscale('log')
        call yscale('log')
        call set_xscale('symlog', linthresh=0.5_wp)
        call set_yscale('symlog', linthresh=0.25_wp)
        ! Backward-compatible threshold keyword still honoured
        call set_xscale('symlog', threshold=1.0_wp)
        call set_yscale('symlog', threshold=1.0_wp)
    end subroutine check_scale_aliases

    subroutine check_legend_kwargs()
        !! Issue #1665: legend accepts loc and silently accepts box/fontsize
        real(wp) :: x(2), y(2)

        x = [0.0_wp, 1.0_wp]
        y = [0.0_wp, 1.0_wp]

        call figure()
        call plot(x, y, label='line')
        call legend(loc='upper right')
        call legend(loc='upper right', box=.true., fontsize=12)
        ! Deprecated alias should still function
        call legend(position='lower left')
    end subroutine check_legend_kwargs

    subroutine check_subplots_returns_axes()
        !! Issue #1666: subplots must return a 2D index matrix when requested
        integer, allocatable :: axes(:,:)
        integer :: expected, i, j

        call figure()
        call subplots(2, 3, axes=axes)

        if (.not. allocated(axes)) then
            print *, "FAIL: subplots did not allocate axes"
            stop 1
        end if
        if (size(axes, 1) /= 2 .or. size(axes, 2) /= 3) then
            print *, "FAIL: subplots axes shape mismatch"
            stop 1
        end if

        do i = 1, 2
            do j = 1, 3
                expected = (i - 1) * 3 + j
                if (axes(i, j) /= expected) then
                    print *, "FAIL: subplots axes indexing mismatch at ", i, j
                    stop 1
                end if
            end do
        end do

        ! Backward-compatible parameterless form must still work
        call subplots(1, 1)
    end subroutine check_subplots_returns_axes

    subroutine check_cmap_aliases()
        !! Issue #1670: cmap canonical, colormap accepted as alias
        real(wp) :: x(4), y(4), z(4, 4)
        integer :: i, j

        do i = 1, 4
            x(i) = real(i - 1, wp)
            y(i) = real(i - 1, wp)
        end do
        do j = 1, 4
            do i = 1, 4
                z(i, j) = real(i * j, wp)
            end do
        end do

        call figure()
        call contour(x, y, z, cmap='viridis')
        call contourf(x, y, z, cmap='plasma')
        call pcolormesh(x, y, z, cmap='inferno')
        call add_pcolormesh(x, y, z, cmap='viridis')
        call add_contour(x, y, z, cmap='viridis')
        call add_contour_filled(x, y, z, cmap='viridis')
        call add_surface(x, y, z, cmap='viridis')

        ! Backward-compatible colormap alias still accepted
        call figure()
        call contourf(x, y, z, colormap='viridis')
        call pcolormesh(x, y, z, colormap='viridis')
    end subroutine check_cmap_aliases

    subroutine check_grid_visible_alias()
        !! Issue #1698: grid accepts visible (canonical) and enabled (deprecated)
        call figure()
        call grid(visible=.true.)
        call grid(visible=.false.)
        ! Deprecated alias still accepted
        call grid(enabled=.true.)
        ! Positional logical must still work (backward compat)
        call grid(.true.)
        ! Keyword-only styling arguments without visible flag
        call grid(which='major', axis='x')
    end subroutine check_grid_visible_alias

end program test_matplotlib_api_alignment
