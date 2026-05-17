program test_surface_plot_storage
    !! Verify that surface plots are stored with proper metadata
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t, PLOT_TYPE_SURFACE
    implicit none

    type(figure_t) :: fig
    real(wp) :: x(3), y(2), z(2,3)
    real(wp) :: edge_color(3)

    call fig%initialize()

    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.5_wp]
    z = reshape([0.0_wp, 0.5_wp, 1.0_wp, 1.5_wp, 2.0_wp, 2.5_wp], shape(z))
    edge_color = [0.9_wp, 0.1_wp, 0.2_wp]

    call fig%add_surface(x, y, z, label='demo surface', edgecolor=edge_color, &
                         linewidth=2.5_wp, alpha=0.6_wp)

    if (fig%plot_count /= 1) error stop 'Surface plot did not increment plot_count'
    if (fig%plots(1)%plot_type /= PLOT_TYPE_SURFACE) error stop 'Surface plot not tagged correctly'

    if (.not. allocated(fig%plots(1)%x_grid)) error stop 'x_grid not stored'
    if (.not. allocated(fig%plots(1)%y_grid)) error stop 'y_grid not stored'
    if (.not. allocated(fig%plots(1)%z_grid)) error stop 'z_grid not stored'

    if (any(abs(fig%plots(1)%x_grid - x) > 1.0e-12_wp)) error stop 'x_grid mismatch'
    if (any(abs(fig%plots(1)%y_grid - y) > 1.0e-12_wp)) error stop 'y_grid mismatch'
    if (any(abs(fig%plots(1)%z_grid - z) > 1.0e-12_wp)) error stop 'z_grid mismatch'

    if (.not. allocated(fig%plots(1)%label)) error stop 'label not stored'
    if (fig%plots(1)%surface_linewidth <= 0.0_wp) error stop 'linewidth not stored'

    if (any(abs(fig%plots(1)%surface_edgecolor - edge_color) > 1.0e-12_wp)) &
        error stop 'edgecolor not preserved'
    if (abs(fig%plots(1)%surface_alpha - 0.6_wp) > 1.0e-12_wp) &
        error stop 'alpha not preserved'
    if (fig%plots(1)%surface_filled) &
        error stop 'surface_filled should be false by default'

    call fig%initialize()
    call fig%add_surface(x, y, z, colormap='viridis', filled=.true.)
    if (.not. fig%plots(1)%surface_filled) &
        error stop 'surface_filled not set to true'
    if (.not. fig%plots(1)%surface_use_colormap) &
        error stop 'surface_use_colormap not set'
    if (.not. allocated(fig%plots(1)%surface_colormap)) &
        error stop 'surface_colormap not allocated'
    if (trim(fig%plots(1)%surface_colormap) /= 'viridis') &
        error stop 'surface_colormap not viridis'

    print *, 'PASS: surface plot stored with 3D metadata'
end program test_surface_plot_storage
