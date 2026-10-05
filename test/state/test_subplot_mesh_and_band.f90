program test_subplot_mesh_and_band
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, subplot, pcolormesh, fill_between, plot, legend, &
                        figure_t, get_global_figure
    use fortplot_raster, only: raster_context
    implicit none
    integer, parameter :: w = 640, h = 480
    real(wp) :: x(3) = [0.0_wp, 0.5_wp, 1.0_wp]
    real(wp) :: y(3) = [0.0_wp, 0.5_wp, 1.0_wp]
    real(wp) :: z(2, 2) = 0.5_wp, lower(3) = 0.2_wp, upper(3) = 0.8_wp
    real(wp) :: rgb(w, h, 3)
    type(figure_t), pointer :: fig
    integer :: pixels
    call figure(figsize=[6.4_wp, 4.8_wp])
    call subplot(2, 1, 1)
    call pcolormesh(x, y, z, cmap='viridis', vmin=0.0_wp, vmax=1.0_wp)
    call subplot(2, 1, 2)
    call fill_between(x, lower, upper, color='lightblue')
    call plot(x, y, label='reference', color=[0.0_wp, 0.0_wp, 0.0_wp])
    call legend(loc='upper right')
    fig => get_global_figure()
    ! Exercise the animation capture route without savefig pre-rendering.
    call fig%setup_png_backend_for_animation()
    call fig%extract_rgb_data_for_animation(rgb)
    ! The interior of the first panel must contain a broad colored mesh,
    ! and the second panel a filled band, not just axes/line traces.
    pixels = count(rgb(180:400, 85:170, 2) - rgb(180:400, 85:170, 1) > 0.1_wp)
    if (pixels < 1000) error stop 'selected subplot mesh was not rendered'
    pixels = count(rgb(180:400, 290:390, 3) - rgb(180:400, 290:390, 1) > 0.05_wp)
    if (pixels < 1000) error stop 'selected subplot fill band was not rendered'
    ! A whole-figure legend requires positive, canvas-scale drawing geometry.
    ! Padding fractions must not be mistaken for right/top edge coordinates.
    select type (bk => fig%state%backend)
    class is (raster_context)
        if (bk%plot_area%width < 400) error stop 'legend frame width collapsed'
        if (bk%plot_area%height < 300) error stop 'legend frame height collapsed'
    class default
        error stop 'PNG reference backend unavailable'
    end select
    call fig%savefig('build/test/output/subplot_mesh_and_band.png')
    print '(a)', 'PASS selected subplot mesh/band pixels and legend geometry'
end program
