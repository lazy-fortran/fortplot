program test_clear_raster_pixels
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, subplot, plot, title, xlabel, ylabel, xlim, ylim, &
                        savefig, get_global_figure, figure_t, tight_layout
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none
    integer, parameter :: width = 640, height = 480
    type(figure_t), pointer :: fig
    real(wp) :: rgb(width, height, 3), first_blue(width, height, 3)
    logical :: ok
    integer :: panel, red_pixels, blue_pixels, frame

    call create_directory_runtime('build/test/output/', ok)
    if (.not. ok) error stop 'test directory creation failed'
    call figure(figsize=[6.4_wp, 4.8_wp])
    fig => get_global_figure()
    do panel = 1, 2
        call subplot(1, 2, panel)
        call plot([0.0_wp, 1.0_wp], [0.25_wp, 0.25_wp], &
                  color=[0.9_wp, 0.0_wp, 0.0_wp])
        call title('old red line')
        call xlabel('old x'); call ylabel('old y')
        call xlim(0.0_wp, 1.0_wp); call ylim(0.0_wp, 1.0_wp)
    end do
    call tight_layout()
    call savefig('build/test/output/clear_raster_before.png')
    call fig%extract_rgb_data_for_animation(rgb)
    red_pixels = count(rgb(:, :, 1) - rgb(:, :, 3) > 0.4_wp)
    if (red_pixels < 100) error stop 'initial red line fixture not visible'

    do frame = 1, 12
        call fig%clear()
        do panel = 1, 2
            call subplot(1, 2, panel)
            call plot([0.0_wp, 1.0_wp], [10.75_wp, 10.75_wp], &
                      color=[0.0_wp, 0.4_wp, 0.9_wp])
            call title('new blue line')
            call xlabel('new x'); call ylabel('new y')
            call xlim(0.0_wp, 1.0_wp); call ylim(10.0_wp, 11.0_wp)
        end do
        call tight_layout()
        call savefig('build/test/output/clear_raster_after.png')
        call fig%extract_rgb_data_for_animation(rgb)
        red_pixels = count(rgb(:, :, 1) - rgb(:, :, 3) > 0.4_wp)
        blue_pixels = count(rgb(:, :, 3) - rgb(:, :, 1) > 0.4_wp)
        if (red_pixels /= 0) error stop 'clear retained old red raster pixels'
        if (blue_pixels < 100) error stop 'clear lost new blue raster pixels'
        if (frame == 1) first_blue = rgb
        if (maxval(abs(rgb - first_blue)) > 0.0_wp) &
            error stop 'clear and tight_layout geometry changed on identical redraw'
    end do
    print '(a)', 'Clear removes old raster pixels and retains new subplot drawing'
end program test_clear_raster_pixels
