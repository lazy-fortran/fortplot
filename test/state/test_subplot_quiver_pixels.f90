program test_subplot_quiver_pixels
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, subplot, plot, quiver, xlim, ylim, savefig, &
                        get_global_figure, figure_t
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none
    integer, parameter :: width = 800, height = 480
    real(wp) :: rgb(width, height, 3)
    type(figure_t), pointer :: fig
    logical :: ok
    integer :: blue_left, blue_right
    call create_directory_runtime('build/test/output/', ok)
    if (.not. ok) error stop 'fixture directory creation failed'
    call figure(figsize=[8.0_wp, 4.8_wp])
    call subplot(1, 2, 1)
    call plot([-1.0_wp, 1.0_wp], [0.0_wp, 0.0_wp], color=[0.4_wp, 0.4_wp, 0.4_wp])
    call xlim(-3.5_wp, 3.5_wp); call ylim(-3.0_wp, 3.0_wp)
    call subplot(1, 2, 2)
    ! The same one-point xy vector contract used by the particle force demo.
    call quiver([-0.4_wp], [0.4_wp], [-0.184_wp], [0.191_wp], &
                color=[0.0_wp, 0.4_wp, 0.9_wp], scale=1.0_wp, angles='xy', &
                scale_units='xy', units='xy', width=0.025_wp, alpha=0.75_wp)
    call xlim(-3.5_wp, 3.5_wp); call ylim(-3.0_wp, 3.0_wp)
    fig => get_global_figure()
    call savefig('build/test/output/subplot_quiver_pixels.png')
    call fig%extract_rgb_data_for_animation(rgb)
    blue_left = count(rgb(1:width/2, :, 3) - rgb(1:width/2, :, 1) > 0.3_wp)
    blue_right = count(rgb(width/2 + 1:width, :, 3) - &
        rgb(width/2 + 1:width, :, 1) > 0.3_wp)
    if (blue_right < 8) error stop 'selected subplot lost one-point quiver arrow'
    if (blue_left /= 0) error stop 'quiver arrow leaked into neighboring subplot'
    print '(a)', 'Selected subplot quiver arrow pixels and isolation pass'
end program test_subplot_quiver_pixels
