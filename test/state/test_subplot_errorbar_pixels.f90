program test_subplot_errorbar_pixels
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, subplot, plot, errorbar, add_errorbar, xlim, &
                        ylim, legend, savefig, get_global_figure, figure_t
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none
    integer, parameter :: width=800, height=480
    real(wp) :: rgb(width,height,3)
    type(figure_t), pointer :: fig
    logical :: ok
    integer :: blue_left, blue_right
    call create_directory_runtime('build/test/output/',ok)
    if (.not.ok) error stop 'fixture output directory'
    call figure(figsize=[8.0_wp,4.8_wp])
    call subplot(1,2,1)
    call plot([-1.0_wp,1.0_wp],[0.0_wp,0.0_wp],color=[0.4_wp,0.4_wp,0.4_wp])
    call xlim(-2.0_wp,2.0_wp); call ylim(-2.0_wp,2.0_wp)
    call subplot(1,2,2)
    call errorbar([-0.5_wp],[0.0_wp],yerr=[0.6_wp],fmt='none', &
        color=[0.0_wp,0.0_wp,1.0_wp],ecolor=[0.0_wp,0.0_wp,1.0_wp], &
        label='first uncertainty',capsize=4.0_wp)
    call add_errorbar([0.5_wp],[0.0_wp],color='blue',yerr=[0.6_wp], &
        fmt='none',label='second uncertainty',capsize=4)
    call xlim(-2.0_wp,2.0_wp); call ylim(-2.0_wp,2.0_wp)
    call legend()
    fig=>get_global_figure()
    call savefig('build/test/output/subplot_errorbar_pixels.png')
    call fig%extract_rgb_data_for_animation(rgb)
    blue_left=count(rgb(1:width/2,height/3:2*height/3,3)- &
        rgb(1:width/2,height/3:2*height/3,1)>0.4_wp)
    blue_right=count(rgb(width/2+1:width,height/3:2*height/3,3)- &
        rgb(width/2+1:width,height/3:2*height/3,1)>0.4_wp)
    if(blue_right<60) error stop 'selected subplot lost uncertainty bars'
    if(blue_left/=0) error stop 'uncertainty bars leaked to neighbor'
    if(fig%subplot_plot_count(1,2)/=2) error stop 'public uncertainty series missing'
    if(fig%subplots_array(1,2)%plots(1)%label/='first uncertainty') &
        error stop 'public uncertainty legend label missing'
    call savefig('build/test/output/subplot_errorbar_pixels.pdf')
    print '(a)','PASS selected subplot uncertainty pixels, isolation and labels'
end program
