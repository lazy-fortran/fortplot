program probability_animation_demo
    use fortplot
    use fortplot_animation
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: nframes = 36
    type(figure_t), pointer :: pfig
    type(animation_t) :: anim
    real(wp) :: x(240), y(240)
    integer :: i
    logical :: ok

    do i = 1, size(x)
        x(i) = -6.0_wp + 12.0_wp * real(i - 1, wp) / real(size(x) - 1, wp)
    end do
    y = gaussian_profile(0.55_wp)

    call create_directory_runtime("output/example/fortran/probability_animation_demo", ok)
    if (.not. ok) then
        error stop "failed to create probability_animation_demo output directory"
    end if

    pfig => get_global_figure()
    call figure(figsize=[8.0_wp, 4.8_wp])
    pfig => get_global_figure()
    call draw_initial_frame(y)
    anim = FuncAnimation(update_gaussian, frames=nframes, interval=60, fig=pfig)

    call save_animation(anim, "output/example/fortran/probability_animation_demo/melting_gaussian.mp4", fps=18)
    call save_animation(anim, "output/example/fortran/probability_animation_demo/melting_gaussian.txt")

contains

    subroutine update_gaussian(frame)
        integer, intent(in) :: frame
        real(wp) :: sigma

        sigma = 0.55_wp + 1.55_wp * real(frame - 1, wp) / real(nframes - 1, wp)
        y = gaussian_profile(sigma)
        call redraw_current_frame(y)
        call pfig%set_rendered(.false.)
    end subroutine update_gaussian

    subroutine draw_initial_frame(yvals)
        real(wp), intent(in) :: yvals(:)
        call add_plot(x, yvals)
        call title("Gaussian with increasing sigma")
        call xlabel("x")
        call ylabel("density")
        call xlim(-6.0_wp, 6.0_wp)
        call ylim(0.0_wp, 0.75_wp)
    end subroutine draw_initial_frame

    subroutine redraw_current_frame(yvals)
        real(wp), intent(in) :: yvals(:)

        call pfig%clear()
        call add_plot(x, yvals)
        call title("Gaussian with increasing sigma")
        call xlabel("x")
        call ylabel("density")
        call xlim(-6.0_wp, 6.0_wp)
        call ylim(0.0_wp, 0.75_wp)
    end subroutine redraw_current_frame

    function gaussian_profile(sigma) result(vals)
        real(wp), intent(in) :: sigma
        real(wp) :: vals(size(x))

        vals = exp(-0.5_wp * (x / sigma)**2) / (sigma * sqrt(2.0_wp * acos(-1.0_wp)))
    end function gaussian_profile

end program probability_animation_demo
