program test_animation_clear_regression
    !! Regression test for clear()-between-frames: the animation update
    !! callback must be able to clear() the figure and redraw on every
    !! frame without crashing. Uses save_frame_sequence directly to
    !! avoid depending on ffmpeg availability at test time.
    use fortplot
    use fortplot_animation
    use fortplot_system_runtime, only: create_directory_runtime, delete_file_runtime
    implicit none

    integer, parameter :: nframes = 4
    type(figure_t), pointer :: pfig
    type(animation_t) :: anim
    real(wp) :: x(64), y(64)
    integer :: i, frame_idx
    logical :: ok, exists
    character(len=*), parameter :: output_stem = "build/test/output/test_animation_clear_regression_frame_"
    character(len=256) :: frame_name

    do i = 1, size(x)
        x(i) = -4.0_wp + 8.0_wp * real(i - 1, wp) / real(size(x) - 1, wp)
    end do
    y = gaussian_profile(0.6_wp)

    call create_directory_runtime("build/test/output", ok)
    if (.not. ok) error stop "failed to create build/test/output"

    call figure(figsize=[6.0_wp, 3.0_wp])
    pfig => get_global_figure()
    call draw_initial_frame(y)

    anim = FuncAnimation(update_curve, frames=nframes, interval=20, fig=pfig)
    call anim%save_frame_sequence(output_stem)

    do frame_idx = 0, nframes - 1
        write(frame_name, '(a,i0,a)') output_stem, frame_idx, ".png"
        inquire(file=trim(frame_name), exist=exists)
        if (.not. exists) then
            print *, "FAIL: expected frame missing: ", trim(frame_name)
            error stop "animation clear regression produced no frame output"
        end if
        call delete_file_runtime(trim(frame_name), ok)
    end do

contains

    subroutine update_curve(frame)
        integer, intent(in) :: frame
        real(wp) :: sigma

        sigma = 0.6_wp + 0.8_wp * real(frame - 1, wp) / real(nframes - 1, wp)
        y = gaussian_profile(sigma)
        call redraw_frame(y)
        call pfig%set_rendered(.false.)
    end subroutine update_curve

    subroutine draw_initial_frame(yvals)
        real(wp), intent(in) :: yvals(:)
        call add_plot(x, yvals)
        call title("Animation clear regression")
        call xlabel("x")
        call ylabel("density")
        call xlim(-4.0_wp, 4.0_wp)
        call ylim(0.0_wp, 0.8_wp)
    end subroutine draw_initial_frame

    subroutine redraw_frame(yvals)
        real(wp), intent(in) :: yvals(:)

        call pfig%clear()
        call add_plot(x, yvals)
        call title("Animation clear regression")
        call xlabel("x")
        call ylabel("density")
        call xlim(-4.0_wp, 4.0_wp)
        call ylim(0.0_wp, 0.8_wp)
    end subroutine redraw_frame

    function gaussian_profile(sigma) result(vals)
        real(wp), intent(in) :: sigma
        real(wp) :: vals(size(x))

        vals = exp(-0.5_wp * (x / sigma)**2) / (sigma * sqrt(2.0_wp * acos(-1.0_wp)))
    end function gaussian_profile

end program test_animation_clear_regression
