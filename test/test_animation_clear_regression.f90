program test_animation_clear_regression
    !! Regression test for clear()-between-frames: FuncAnimation must keep
    !! producing rendered frames without crashing when the update callback
    !! clears the figure before drawing. Saves to .mp4 and accepts either
    !! an ffmpeg video or the PNG-sequence fallback as proof of success.
    use fortplot
    use fortplot_animation
    use fortplot_system_runtime, only: create_directory_runtime, delete_file_runtime
    implicit none

    integer, parameter :: nframes = 4
    type(figure_t), pointer :: pfig
    type(animation_t) :: anim
    real(wp) :: x(64), y(64)
    integer :: i, status, frame_idx
    integer(8) :: video_size
    logical :: ok, video_exists, fallback_ok
    character(len=*), parameter :: output_stem = "test/output/test_animation_clear_regression"
    character(len=*), parameter :: output_file = output_stem // ".mp4"
    character(len=64) :: frame_name

    do i = 1, size(x)
        x(i) = -4.0_wp + 8.0_wp * real(i - 1, wp) / real(size(x) - 1, wp)
    end do
    y = gaussian_profile(0.6_wp)

    call create_directory_runtime("test/output", ok)
    if (.not. ok) error stop "failed to create test/output"

    call figure(figsize=[6.0_wp, 3.0_wp])
    pfig => get_global_figure()
    call draw_initial_frame(y)

    anim = FuncAnimation(update_curve, frames=nframes, interval=20, fig=pfig)
    call save_animation(anim, output_file, status=status)
    if (status /= 0) error stop "animation clear regression save failed"

    ! Either ffmpeg produced the mp4 or the PNG sequence fallback wrote
    ! one png per frame; both outcomes prove all frames rendered.
    inquire(file=output_file, exist=video_exists, size=video_size)
    fallback_ok = .false.
    if (.not. video_exists .or. video_size <= 0) then
        fallback_ok = .true.
        do frame_idx = 1, nframes
            write(frame_name, '(a,"_frame_",i4.4,".png")') output_stem, frame_idx
            inquire(file=trim(frame_name), exist=ok)
            if (.not. ok) fallback_ok = .false.
        end do
    end if

    if (.not. (video_exists .and. video_size > 0) .and. .not. fallback_ok) then
        error stop "animation clear regression produced no frame output"
    end if

    if (video_exists) call delete_file_runtime(output_file, ok)
    do frame_idx = 1, nframes
        write(frame_name, '(a,"_frame_",i4.4,".png")') output_stem, frame_idx
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
