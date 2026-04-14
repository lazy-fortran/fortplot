program test_animation_clear_regression
    use fortplot
    use fortplot_animation
    use fortplot_system_runtime, only: create_directory_runtime, delete_file_runtime
    implicit none

    integer, parameter :: nframes = 4
    type(figure_t), pointer :: pfig
    type(animation_t) :: anim
    real(wp) :: x(64), y(64)
    integer :: i, status
    logical :: ok
    character(len=*), parameter :: output_file = "test/output/test_animation_clear_regression.txt"

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

    call assert_file_contains(output_file, "=== Frame 4/4 ===")
    call delete_file_runtime(output_file, ok)

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

    subroutine assert_file_contains(filename, needle)
        character(len=*), intent(in) :: filename, needle
        integer :: unit, ios
        character(len=2048) :: line
        logical :: found

        found = .false.
        open(newunit=unit, file=filename, status="old", action="read", iostat=ios)
        if (ios /= 0) error stop "failed to open regression animation output"

        do
            read(unit, "(A)", iostat=ios) line
            if (ios /= 0) exit
            if (index(line, needle) > 0) then
                found = .true.
                exit
            end if
        end do
        close(unit)

        if (.not. found) error stop "regression animation output missing expected frame marker"
    end subroutine assert_file_contains

end program test_animation_clear_regression
