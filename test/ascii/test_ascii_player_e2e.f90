program test_ascii_player_e2e
    !! End-to-end: save_animation('*.txt') then play with the player.
    use fortplot
    use fortplot_animation
    use fortplot_ascii_player, only: ascii_player_options_t, play_ascii_animation, &
        count_animation_frames
    use fortplot_system_runtime, only: create_directory_runtime, delete_file_runtime
    implicit none

    integer, parameter :: NFRAMES = 3
    character(len=*), parameter :: OUTDIR = "build/test/output"
    character(len=*), parameter :: ANIM_FILE = "build/test/output/test_ascii_player_e2e.txt"
    type(figure_t), pointer :: pfig
    type(animation_t) :: anim
    real(wp) :: x(20), y(20)
    type(ascii_player_options_t) :: opts
    integer :: i, status, n_frames_in_file, played
    logical :: ok

    do i = 1, size(x)
        x(i) = real(i - 1, wp)
        y(i) = sin(x(i) * 0.5_wp)
    end do

    call create_directory_runtime(OUTDIR, ok)
    if (.not. ok) error stop "could not create build/test/output"

    call figure(figsize=[6.0_wp, 4.0_wp])
    pfig => get_global_figure()
    call add_plot(x, y)
    call title("e2e player test")

    anim = FuncAnimation(update, frames=NFRAMES, interval=10, fig=pfig)
    call save_animation(anim, ANIM_FILE, status=status)
    if (status /= 0) then
        print *, "save_animation status=", status
        error stop "save_animation .txt failed"
    end if

    call count_animation_frames(ANIM_FILE, n_frames_in_file)
    if (n_frames_in_file /= NFRAMES) then
        print *, "expected ", NFRAMES, " frames, got ", n_frames_in_file
        error stop "frame count mismatch in produced file"
    end if

    opts%dry_run = .true.
    opts%clear_screen = .false.
    opts%fps = 100
    call play_ascii_animation(ANIM_FILE, opts, status=status, frames_played=played)
    if (status /= 0) error stop "player rejected real .txt animation"
    if (played /= NFRAMES) then
        print *, "player played ", played, " expected ", NFRAMES
        error stop "player played wrong number of frames"
    end if

    call delete_file_runtime(ANIM_FILE, ok)
    print *, "PASS test_ascii_player_e2e"

contains

    subroutine update(frame)
        integer, intent(in) :: frame
        real(wp) :: phase
        phase = real(frame, wp) * 0.4_wp
        y = sin(x * 0.5_wp + phase)
        call pfig%clear()
        call add_plot(x, y)
        call title("e2e player test")
        call pfig%set_rendered(.false.)
    end subroutine update
end program test_ascii_player_e2e
