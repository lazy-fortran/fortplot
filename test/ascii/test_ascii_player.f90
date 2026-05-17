program test_ascii_player
    !! Verify parse/count/play behavior of fortplot_ascii_player.
    !!
    !! Strategy: build a synthetic .txt animation with known frame
    !! contents, then drive the player through count_animation_frames and
    !! play_ascii_animation in dry-run + capture-to-file modes. We avoid
    !! the real sleep path (CI-safe) by setting fps high enough that the
    !! sleep is 0/1 ms or by running dry-run.
    use iso_fortran_env, only: iostat_end
    use fortplot_ascii_player, only: ascii_player_options_t, &
        play_ascii_animation, count_animation_frames, parse_frame_header, &
        ASCII_PLAYER_DEFAULT_FPS
    use fortplot_system_runtime, only: create_directory_runtime, delete_file_runtime
    implicit none

    integer, parameter :: NFRAMES_FIXTURE = 4
    character(len=*), parameter :: TMP_DIR = "build/test/output"
    character(len=*), parameter :: FIXTURE = "build/test/output/test_ascii_player_fixture.txt"
    character(len=*), parameter :: PLAY_OUT = "build/test/output/test_ascii_player_capture.txt"
    logical :: dir_ok, deleted

    call create_directory_runtime(TMP_DIR, dir_ok)
    if (.not. dir_ok) error stop "could not create build/test/output"

    call test_parse_frame_header_recognizes_well_formed_lines()
    call test_parse_frame_header_rejects_garbage()

    call write_fixture(FIXTURE, NFRAMES_FIXTURE)

    call test_count_animation_frames(FIXTURE, NFRAMES_FIXTURE)
    call test_play_dry_run_counts_frames(FIXTURE, NFRAMES_FIXTURE)
    call test_play_writes_all_frames_to_unit(FIXTURE, PLAY_OUT, NFRAMES_FIXTURE)
    call test_play_missing_file_returns_error()
    call test_play_rejects_zero_fps(FIXTURE)

    call delete_file_runtime(FIXTURE, deleted)
    call delete_file_runtime(PLAY_OUT, deleted)

    print *, "PASS test_ascii_player"

contains

    subroutine test_parse_frame_header_recognizes_well_formed_lines()
        integer :: idx
        logical :: ok
        ok = parse_frame_header("=== Frame 1 ===", idx)
        if (.not. ok) error stop "parse_frame_header missed '=== Frame 1 ==='"
        if (idx /= 1) error stop "parse_frame_header returned wrong index for frame 1"

        ok = parse_frame_header("=== Frame 42 ===", idx)
        if (.not. ok) error stop "parse_frame_header missed '=== Frame 42 ==='"
        if (idx /= 42) error stop "parse_frame_header returned wrong index for frame 42"
    end subroutine test_parse_frame_header_recognizes_well_formed_lines

    subroutine test_parse_frame_header_rejects_garbage()
        integer :: idx
        logical :: ok
        ok = parse_frame_header("Frame 1", idx)
        if (ok) error stop "parse_frame_header accepted bare 'Frame 1'"
        ok = parse_frame_header("=== Frame X ===", idx)
        if (ok) error stop "parse_frame_header accepted non-numeric frame number"
        ok = parse_frame_header("=== Frame ===", idx)
        if (ok) error stop "parse_frame_header accepted missing frame number"
        ok = parse_frame_header("", idx)
        if (ok) error stop "parse_frame_header accepted empty line"
        ok = parse_frame_header("=== Other 1 ===", idx)
        if (ok) error stop "parse_frame_header accepted non-Frame header"
    end subroutine test_parse_frame_header_rejects_garbage

    subroutine test_count_animation_frames(path, expected)
        character(len=*), intent(in) :: path
        integer, intent(in) :: expected
        integer :: n, status
        call count_animation_frames(path, n, status)
        if (status /= 0) error stop "count_animation_frames status non-zero"
        if (n /= expected) then
            print *, "expected ", expected, " frames, got ", n
            error stop "count_animation_frames wrong count"
        end if
    end subroutine test_count_animation_frames

    subroutine test_play_dry_run_counts_frames(path, expected)
        character(len=*), intent(in) :: path
        integer, intent(in) :: expected
        type(ascii_player_options_t) :: opts
        integer :: status, played
        opts%dry_run = .true.
        opts%clear_screen = .false.
        opts%fps = 1000   ! ignored on dry-run path
        call play_ascii_animation(path, opts, status=status, frames_played=played)
        if (status /= 0) then
            print *, "status=", status
            error stop "play dry-run returned non-zero"
        end if
        if (played /= expected) then
            print *, "played=", played, " expected=", expected
            error stop "play dry-run frames_played mismatch"
        end if
    end subroutine test_play_dry_run_counts_frames

    subroutine test_play_writes_all_frames_to_unit(path, capture_path, expected)
        character(len=*), intent(in) :: path, capture_path
        integer, intent(in) :: expected
        type(ascii_player_options_t) :: opts
        integer :: out_unit, ios, status, played, header_count, idx
        character(len=512) :: line

        opts%fps = 1000
        opts%loop = .false.
        opts%clear_screen = .false.
        opts%max_loops = 1

        open(newunit=out_unit, file=capture_path, status='replace', action='write', iostat=ios)
        if (ios /= 0) error stop "could not open capture file"
        call play_ascii_animation(path, opts, out_unit=out_unit, status=status, &
                                   frames_played=played)
        close(out_unit)
        if (status /= 0) error stop "play to file returned non-zero"
        if (played /= expected) error stop "play to file wrote wrong frame count"

        header_count = 0
        open(newunit=out_unit, file=capture_path, status='old', action='read', iostat=ios)
        if (ios /= 0) error stop "could not reopen capture file"
        do
            read(out_unit, '(A)', iostat=ios) line
            if (ios == iostat_end) exit
            if (ios /= 0) exit
            if (parse_frame_header(trim(line), idx)) header_count = header_count + 1
        end do
        close(out_unit)

        if (header_count /= expected) then
            print *, "captured headers=", header_count, " expected=", expected
            error stop "captured frame headers mismatch"
        end if
    end subroutine test_play_writes_all_frames_to_unit

    subroutine test_play_missing_file_returns_error()
        type(ascii_player_options_t) :: opts
        integer :: status, played
        opts%dry_run = .true.
        opts%fps = ASCII_PLAYER_DEFAULT_FPS
        call play_ascii_animation("/tmp/__fortplot_does_not_exist__.txt", opts, &
                                   status=status, frames_played=played)
        if (status == 0) error stop "play accepted missing file"
        if (played /= 0) error stop "play counted frames in missing file"
    end subroutine test_play_missing_file_returns_error

    subroutine test_play_rejects_zero_fps(path)
        character(len=*), intent(in) :: path
        type(ascii_player_options_t) :: opts
        integer :: status
        opts%dry_run = .false.
        opts%fps = 0
        call play_ascii_animation(path, opts, status=status)
        if (status == 0) error stop "play accepted fps=0"
    end subroutine test_play_rejects_zero_fps

    subroutine write_fixture(path, n_frames)
        character(len=*), intent(in) :: path
        integer, intent(in) :: n_frames
        integer :: unit, ios, k, line

        open(newunit=unit, file=path, status='replace', action='write', iostat=ios)
        if (ios /= 0) error stop "could not write fixture"

        do k = 1, n_frames
            write(unit, '(A,I0,A)') "=== Frame ", k, " ==="
            write(unit, '(A,I0)') "header line for frame ", k
            do line = 1, 3
                write(unit, '(A,I0,A,I0)') "data ", k, ":", line
            end do
            write(unit, '(A)') ""
        end do

        close(unit)
    end subroutine write_fixture

end program test_ascii_player
