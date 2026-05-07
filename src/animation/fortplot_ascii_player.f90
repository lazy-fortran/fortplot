module fortplot_ascii_player
    !! Plays back ASCII animations produced by save_animation(*.txt).
    !!
    !! The on-disk format is a sequence of frames separated by a header
    !! line of the form "=== Frame <N> ===" followed by the ASCII rendering
    !! of the figure. The player reads each frame, prints it to a chosen
    !! output unit, and sleeps for 1000/fps milliseconds between frames.
    use iso_fortran_env, only: output_unit, error_unit, int64
    implicit none
    private

    public :: play_ascii_animation
    public :: ascii_player_options_t
    public :: count_animation_frames
    public :: parse_frame_header
    public :: max_player_line_length
    public :: ASCII_PLAYER_MAX_LINE
    public :: ASCII_PLAYER_DEFAULT_FPS

    integer, parameter :: ASCII_PLAYER_MAX_LINE = 1024
    integer, parameter :: ASCII_PLAYER_DEFAULT_FPS = 10
    character(len=*), parameter :: FRAME_HEADER_PREFIX = "=== Frame "
    character(len=*), parameter :: FRAME_HEADER_SUFFIX = " ==="
    character(len=*), parameter :: ANSI_CLEAR = char(27) // "[2J" // char(27) // "[H"

    type :: ascii_player_options_t
        integer :: fps = ASCII_PLAYER_DEFAULT_FPS
        logical :: loop = .false.
        logical :: clear_screen = .true.
        logical :: dry_run = .false.
        integer :: max_loops = 1
    end type ascii_player_options_t

contains

    pure function max_player_line_length() result(n)
        integer :: n
        n = ASCII_PLAYER_MAX_LINE
    end function max_player_line_length

    subroutine play_ascii_animation(filename, options, out_unit, status, frames_played)
        !! Open a .txt animation and play frames at options%fps.
        character(len=*), intent(in) :: filename
        type(ascii_player_options_t), intent(in) :: options
        integer, intent(in), optional :: out_unit
        integer, intent(out), optional :: status
        integer, intent(out), optional :: frames_played

        integer :: unit, ios, sink_unit, loops_done, frames_in_loop, played
        logical :: is_open
        character(len=:), allocatable :: pending_header
        character(len=ASCII_PLAYER_MAX_LINE) :: line_buffer
        integer :: line_len
        logical :: have_pending, eof
        integer :: header_index

        if (present(status)) status = 0
        if (present(frames_played)) frames_played = 0

        if (present(out_unit)) then
            sink_unit = out_unit
        else
            sink_unit = output_unit
        end if

        if (.not. file_exists(filename)) then
            if (present(status)) status = -1
            write(error_unit, '(A)') "fortplot_ascii_player: file not found: " // trim(filename)
            return
        end if

        if (options%fps <= 0) then
            if (present(status)) status = -2
            write(error_unit, '(A)') "fortplot_ascii_player: fps must be > 0"
            return
        end if

        played = 0
        loops_done = 0
        do
            open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
            if (ios /= 0) then
                if (present(status)) status = -3
                write(error_unit, '(A)') "fortplot_ascii_player: cannot open " // trim(filename)
                return
            end if

            have_pending = .false.
            pending_header = ""
            frames_in_loop = 0

            ! Advance to first frame header, ignoring any preamble lines.
            call advance_to_first_header(unit, pending_header, have_pending, eof)
            if (eof) then
                close(unit)
                if (frames_in_loop == 0 .and. loops_done == 0) then
                    if (present(status)) status = -4
                    write(error_unit, '(A)') &
                        "fortplot_ascii_player: no frames found in " // trim(filename)
                end if
                exit
            end if

            do
                call play_one_frame(unit, sink_unit, options, pending_header, have_pending, &
                                     line_buffer, line_len, eof, header_index)
                frames_in_loop = frames_in_loop + 1
                played = played + 1
                if (header_index <= 0) then
                    ! Reached end of file after writing the frame.
                    have_pending = .false.
                    exit
                end if
            end do

            close(unit)

            loops_done = loops_done + 1
            if (.not. options%loop) exit
            if (loops_done >= options%max_loops .and. options%max_loops > 0) exit
        end do

        if (present(frames_played)) frames_played = played
    end subroutine play_ascii_animation

    subroutine play_one_frame(unit, sink_unit, options, pending_header, have_pending, &
                               line_buffer, line_len, eof, header_index)
        integer, intent(in) :: unit, sink_unit
        type(ascii_player_options_t), intent(in) :: options
        character(len=:), allocatable, intent(inout) :: pending_header
        logical, intent(inout) :: have_pending
        character(len=*), intent(inout) :: line_buffer
        integer, intent(out) :: line_len, header_index
        logical, intent(out) :: eof

        integer :: ios

        if (.not. options%dry_run) then
            if (options%clear_screen) then
                write(sink_unit, '(A)', advance='no') ANSI_CLEAR
            end if
            if (have_pending) then
                write(sink_unit, '(A)') pending_header
            end if
        end if

        eof = .false.
        header_index = 0
        do
            read(unit, '(A)', iostat=ios) line_buffer
            if (ios /= 0) then
                eof = .true.
                exit
            end if
            line_len = len_trim(line_buffer)
            if (parse_frame_header(line_buffer(1:line_len), header_index)) then
                pending_header = line_buffer(1:line_len)
                have_pending = .true.
                exit
            end if
            if (.not. options%dry_run) then
                write(sink_unit, '(A)') line_buffer(1:line_len)
            end if
        end do

        if (.not. options%dry_run) then
            call sleep_ms(1000 / options%fps)
        end if
    end subroutine play_one_frame

    subroutine advance_to_first_header(unit, pending_header, have_pending, eof)
        integer, intent(in) :: unit
        character(len=:), allocatable, intent(out) :: pending_header
        logical, intent(out) :: have_pending, eof

        character(len=ASCII_PLAYER_MAX_LINE) :: line_buffer
        integer :: ios, idx

        have_pending = .false.
        eof = .false.
        do
            read(unit, '(A)', iostat=ios) line_buffer
            if (ios /= 0) then
                eof = .true.
                pending_header = ""
                return
            end if
            if (parse_frame_header(trim(line_buffer), idx)) then
                pending_header = trim(line_buffer)
                have_pending = .true.
                return
            end if
        end do
    end subroutine advance_to_first_header

    function parse_frame_header(line, frame_index) result(is_header)
        !! Recognize lines of the form "=== Frame <N> ===" and return N.
        character(len=*), intent(in) :: line
        integer, intent(out) :: frame_index
        logical :: is_header

        integer :: prefix_len, suffix_len, n_start, n_end, ios

        is_header = .false.
        frame_index = 0
        prefix_len = len(FRAME_HEADER_PREFIX)
        suffix_len = len(FRAME_HEADER_SUFFIX)

        if (len(line) < prefix_len + suffix_len + 1) return
        if (line(1:prefix_len) /= FRAME_HEADER_PREFIX) return
        if (line(len(line) - suffix_len + 1:) /= FRAME_HEADER_SUFFIX) return

        n_start = prefix_len + 1
        n_end = len(line) - suffix_len
        if (n_end < n_start) return

        read(line(n_start:n_end), *, iostat=ios) frame_index
        if (ios /= 0) then
            frame_index = 0
            return
        end if
        is_header = .true.
    end function parse_frame_header

    subroutine count_animation_frames(filename, n_frames, status)
        !! Count frames in a .txt animation without playing them.
        character(len=*), intent(in) :: filename
        integer, intent(out) :: n_frames
        integer, intent(out), optional :: status

        integer :: unit, ios, idx
        character(len=ASCII_PLAYER_MAX_LINE) :: line_buffer

        n_frames = 0
        if (present(status)) status = 0

        if (.not. file_exists(filename)) then
            if (present(status)) status = -1
            return
        end if

        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            if (present(status)) status = -2
            return
        end if

        do
            read(unit, '(A)', iostat=ios) line_buffer
            if (ios /= 0) exit
            if (parse_frame_header(trim(line_buffer), idx)) then
                n_frames = n_frames + 1
            end if
        end do
        close(unit)
    end subroutine count_animation_frames

    function file_exists(filename) result(exists)
        character(len=*), intent(in) :: filename
        logical :: exists
        inquire(file=filename, exist=exists)
    end function file_exists

    subroutine sleep_ms(milliseconds)
        !! Portable wall-clock delay using system_clock (POSIX-free, Windows-safe).
        integer, intent(in) :: milliseconds
        integer(int64) :: start_count, current_count, count_rate, target_ticks

        if (milliseconds <= 0) return
        call system_clock(start_count, count_rate)
        if (count_rate <= 0_int64) return
        target_ticks = (int(milliseconds, int64) * count_rate) / 1000_int64
        if (target_ticks <= 0_int64) target_ticks = 1_int64
        do
            call system_clock(current_count)
            if (current_count - start_count >= target_ticks) exit
        end do
    end subroutine sleep_ms

end module fortplot_ascii_player
