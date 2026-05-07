program fortplot_play_ascii
    !! CLI player for .txt animations produced by save_animation.
    !!
    !! Usage: fortplot_play_ascii <file.txt> [--fps N] [--loop] [--no-clear]
    use iso_fortran_env, only: output_unit, error_unit
    use fortplot_ascii_player, only: play_ascii_animation, ascii_player_options_t, &
        ASCII_PLAYER_DEFAULT_FPS
    implicit none

    character(len=4096) :: arg
    character(len=4096) :: filename
    type(ascii_player_options_t) :: opts
    integer :: i, nargs, status, ios

    filename = ""
    opts%fps = ASCII_PLAYER_DEFAULT_FPS
    opts%loop = .false.
    opts%clear_screen = .true.
    opts%dry_run = .false.
    opts%max_loops = 1

    nargs = command_argument_count()
    if (nargs < 1) then
        call print_usage()
        stop 2
    end if

    i = 1
    do while (i <= nargs)
        call get_command_argument(i, arg)
        select case (trim(arg))
        case ("-h", "--help")
            call print_usage()
            stop 0
        case ("--fps")
            if (i + 1 > nargs) call die("--fps requires a value")
            i = i + 1
            call get_command_argument(i, arg)
            read(arg, *, iostat=ios) opts%fps
            if (ios /= 0 .or. opts%fps <= 0) call die("invalid --fps value")
        case ("--loop")
            opts%loop = .true.
            opts%max_loops = 0
        case ("--no-clear")
            opts%clear_screen = .false.
        case ("--once")
            opts%loop = .false.
        case ("--dry-run")
            opts%dry_run = .true.
            opts%clear_screen = .false.
        case default
            if (len_trim(filename) == 0) then
                filename = arg
            else
                call die("unexpected argument: " // trim(arg))
            end if
        end select
        i = i + 1
    end do

    if (len_trim(filename) == 0) then
        call print_usage()
        stop 2
    end if

    call play_ascii_animation(trim(filename), opts, status=status)
    if (status /= 0) stop 1

contains

    subroutine print_usage()
        write(output_unit, '(A)') &
            "Usage: fortplot_play_ascii <file.txt> [--fps N] [--loop] [--once] [--no-clear] [--dry-run]"
        write(output_unit, '(A)') &
            "  Plays an ASCII animation produced by save_animation('*.txt')."
        write(output_unit, '(A)') &
            "  --fps N      target frames per second (default 10, must be > 0)"
        write(output_unit, '(A)') &
            "  --loop       loop forever (Ctrl-C to stop)"
        write(output_unit, '(A)') &
            "  --once       play once (default)"
        write(output_unit, '(A)') &
            "  --no-clear   do not emit ANSI clear-screen between frames"
        write(output_unit, '(A)') &
            "  --dry-run    parse and count frames without printing or sleeping"
    end subroutine print_usage

    subroutine die(msg)
        character(len=*), intent(in) :: msg
        write(error_unit, '(A)') "fortplot_play_ascii: " // msg
        write(error_unit, '(A)') "Try --help."
        stop 2
    end subroutine die
end program fortplot_play_ascii
