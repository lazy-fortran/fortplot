module fortplot_text_player_cli
    !! Shared command-line driver for the text-backend animation players.
    !!
    !! The text backend renders .txt animations (ASCII charset by default).
    !! Both the `fortplot_play_text` target and its `fortplot_play_ascii`
    !! compatibility alias route through run_text_player_cli so their argument
    !! parsing and behavior stay identical.
    use iso_fortran_env, only: output_unit, error_unit
    use fortplot_ascii_player, only: play_ascii_animation, &
        ascii_player_options_t, ASCII_PLAYER_DEFAULT_FPS
    implicit none

    private
    public :: run_text_player_cli

contains

    subroutine run_text_player_cli(program_name, exit_code)
        !! Parse arguments and play a .txt text-backend animation.
        !!
        !! program_name: name shown in usage/error output (invoked target).
        !! exit_code: process status to stop with (0 success, non-zero error).
        character(len=*), intent(in) :: program_name
        integer, intent(out) :: exit_code

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
            call print_usage(program_name)
            exit_code = 2
            return
        end if

        i = 1
        do while (i <= nargs)
            call get_command_argument(i, arg)
            select case (trim(arg))
            case ("-h", "--help")
                call print_usage(program_name)
                exit_code = 0
                return
            case ("--fps")
                if (i + 1 > nargs) then
                    call die(program_name, "--fps requires a value", exit_code)
                    return
                end if
                i = i + 1
                call get_command_argument(i, arg)
                read (arg, *, iostat=ios) opts%fps
                if (ios /= 0 .or. opts%fps <= 0) then
                    call die(program_name, "invalid --fps value", exit_code)
                    return
                end if
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
                    call die(program_name, "unexpected argument: "//trim(arg), &
                        exit_code)
                    return
                end if
            end select
            i = i + 1
        end do

        if (len_trim(filename) == 0) then
            call print_usage(program_name)
            exit_code = 2
            return
        end if

        call play_ascii_animation(trim(filename), opts, status=status)
        if (status /= 0) then
            exit_code = 1
        else
            exit_code = 0
        end if
    end subroutine run_text_player_cli

    subroutine print_usage(program_name)
        character(len=*), intent(in) :: program_name
        write (output_unit, '(A)') &
            "Usage: "//trim(program_name)// &
            " <file.txt> [--fps N] [--loop] [--once] [--no-clear] [--dry-run]"
        write (output_unit, '(A)') &
            "  Plays a text-backend animation produced by save_animation('*.txt')."
        write (output_unit, '(A)') &
            "  The text backend renders the ASCII charset by default."
        write (output_unit, '(A)') &
            "  --fps N      target frames per second (default 10, must be > 0)"
        write (output_unit, '(A)') &
            "  --loop       loop forever (Ctrl-C to stop)"
        write (output_unit, '(A)') &
            "  --once       play once (default)"
        write (output_unit, '(A)') &
            "  --no-clear   do not emit ANSI clear-screen between frames"
        write (output_unit, '(A)') &
            "  --dry-run    parse and count frames without printing or sleeping"
    end subroutine print_usage

    subroutine die(program_name, msg, exit_code)
        character(len=*), intent(in) :: program_name
        character(len=*), intent(in) :: msg
        integer, intent(out) :: exit_code
        write (error_unit, '(A)') trim(program_name)//": "//msg
        write (error_unit, '(A)') "Try --help."
        exit_code = 2
    end subroutine die

end module fortplot_text_player_cli
