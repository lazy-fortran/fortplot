program fortplot_play_ascii
    !! Compatibility alias for fortplot_play_text.
    !!
    !! The .txt renderer is the text backend; ASCII is its default charset.
    !! This target stays available for existing users and forwards to the
    !! shared text-player CLI driver.
    !!
    !! Usage: fortplot_play_ascii <file.txt> [--fps N] [--loop] [--no-clear]
    use fortplot_text_player_cli, only: run_text_player_cli
    implicit none

    integer :: exit_code

    call run_text_player_cli("fortplot_play_ascii", exit_code)
    if (exit_code /= 0) stop exit_code
end program fortplot_play_ascii
