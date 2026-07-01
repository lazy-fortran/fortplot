program fortplot_play_text
    !! CLI player for .txt text-backend animations produced by save_animation.
    !!
    !! Usage: fortplot_play_text <file.txt> [--fps N] [--loop] [--no-clear]
    use fortplot_text_player_cli, only: run_text_player_cli
    implicit none

    integer :: exit_code

    call run_text_player_cli("fortplot_play_text", exit_code)
    if (exit_code /= 0) stop exit_code
end program fortplot_play_text
