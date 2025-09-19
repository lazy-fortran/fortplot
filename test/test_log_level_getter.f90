program test_log_level_getter
    !! Verify get_log_level() reflects the current logging level
    !! and stays consistent across transitions.

    use fortplot, only: set_log_level, get_log_level, &
                        LOG_LEVEL_SILENT, LOG_LEVEL_ERROR, &
                        LOG_LEVEL_WARNING, LOG_LEVEL_INFO, LOG_LEVEL_DEBUG
    implicit none

    call ensure(get_log_level() == LOG_LEVEL_WARNING, 'default level is WARNING')

    call set_log_level(LOG_LEVEL_SILENT)
    call ensure(get_log_level() == LOG_LEVEL_SILENT, 'level SILENT after set')

    call set_log_level(LOG_LEVEL_ERROR)
    call ensure(get_log_level() == LOG_LEVEL_ERROR, 'level ERROR after set')

    call set_log_level(LOG_LEVEL_INFO)
    call ensure(get_log_level() == LOG_LEVEL_INFO, 'level INFO after set')

    call set_log_level(LOG_LEVEL_DEBUG)
    call ensure(get_log_level() == LOG_LEVEL_DEBUG, 'level DEBUG after set')

    ! Reset to default for other tests
    call set_log_level(LOG_LEVEL_WARNING)
    call ensure(get_log_level() == LOG_LEVEL_WARNING, 'level reset to WARNING')

contains

    subroutine ensure(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        if (.not. cond) then
            print *, 'ASSERTION FAILED:', trim(msg)
            stop 1
        end if
    end subroutine ensure

end program test_log_level_getter

