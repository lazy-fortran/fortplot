program test_log_level_input_validation
    !! Validate set_log_level() clamps inputs to the supported range

    use fortplot, only: set_log_level, get_log_level, &
                        LOG_LEVEL_SILENT, LOG_LEVEL_WARNING, LOG_LEVEL_DEBUG
    implicit none

    ! Start from default (WARNING)
    call ensure(get_log_level() == LOG_LEVEL_WARNING, 'default level is WARNING')

    ! Below minimum -> clamp to SILENT
    call set_log_level(-999)
    call ensure(get_log_level() == LOG_LEVEL_SILENT, 'clamp below min to SILENT')

    ! Within range -> keep as-is
    call set_log_level(LOG_LEVEL_WARNING)
    call ensure(get_log_level() == LOG_LEVEL_WARNING, 'keep valid level (WARNING)')

    ! Above maximum -> clamp to DEBUG
    call set_log_level(999)
    call ensure(get_log_level() == LOG_LEVEL_DEBUG, 'clamp above max to DEBUG')

    ! Reset to default for other tests
    call set_log_level(LOG_LEVEL_WARNING)

contains

    subroutine ensure(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        if (.not. cond) then
            print *, 'ASSERTION FAILED:', trim(msg)
            stop 1
        end if
    end subroutine ensure

end program test_log_level_input_validation

