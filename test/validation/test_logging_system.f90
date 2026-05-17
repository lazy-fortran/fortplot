program test_logging_system
    !! Test program to validate the new logging system
    !! Demonstrates control over console output verbosity
    
    use fortplot, only: set_log_level, LOG_LEVEL_SILENT, LOG_LEVEL_ERROR, &
                        LOG_LEVEL_WARNING, LOG_LEVEL_INFO, LOG_LEVEL_DEBUG
    use fortplot_logging, only: log_info, log_warning, log_error, log_debug
    
    implicit none
    
    print *, "=== Testing fortplot logging system ==="
    
    ! Test default level (warnings and errors)
    print *, "Default level (warnings and errors):"
    call log_error("This error should appear")
    call log_warning("This warning should appear")
    call log_info("This info should NOT appear")
    call log_debug("This debug should NOT appear")
    
    print *, ""
    
    ! Test silent level
    print *, "Silent level (no output):"
    call set_log_level(LOG_LEVEL_SILENT)
    call log_error("This error should NOT appear")
    call log_warning("This warning should NOT appear")
    call log_info("This info should NOT appear")
    call log_debug("This debug should NOT appear")
    
    print *, ""
    
    ! Test info level
    print *, "Info level (all except debug):"
    call set_log_level(LOG_LEVEL_INFO)
    call log_error("This error should appear")
    call log_warning("This warning should appear")
    call log_info("This info should appear")
    call log_debug("This debug should NOT appear")
    
    print *, ""
    
    ! Test debug level (all messages)
    print *, "Debug level (all messages):"
    call set_log_level(LOG_LEVEL_DEBUG)
    call log_error("This error should appear")
    call log_warning("This warning should appear")
    call log_info("This info should appear")
    call log_debug("This debug should appear")
    
    print *, ""
    print *, "=== Logging system test completed ==="
    
    ! Reset to default for other tests
    call set_log_level(LOG_LEVEL_WARNING)

end program test_logging_system