program test_silent_output
    !! Test program to verify silent output works in production
    !! Should demonstrate zero output when set to silent level
    
    use fortplot, only: figure, plot, savefig, set_log_level, LOG_LEVEL_SILENT
    
    implicit none
    real(8) :: x(10), y(10)
    integer :: i
    
    ! Set to silent mode - no console output should occur
    call set_log_level(LOG_LEVEL_SILENT)
    
    ! Generate test data
    do i = 1, 10
        x(i) = real(i-1, 8)
        y(i) = sin(x(i))
    end do
    
    ! Create plot - should produce no console output
    call figure(400, 300)
    call plot(x, y)
    call savefig('/tmp/silent_test.png')
    
    ! This print should still work (it's not going through logging)
    print *, "Silent test completed - check /tmp/silent_test.png"

end program test_silent_output