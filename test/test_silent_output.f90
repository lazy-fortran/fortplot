program test_silent_output
    !! Test program to verify silent output works in production
    !! Should demonstrate zero output when set to silent level
    
    use fortplot, only: figure, plot, savefig, set_log_level, LOG_LEVEL_SILENT
    use fortplot_security, only: get_test_output_path
    
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
    call figure(figsize=[4.0d0, 3.0d0])
    call plot(x, y)
    call savefig(get_test_output_path('/tmp/silent_test.png'))
    
    ! This print should still work (it's not going through logging)
    print *, "Silent test completed - output saved"

end program test_silent_output