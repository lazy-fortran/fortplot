program test_system_fpm_example
    implicit none
    integer :: exit_code
    character(len=256) :: cwd
    
    ! Get current working directory
    call getcwd(cwd)
    
    ! Change to fpm example directory
    call chdir("doc/fpm_example")
    
    ! Clean any previous build
    call execute_command_line("fpm clean --skip", exitstat=exit_code)
    if (exit_code /= 0) then
        print *, "Failed to clean fpm build"
        stop 1
    end if
    
    ! Build the fpm example
    call execute_command_line("fpm build", exitstat=exit_code)
    if (exit_code /= 0) then
        print *, "Failed to build fpm example"
        stop 1
    end if
    
    ! Change back to original directory
    call chdir(trim(cwd))
    
    print *, "FPM example build test passed"
    
end program test_system_fpm_example