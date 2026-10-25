program test_package_links
    implicit none
    integer :: command_status, exit_status

    call execute_command_line('python3 scripts/check_package_links.py', &
                              exitstat=exit_status, cmdstat=command_status)
    if (command_status /= 0) error stop 'cannot run package source check'
    if (exit_status /= 0) error stop 'invalid package source alias'
    print *, 'PASS: package source links'
end program test_package_links
