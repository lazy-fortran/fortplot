program test_expected_fail_guard
    implicit none
    integer :: stat
    logical :: has_test_dir

    print *, 'Policy: Forbid use of "EXPECTED FAIL" markers in tests'

    ! If the 'test' directory is not visible from current working directory,
    ! skip gracefully to keep this guard portable across runners.
    inquire(file='test', exist=has_test_dir)
    if (.not. has_test_dir) then
        print *, 'SKIP: test directory not found; skipping EXPECTED FAIL guard'
        stop 0
    end if

    ! Try ripgrep first for speed; fall back to grep if unavailable.
    call execute_command_line( &
        'sh -lc ''rg -n --glob "!**/test_expected_fail_guard.f90" -F "EXPECTED FAIL" test''', exitstat=stat)

    if (stat == 0) then
        print *, 'FAIL: Found forbidden "EXPECTED FAIL" markers in tests'
        stop 1
    else if (stat == 1) then
        print *, 'PASS: No "EXPECTED FAIL" markers found (rg)'
        stop 0
    else
        ! rg not available or other error; try grep -R as fallback
        call execute_command_line( &
            'sh -lc ''grep -R -n --exclude="*test_expected_fail_guard.f90" -F "EXPECTED FAIL" test''', exitstat=stat)
        if (stat == 0) then
            print *, 'FAIL: Found forbidden "EXPECTED FAIL" markers in tests'
            stop 1
        else if (stat == 1) then
            print *, 'PASS: No "EXPECTED FAIL" markers found (grep)'
            stop 0
        else
            print *, 'SKIP: Could not run text search tool (rg/grep)'
            stop 0
        end if
    end if
end program test_expected_fail_guard
