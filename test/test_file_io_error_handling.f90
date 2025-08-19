program test_file_io_error_handling
    use fortplot
    use fortplot_logging, only: log_info, log_error
    use fortplot_validation, only: validation_result_t, validate_file_exists
    use iso_fortran_env, only: error_unit, wp => real64
    implicit none
    
    integer :: i, status, unit
    character(len=256) :: test_msg
    logical :: test_passed, all_tests_passed
    type(figure_t) :: fig
    real(wp) :: x(10), y(10)
    type(validation_result_t) :: validation
    character(len=256) :: invalid_path
    character(len=256) :: valid_file
    logical :: file_exists
    
    all_tests_passed = .true.
    
    ! Initialize test data
    do i = 1, 10
        x(i) = real(i, wp)
        y(i) = real(i**2, wp)
    end do
    
    print *, "Testing file I/O error handling..."
    print *, "=================================="
    
    ! Test 1: Saving to non-existent directory should fail gracefully
    print *, ""
    print *, "Test 1: Non-existent directory handling"
    print *, "----------------------------------------"
    
    invalid_path = "/non/existent/directory/test_plot.png"
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label="test")
    
    ! Attempt to save to invalid path
    call fig%savefig(invalid_path)
    
    ! Check if file was created (it shouldn't be)
    inquire(file=invalid_path, exist=file_exists)
    if (file_exists) then
        print *, "  ERROR: File created in non-existent directory!"
        test_passed = .false.
    else
        print *, "  PASS: File not created in non-existent directory"
        test_passed = .true.
    end if
    all_tests_passed = all_tests_passed .and. test_passed
    
    ! Test 2: Permission denied handling (try to write to root)
    print *, ""
    print *, "Test 2: Permission denied handling"
    print *, "-----------------------------------"
    
    invalid_path = "/root/test_plot.png"
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label="test")
    
    ! Attempt to save to restricted path
    call fig%savefig(invalid_path)
    
    ! Check if file was created (it shouldn't be)
    inquire(file=invalid_path, exist=file_exists)
    if (file_exists) then
        print *, "  ERROR: File created in restricted directory!"
        test_passed = .false.
    else
        print *, "  PASS: File not created in restricted directory"
        test_passed = .true.
    end if
    all_tests_passed = all_tests_passed .and. test_passed
    
    ! Test 3: Valid directory - should succeed
    print *, ""
    print *, "Test 3: Valid directory handling"
    print *, "---------------------------------"
    
    valid_file = "test_io_valid.png"
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label="test")
    
    ! Save to valid path
    call fig%savefig(valid_file)
    
    ! Check if file was created
    inquire(file=valid_file, exist=file_exists)
    if (file_exists) then
        print *, "  PASS: File created successfully in valid directory"
        test_passed = .true.
        ! Clean up
        open(newunit=unit, file=valid_file, status='old')
        close(unit, status='delete')
    else
        print *, "  ERROR: File not created in valid directory!"
        test_passed = .false.
    end if
    all_tests_passed = all_tests_passed .and. test_passed
    
    ! Test 4: Test with PDF backend
    print *, ""
    print *, "Test 4: PDF backend error handling"
    print *, "-----------------------------------"
    
    invalid_path = "/non/existent/directory/test_plot.pdf"
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label="test")
    
    ! Attempt to save PDF to invalid path
    call fig%savefig(invalid_path)
    
    inquire(file=invalid_path, exist=file_exists)
    if (file_exists) then
        print *, "  ERROR: PDF created in non-existent directory!"
        test_passed = .false.
    else
        print *, "  PASS: PDF not created in non-existent directory"
        test_passed = .true.
    end if
    all_tests_passed = all_tests_passed .and. test_passed
    
    ! Test 5: Test with ASCII backend
    print *, ""
    print *, "Test 5: ASCII backend error handling"
    print *, "-------------------------------------"
    
    invalid_path = "/non/existent/directory/test_plot.txt"
    call fig%initialize(width=80, height=24)
    call fig%add_plot(x, y, label="test")
    
    ! Attempt to save ASCII to invalid path
    call fig%savefig(invalid_path)
    
    inquire(file=invalid_path, exist=file_exists)
    if (file_exists) then
        print *, "  ERROR: ASCII file created in non-existent directory!"
        test_passed = .false.
    else
        print *, "  PASS: ASCII file not created in non-existent directory"
        test_passed = .true.
    end if
    all_tests_passed = all_tests_passed .and. test_passed
    
    ! Summary
    print *, ""
    print *, "=================================="
    if (all_tests_passed) then
        print *, "All file I/O error handling tests PASSED"
        stop 0
    else
        print *, "Some file I/O error handling tests FAILED"
        stop 1
    end if
    
end program test_file_io_error_handling