program test_file_io_error_handling
    use fortplot
    use fortplot_logging, only: log_info, log_error
    use fortplot_validation, only: validation_result_t, validate_file_exists
    use fortplot_security, only: get_test_output_path
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
    
    ! Test 1: Library should handle directory creation gracefully
    print *, ""
    print *, "Test 1: Directory auto-creation handling"
    print *, "----------------------------------------"
    
    invalid_path = "test_output_dir/test_plot.png"
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label="test")
    
    ! Attempt to save to new directory (should auto-create)
    call figure_savefig(fig, invalid_path)
    
    ! Check if file was created (it should be - library auto-creates dirs)
    inquire(file=invalid_path, exist=file_exists)
    if (file_exists) then
        print *, "  PASS: File created with auto-directory creation"
        test_passed = .true.
        ! Clean up
        open(newunit=unit, file=invalid_path, status='old')
        close(unit, status='delete')
    else
        print *, "  ERROR: File not created even with valid path!"
        test_passed = .false.
    end if
    all_tests_passed = all_tests_passed .and. test_passed
    
    ! Test 2: Test with empty filename (should fail gracefully)
    print *, ""
    print *, "Test 2: Empty filename handling"
    print *, "-----------------------------------"
    
    invalid_path = ""  ! Empty filename
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label="test")
    
    ! Attempt to save to empty path (should not crash)
    call figure_savefig(fig, invalid_path)
    
    ! Test passes if we reach here without crashing
    print *, "  PASS: Empty filename handled gracefully (no crash)"
    test_passed = .true.
    all_tests_passed = all_tests_passed .and. test_passed
    
    ! Test 3: Valid directory - should succeed
    print *, ""
    print *, "Test 3: Valid directory handling"
    print *, "---------------------------------"
    
    valid_file = "test_io_valid.png"
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label="test")
    
    ! Save to valid path
    call figure_savefig(fig, valid_file)
    
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
    
    ! Test 4: Test with PDF backend auto-creation
    print *, ""
    print *, "Test 4: PDF backend directory handling"
    print *, "-----------------------------------"
    
    invalid_path = "test_pdf_dir/test_plot.pdf"
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label="test")
    
    ! Attempt to save PDF (should auto-create directory)
    call figure_savefig(fig, invalid_path)
    
    inquire(file=invalid_path, exist=file_exists)
    if (file_exists) then
        print *, "  PASS: PDF created with directory auto-creation"
        test_passed = .true.
        ! Clean up
        open(newunit=unit, file=invalid_path, status='old')
        close(unit, status='delete')
    else
        print *, "  ERROR: PDF not created!"
        test_passed = .false.
    end if
    all_tests_passed = all_tests_passed .and. test_passed
    
    ! Test 5: Test with ASCII backend auto-creation
    print *, ""
    print *, "Test 5: ASCII backend directory handling"
    print *, "-------------------------------------"
    
    invalid_path = "test_ascii_dir/test_plot.txt"
    call fig%initialize(width=80, height=24)
    call fig%add_plot(x, y, label="test")
    
    ! Attempt to save ASCII (should auto-create directory)
    call figure_savefig(fig, invalid_path)
    
    inquire(file=invalid_path, exist=file_exists)
    if (file_exists) then
        print *, "  PASS: ASCII file created with directory auto-creation"
        test_passed = .true.
        ! Clean up
        open(newunit=unit, file=invalid_path, status='old')
        close(unit, status='delete')
    else
        print *, "  ERROR: ASCII file not created!"
        test_passed = .false.
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