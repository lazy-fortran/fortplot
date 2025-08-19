program test_python_docstring_integration
    !! Integration test for Python docstring validation
    !!
    !! Given: Python interface docstring validation script
    !! When: We execute the Python validation test
    !! Then: Python docstring requirements must be satisfied
    
    use iso_fortran_env, only: wp => real64
    implicit none
    
    logical :: test_passed
    integer :: exit_code
    
    print *, "Running Python docstring validation integration test..."
    
    ! Execute Python docstring validation script
    call execute_command_line("python test/test_python_docstring_validation.py", &
                               exitstat=exit_code)
    
    test_passed = (exit_code == 0)
    
    if (test_passed) then
        print *, "PASS: Python docstring validation test completed successfully"
    else
        print *, "FAIL: Python docstring validation test failed"
        print *, "      Core Python functions need docstrings added"
        print *, "      Run: python test/test_python_docstring_validation.py"
        print *, "      for detailed failure information"
        error stop "Python docstring validation failed"
    end if
    
end program test_python_docstring_integration