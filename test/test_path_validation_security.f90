!! Path validation security test suite
!! Tests for Issue #135: Path validation allows relative paths with dots creating potential security vulnerability
!!
!! Given: A path validation function that should block dangerous paths
!! When: Testing various path patterns for security vulnerabilities  
!! Then: Ensure malicious paths are rejected and safe paths are allowed
program test_path_validation_security
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use fortplot_security, only: is_safe_path
    use fortplot, only: figure_t
    implicit none
    
    logical :: all_tests_passed, is_ci_environment
    integer :: test_count, passed_count
    character(len=256) :: ci_var
    
    all_tests_passed = .true.
    test_count = 0
    passed_count = 0
    
    ! Check for CI environment to skip heavy file operations
    call get_environment_variable('GITHUB_ACTIONS', ci_var)
    is_ci_environment = (len_trim(ci_var) > 0)
    if (.not. is_ci_environment) then
        call get_environment_variable('CI', ci_var)
        is_ci_environment = (len_trim(ci_var) > 0)
    end if
    
    print *, "Testing path validation security (Issue #135)..."
    print *, "================================================"
    
    if (is_ci_environment) then
        print *, "CI environment detected - focusing on validation logic only"
    end if
    
    ! Test categories
    call test_single_dot_vulnerability(all_tests_passed, test_count, passed_count)
    call test_double_dot_traversal(all_tests_passed, test_count, passed_count)
    call test_valid_paths(all_tests_passed, test_count, passed_count)
    call test_attack_vectors(all_tests_passed, test_count, passed_count)
    call test_edge_cases(all_tests_passed, test_count, passed_count)
    call test_savefig_integration(all_tests_passed, test_count, passed_count, is_ci_environment)
    
    ! Summary
    print *, ""
    print *, "================================================"
    write(*, '(A,I0,A,I0,A)') "Results: ", passed_count, "/", test_count, " tests passed"
    
    if (all_tests_passed) then
        print *, "All path validation security tests PASSED"
        stop 0
    else
        print *, "Some path validation security tests FAILED"
        print *, "CRITICAL: Security vulnerability confirmed in Issue #135"
        stop 1
    end if

contains

    !! Given: Paths containing single dots
    !! When: Testing if single dots are properly blocked
    !! Then: Single dots in suspicious contexts should be rejected for security
    subroutine test_single_dot_vulnerability(all_passed, test_count, passed_count)
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, passed_count
        
        print *, ""
        print *, "Test Category: Single Dot Vulnerability (CRITICAL)"
        print *, "------------------------------------------------"
        
        ! These paths with single dots should be BLOCKED for security
        ! Current vulnerability: is_safe_path allows these dangerous patterns
        
        call assert_path_blocked("./sensitive_file.png", "Current directory reference", &
                                 all_passed, test_count, passed_count)
        call assert_path_blocked("dir/./file.png", "Hidden current directory in path", &
                                 all_passed, test_count, passed_count)
        call assert_path_blocked("./../../etc/passwd", "Combined single/double dot attack", &
                                 all_passed, test_count, passed_count)
        call assert_path_blocked("./config/secrets.txt", "Access to current dir config", &
                                 all_passed, test_count, passed_count)
        call assert_path_blocked("output/./hidden.png", "Hidden directory traversal", &
                                 all_passed, test_count, passed_count)
        call assert_path_blocked("/.bashrc", "Root directory with single dot prefix", &
                                 all_passed, test_count, passed_count)
    end subroutine test_single_dot_vulnerability

    !! Given: Paths containing double dots
    !! When: Testing if double dots are properly blocked  
    !! Then: Directory traversal attempts should be rejected
    subroutine test_double_dot_traversal(all_passed, test_count, passed_count)
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, passed_count
        
        print *, ""
        print *, "Test Category: Double Dot Traversal (Known Issue)"
        print *, "------------------------------------------------"
        
        ! These should be BLOCKED (current implementation should handle these)
        call assert_path_blocked("../../../etc/passwd", "Directory traversal attack", &
                                 all_passed, test_count, passed_count)
        call assert_path_blocked("output/../../../home/user/.ssh/id_rsa", "Complex traversal", &
                                 all_passed, test_count, passed_count)
        call assert_path_blocked("../sensitive.png", "Simple parent directory access", &
                                 all_passed, test_count, passed_count)
        call assert_path_blocked("plots/../config/database.conf", "Config file access", &
                                 all_passed, test_count, passed_count)
    end subroutine test_double_dot_traversal

    !! Given: Legitimate file paths
    !! When: Testing if valid paths are correctly allowed
    !! Then: Safe paths should be accepted by validation
    subroutine test_valid_paths(all_passed, test_count, passed_count)
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, passed_count
        
        print *, ""
        print *, "Test Category: Valid Paths (Should Pass)"
        print *, "---------------------------------------"
        
        ! These should be ALLOWED
        call assert_path_allowed("output.png", "Simple filename", &
                                all_passed, test_count, passed_count)
        call assert_path_allowed("plots/figure1.png", "Subdirectory path", &
                                all_passed, test_count, passed_count)
        call assert_path_allowed("/tmp/output.pdf", "Absolute path", &
                                all_passed, test_count, passed_count)
        call assert_path_allowed("results/data_analysis_2025.png", "Complex valid filename", &
                                all_passed, test_count, passed_count)
        call assert_path_allowed("output/subdir/file.txt", "Multi-level subdirectory", &
                                all_passed, test_count, passed_count)
    end subroutine test_valid_paths

    !! Given: Various attack vector patterns
    !! When: Testing sophisticated attack attempts
    !! Then: All attack vectors should be blocked
    subroutine test_attack_vectors(all_passed, test_count, passed_count)
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, passed_count
        
        print *, ""
        print *, "Test Category: Attack Vectors (CRITICAL)"
        print *, "---------------------------------------"
        
        ! Injection attacks - these should be BLOCKED
        call assert_path_blocked("file.png; rm -rf /", "Command injection attempt", &
                                all_passed, test_count, passed_count)
        call assert_path_blocked("file.png | cat /etc/passwd", "Pipe injection", &
                                all_passed, test_count, passed_count)
        call assert_path_blocked("file.png && ls -la", "Command chaining", &
                                all_passed, test_count, passed_count)
        call assert_path_blocked("file.png`whoami`", "Command substitution", &
                                all_passed, test_count, passed_count)
        call assert_path_blocked("$(rm -rf /).png", "Command substitution in filename", &
                                all_passed, test_count, passed_count)
        
        ! Path manipulation attacks
        call assert_path_blocked("file.png//", "Double slash attack", &
                                all_passed, test_count, passed_count)
        call assert_path_blocked("/dev/null", "Device file access", &
                                all_passed, test_count, passed_count)
        call assert_path_blocked("/proc/self/mem", "Process memory access", &
                                all_passed, test_count, passed_count)
    end subroutine test_attack_vectors

    !! Given: Edge case path patterns
    !! When: Testing boundary conditions and unusual inputs
    !! Then: Edge cases should be handled securely
    subroutine test_edge_cases(all_passed, test_count, passed_count)
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, passed_count
        
        print *, ""
        print *, "Test Category: Edge Cases"
        print *, "------------------------"
        
        ! Edge cases - these should be BLOCKED
        call assert_path_blocked("", "Empty path", &
                                all_passed, test_count, passed_count)
        call assert_path_blocked(repeat("a", 5000), "Extremely long path", &
                                all_passed, test_count, passed_count)
        call assert_path_blocked(char(0) // "file.png", "Null byte injection", &
                                all_passed, test_count, passed_count)
        call assert_path_blocked("file" // char(10) // ".png", "Newline in filename", &
                                all_passed, test_count, passed_count)
        call assert_path_blocked("file.png" // char(13), "Carriage return", &
                                all_passed, test_count, passed_count)
        
        ! Quote-based attacks
        call assert_path_blocked("'file.png'", "Single quotes", &
                                all_passed, test_count, passed_count)
        call assert_path_blocked('"file.png"', "Double quotes", &
                                all_passed, test_count, passed_count)
    end subroutine test_edge_cases

    !! Given: Integration with savefig functionality
    !! When: Testing actual savefig calls with malicious paths
    !! Then: savefig should reject dangerous paths through validation
    subroutine test_savefig_integration(all_passed, test_count, passed_count, is_ci)
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, passed_count
        logical, intent(in) :: is_ci
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        integer :: i
        logical :: file_exists
        
        print *, ""
        print *, "Test Category: savefig Integration (CRITICAL)"
        print *, "-------------------------------------------"
        
        if (is_ci) then
            print *, "CI mode: Skipping actual file creation, testing validation logic only"
            
            ! Test path validation directly without creating files
            test_count = test_count + 1
            if (.not. is_safe_path("./malicious.png")) then
                print *, "  PASS: Path validation correctly blocks './malicious.png'"
                passed_count = passed_count + 1
            else
                print *, "  FAIL: Path validation allows dangerous path"
                all_passed = .false.
            end if
            
            test_count = test_count + 1  
            if (.not. is_safe_path("../../../etc/passwd.png")) then
                print *, "  PASS: Path validation correctly blocks '../../../etc/passwd.png'"
                passed_count = passed_count + 1
            else
                print *, "  FAIL: Path validation allows directory traversal"
                all_passed = .false.
            end if
            
            return
        end if
        
        ! Setup test data for full file creation tests
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i**2, wp)
        end do
        
        call fig%initialize(width=400, height=300)
        call fig%add_plot(x, y, label="test")
        
        ! Test that savefig rejects dangerous paths
        ! These calls should FAIL due to path validation
        
        test_count = test_count + 1
        print *, "Testing savefig with single dot path: './malicious.png'"
        call fig%savefig("./malicious.png")
        inquire(file="./malicious.png", exist=file_exists)
        if (.not. file_exists) then
            print *, "  PASS: savefig rejected single dot path"
            passed_count = passed_count + 1
        else
            print *, "  FAIL: savefig created file with dangerous path"
            all_passed = .false.
            ! Clean up if file was created
            open(unit=99, file="./malicious.png", status='old')
            close(99, status='delete')
        end if
        
        test_count = test_count + 1
        print *, "Testing savefig with double dot path: '../malicious.png'"
        call fig%savefig("../malicious.png")
        inquire(file="../malicious.png", exist=file_exists)
        if (.not. file_exists) then
            print *, "  PASS: savefig rejected double dot path"
            passed_count = passed_count + 1
        else
            print *, "  FAIL: savefig created file with traversal path"
            all_passed = .false.
            ! Clean up if file was created
            open(unit=99, file="../malicious.png", status='old')
            close(99, status='delete')
        end if
        
        test_count = test_count + 1
        print *, "Testing savefig with valid path: 'safe_output.png'"
        call fig%savefig("safe_output.png")
        inquire(file="safe_output.png", exist=file_exists)
        if (file_exists) then
            print *, "  PASS: savefig accepted safe path"
            passed_count = passed_count + 1
            ! Clean up test file
            open(unit=99, file="safe_output.png", status='old')
            close(99, status='delete')
        else
            print *, "  FAIL: savefig rejected valid safe path"
            all_passed = .false.
        end if
    end subroutine test_savefig_integration

    !! Helper: Assert that a path should be blocked by validation
    subroutine assert_path_blocked(path, description, all_passed, test_count, passed_count)
        character(len=*), intent(in) :: path, description
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, passed_count
        logical :: is_safe
        
        test_count = test_count + 1
        is_safe = is_safe_path(path)
        
        write(*, '(A,A,A)', advance='no') "Testing BLOCKED: '", trim(path), "' ("
        write(*, '(A)', advance='no') trim(description)
        write(*, '(A)', advance='no') ") - "
        
        if (.not. is_safe) then
            print *, "PASS (correctly blocked)"
            passed_count = passed_count + 1
        else
            print *, "FAIL (SECURITY VULNERABILITY: path allowed)"
            all_passed = .false.
        end if
    end subroutine assert_path_blocked

    !! Helper: Assert that a path should be allowed by validation
    subroutine assert_path_allowed(path, description, all_passed, test_count, passed_count)
        character(len=*), intent(in) :: path, description
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, passed_count
        logical :: is_safe
        
        test_count = test_count + 1
        is_safe = is_safe_path(path)
        
        write(*, '(A,A,A)', advance='no') "Testing ALLOWED: '", trim(path), "' ("
        write(*, '(A)', advance='no') trim(description)
        write(*, '(A)', advance='no') ") - "
        
        if (is_safe) then
            print *, "PASS (correctly allowed)"
            passed_count = passed_count + 1
        else
            print *, "FAIL (valid path incorrectly blocked)"
            all_passed = .false.
        end if
    end subroutine assert_path_allowed

end program test_path_validation_security