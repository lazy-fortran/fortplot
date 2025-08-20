program test_security_validation
    !! Test security validation functions including single dot vulnerability  
    use fortplot_security, only: is_safe_path
    implicit none
    
    integer :: test_count, passed_count
    
    test_count = 0
    passed_count = 0
    
    print *, "=============================================="
    print *, "Security Path Validation Tests"
    print *, "=============================================="
    print *, ""
    
    ! Test dangerous paths - existing tests
    call test_path_validation("../../../etc/passwd", .false., "Directory traversal attack", test_count, passed_count)
    call test_path_validation("file.mp4; rm -rf /", .false., "Command injection", test_count, passed_count)
    call test_path_validation("file.mp4`whoami`", .false., "Command substitution", test_count, passed_count)
    call test_path_validation("file.mp4$(rm -f *)", .false., "Command substitution variant", test_count, passed_count)
    
    ! NEW: Test single dot vulnerability (Issue #137)
    print *, ""
    print *, "Single Dot Vulnerability Tests (Issue #137):"
    print *, "--------------------------------------------"
    call test_path_validation("./sensitive_file", .false., "Current directory reference", test_count, passed_count)
    call test_path_validation("./", .false., "Current directory path", test_count, passed_count)
    call test_path_validation("./etc/passwd", .false., "Current dir to system files", test_count, passed_count)
    call test_path_validation("./././file", .false., "Multiple current dir references", test_count, passed_count)
    call test_path_validation("dir/./file", .false., "Hidden current dir in path", test_count, passed_count)
    call test_path_validation("/.bashrc", .false., "Root with dot file", test_count, passed_count)
    call test_path_validation("//etc///passwd", .false., "Multiple consecutive slashes", test_count, passed_count)
    call test_path_validation("output/./file.png", .false., "Hidden traversal in output", test_count, passed_count)
    
    print *, ""
    
    ! Test safe paths
    call test_path_validation("animation.mp4", .true., "Simple filename", test_count, passed_count)
    call test_path_validation("output/test.mp4", .true., "Subdirectory path", test_count, passed_count)
    call test_path_validation("my-video_file.mp4", .true., "Complex safe filename", test_count, passed_count)
    call test_path_validation("/tmp/output.png", .true., "Absolute path", test_count, passed_count)
    call test_path_validation("plots/figure_2025.pdf", .true., "Year in filename", test_count, passed_count)
    
    print *, ""
    print *, "=============================================="
    write(*, '(A,I0,A,I0,A)') "Results: ", passed_count, "/", test_count, " tests passed"
    
    if (passed_count == test_count) then
        print *, "✓ All security validation tests PASSED!"
        print *, "✓ Single dot vulnerability properly tested and blocked"
        stop 0
    else
        print *, "✗ Some security validation tests FAILED!"
        print *, "✗ Security vulnerabilities may exist"
        stop 1
    end if

contains

    subroutine test_path_validation(path, expected_valid, description, test_count, passed_count)
        character(len=*), intent(in) :: path
        logical, intent(in) :: expected_valid
        character(len=*), intent(in) :: description
        integer, intent(inout) :: test_count, passed_count
        logical :: is_valid
        
        test_count = test_count + 1
        is_valid = is_safe_path(path)
        
        write(*, '(A,A,A)', advance='no') "Testing: '", trim(path), "' ("
        write(*, '(A)', advance='no') trim(description)
        write(*, '(A)', advance='no') ") - "
        
        if (is_valid .eqv. expected_valid) then
            if (expected_valid) then
                print *, "PASS (allowed)"
            else
                print *, "PASS (blocked)"
            end if
            passed_count = passed_count + 1
        else
            if (expected_valid) then
                print *, "FAIL (should allow, but blocked)"
            else
                print *, "FAIL (SECURITY VULNERABILITY: should block, but allowed)"
            end if
        end if
    end subroutine test_path_validation

end program test_security_validation