program test_security_validation
    !! Test security validation functions
    use fortplot_system_secure, only: validate_path_secure
    implicit none
    
    ! Test dangerous paths
    call test_path_validation("../../../etc/passwd", .false.)
    call test_path_validation("file.mp4; rm -rf /", .false.)
    call test_path_validation("file.mp4`whoami`", .false.)
    call test_path_validation("file.mp4$(rm -f *)", .false.)
    
    ! Test safe paths
    call test_path_validation("animation.mp4", .true.)
    call test_path_validation("output/test.mp4", .true.)
    call test_path_validation("my-video_file.mp4", .true.)
    
    print *, "✓ All security validation tests passed!"

contains

    subroutine test_path_validation(path, expected_valid)
        character(len=*), intent(in) :: path
        logical, intent(in) :: expected_valid
        logical :: is_valid
        
        is_valid = validate_path_secure(path)
        
        if (is_valid .eqv. expected_valid) then
            print *, "✓ PASS: '", trim(path), "' -> valid:", is_valid
        else
            print *, "✗ FAIL: '", trim(path), "' -> expected:", expected_valid, "got:", is_valid
            stop 1
        end if
    end subroutine test_path_validation

end program test_security_validation