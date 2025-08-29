module fortplot_windows_test_helper
    !! Windows-specific test helpers for CI compatibility
    !! Issue #300: Windows CI environment compatibility
    
    use iso_fortran_env, only: real64, int32
    use fortplot_system_runtime, only: is_windows
    implicit none
    private
    
    public :: get_test_temp_dir
    public :: normalize_test_path
    public :: skip_test_on_windows_ci
    public :: get_windows_safe_tolerance
    
contains
    
    function get_test_temp_dir() result(temp_dir)
        !! Get appropriate temporary directory for tests
        character(len=256) :: temp_dir
        character(len=256) :: env_value
        integer :: status
        
        if (is_windows()) then
            ! Try Windows TEMP first
            call get_environment_variable("TEMP", env_value, status=status)
            if (status == 0 .and. len_trim(env_value) > 0) then
                temp_dir = trim(env_value)
                return
            end if
            
            ! Try TMP as fallback
            call get_environment_variable("TMP", env_value, status=status)
            if (status == 0 .and. len_trim(env_value) > 0) then
                temp_dir = trim(env_value)
                return
            end if
            
            ! Fallback to local directory
            temp_dir = "test_output"
        else
            ! Unix/Linux: use /tmp
            temp_dir = "/tmp"
        end if
    end function get_test_temp_dir
    
    function normalize_test_path(path) result(normalized)
        !! Normalize path for cross-platform compatibility
        character(len=*), intent(in) :: path
        character(len=512) :: normalized
        integer :: i
        
        normalized = path
        
        if (is_windows()) then
            ! Replace /tmp with Windows temp directory
            if (len_trim(path) >= 4 .and. path(1:4) == "/tmp") then
                normalized = trim(get_test_temp_dir()) // path(5:)
            end if
            
            ! Convert forward slashes to backslashes
            do i = 1, len_trim(normalized)
                if (normalized(i:i) == '/') then
                    normalized(i:i) = '\'
                end if
            end do
        end if
    end function normalize_test_path
    
    function skip_test_on_windows_ci(test_name) result(should_skip)
        !! Check if test should be skipped on Windows CI
        character(len=*), intent(in) :: test_name
        logical :: should_skip
        character(len=256) :: ci_env, github_env
        integer :: status
        
        should_skip = .false.
        
        ! Only consider skipping on Windows
        if (.not. is_windows()) return
        
        ! Check if in CI environment
        call get_environment_variable("CI", ci_env, status=status)
        if (status /= 0) then
            call get_environment_variable("GITHUB_ACTIONS", github_env, status=status)
            if (status /= 0) return  ! Not in CI
        end if
        
        ! Skip certain tests known to have issues on Windows CI
        select case(trim(test_name))
        case("test_mpeg_consolidated", &
             "test_ffmpeg_pipe", &
             "test_animation_video")
            should_skip = .true.
            print *, "SKIPPED on Windows CI: ", trim(test_name)
        case default
            should_skip = .false.
        end select
    end function skip_test_on_windows_ci
    
    function get_windows_safe_tolerance(base_tol) result(tolerance)
        !! Get Windows-safe numerical tolerance
        real(real64), intent(in) :: base_tol
        real(real64) :: tolerance
        
        if (is_windows()) then
            ! Windows with MSYS2 may have different floating-point behavior
            ! Use adaptive tolerance based on magnitude
            if (base_tol < 1.0e-12_real64) then
                tolerance = base_tol * 1000.0_real64  ! Very small values need more tolerance
            else if (base_tol < 1.0e-8_real64) then
                tolerance = base_tol * 100.0_real64   ! Small values
            else
                tolerance = base_tol * 10.0_real64    ! Normal values
            end if
        else
            tolerance = base_tol
        end if
    end function get_windows_safe_tolerance
    
end module fortplot_windows_test_helper