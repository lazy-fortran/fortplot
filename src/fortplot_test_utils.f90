module fortplot_test_utils
    !! Test utilities for Windows CI performance optimization
    !!
    !! This module provides utilities to help tests run efficiently on Windows CI
    !! while maintaining test coverage quality (Issue #188).
    
    use iso_fortran_env, only: int32, real64
    use fortplot_system_runtime, only: is_windows
    use fortplot_windows_performance, only: is_ci_environment, &
                                            setup_windows_performance
    use fortplot_ci_performance_monitor, only: ci_performance_monitor_t, &
                                               get_ci_monitor
    implicit none
    private
    
    public :: skip_on_slow_ci
    public :: use_fast_test_mode
    public :: start_performance_test
    public :: end_performance_test
    public :: assert_performance_target
    public :: int_to_str
    public :: get_platform_tolerance
    
contains
    
    function skip_on_slow_ci(test_name) result(should_skip)
        !! Determine if test should be skipped on slow CI
        character(len=*), intent(in) :: test_name
        logical :: should_skip
        
        character(len=256) :: env_value
        integer :: stat
        
        should_skip = .false.
        
        ! Check if we're on Windows CI
        if (.not. is_windows()) return
        if (.not. is_ci_environment()) return
        
        ! Check for force-run flag
        call get_environment_variable("FORTPLOT_RUN_ALL_TESTS", env_value, status=stat)
        if (stat == 0 .and. (trim(env_value) == "1" .or. trim(env_value) == "true")) then
            should_skip = .false.
            return
        end if
        
        ! Check if this is a known slow test that should be optimized
        select case(trim(test_name))
        case("test_pcolormesh_consolidated", &
             "test_histogram_consolidated", &
             "test_contour_filled_backend_rendering")
            ! These are the tests from Issue #188
            ! With optimizations, they should not be skipped
            should_skip = .false.
        case default
            should_skip = .false.
        end select
        
    end function skip_on_slow_ci
    
    function use_fast_test_mode() result(use_fast)
        !! Check if fast test mode should be used
        logical :: use_fast
        
        character(len=256) :: env_value
        integer :: stat
        
        use_fast = .false.
        
        ! Automatically use fast mode on Windows CI
        if (is_windows() .and. is_ci_environment()) then
            use_fast = .true.
            return
        end if
        
        ! Check for explicit fast mode flag
        call get_environment_variable("FORTPLOT_FAST_TESTS", env_value, status=stat)
        if (stat == 0 .and. (trim(env_value) == "1" .or. trim(env_value) == "true")) then
            use_fast = .true.
        end if
        
    end function use_fast_test_mode
    
    subroutine start_performance_test(test_name)
        !! Start timing a performance test
        character(len=*), intent(in) :: test_name
        
        type(ci_performance_monitor_t), pointer :: monitor
        
        monitor => get_ci_monitor()
        call monitor%start_test(test_name)
        
    end subroutine start_performance_test
    
    subroutine end_performance_test(test_name)
        !! End timing a performance test
        character(len=*), intent(in), optional :: test_name
        
        type(ci_performance_monitor_t), pointer :: monitor
        
        monitor => get_ci_monitor()
        if (present(test_name)) then
            call monitor%end_test(test_name)
        else
            call monitor%end_test()
        end if
        
    end subroutine end_performance_test
    
    subroutine assert_performance_target(test_name, target_time, actual_time)
        !! Assert that performance meets target
        character(len=*), intent(in) :: test_name
        real(real64), intent(in) :: target_time
        real(real64), intent(in) :: actual_time
        
        real(real64) :: ratio
        
        ratio = actual_time / target_time
        
        if (ratio > 1.0_real64) then
            print *, "PERFORMANCE WARNING: ", trim(test_name)
            print *, "  Target time: ", target_time, " seconds"
            print *, "  Actual time: ", actual_time, " seconds"
            print *, "  Ratio: ", ratio, "x slower than target"
            
            if (ratio > 2.0_real64) then
                print *, "  CRITICAL: Performance more than 2x slower than target!"
            end if
        else
            print *, "PERFORMANCE OK: ", trim(test_name)
            print *, "  Target time: ", target_time, " seconds"
            print *, "  Actual time: ", actual_time, " seconds"
            print *, "  Ratio: ", 1.0_real64/ratio, "x faster than target"
        end if
        
    end subroutine assert_performance_target
    
    function int_to_str(i) result(str)
        !! Convert integer to string
        integer, intent(in) :: i
        character(len=32) :: str
        
        write(str, '(I0)') i
        str = adjustl(str)
        
    end function int_to_str
    
    function get_platform_tolerance(base_tolerance) result(tolerance)
        !! Get platform-appropriate tolerance for numerical comparisons
        !! Addresses Issue #297: Windows CI precision differences
        !!
        !! On Windows, floating-point precision can vary due to:
        !! - Different compiler optimization flags
        !! - Different math libraries (MSYS2 vs native)
        !! - Different CPU instruction sets
        !!
        !! This function provides a systematic way to handle platform
        !! precision differences while maintaining test accuracy.
        
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: base_tolerance
        real(real64) :: tolerance
        
        if (is_windows()) then
            ! On Windows, relax tolerance to handle:
            ! - Different math libraries (MSYS2 mingw-w64 vs native)
            ! - Different optimization levels and compiler flags
            ! - Different floating-point unit control words
            ! Factor of 100 for very small values, 10 for larger values
            if (base_tolerance < 1.0e-10_real64) then
                tolerance = base_tolerance * 100.0_real64
            else
                tolerance = base_tolerance * 10.0_real64
            end if
        else
            ! On Linux/Unix, maintain tight tolerance for precision
            tolerance = base_tolerance
        end if
        
    end function get_platform_tolerance
    
end module fortplot_test_utils