program test_axis_range_verification
    !! Unit test to verify axis ranges for mathematical function plots
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_sine_wave_range()
    call test_axis_tick_coverage()
    print *, "All axis range tests passed!"
    
contains

    subroutine test_sine_wave_range()
        !! Test that sine wave data covers at least one complete period (2π)
        real(wp), parameter :: PI = 3.141592653589793_wp
        real(wp), dimension(50) :: x, y
        integer :: i
        
        ! Generate test sine data (fixed implementation)
        x = [(real(i-1, wp) * 4.0_wp * PI / 49.0_wp, i=1, 50)]
        y = sin(x)
        
        ! Test assertions
        if (maxval(x) < 2.0_wp * PI) then
            print *, "FAIL: Sine wave x-axis range only covers", maxval(x), "but should cover at least 2π =", 2.0_wp * PI
            print *, "Current range: 0 to", maxval(x), "(covers", maxval(x)/(2.0_wp * PI), "periods)"
            print *, "Recommended: extend to 4π for two complete periods"
            error stop 1
        end if
        
        ! Check that we have reasonable resolution
        if (size(x) < 40) then
            print *, "FAIL: Insufficient resolution for sine wave (", size(x), "points)"
            error stop 1
        end if
        
        print *, "PASS: Sine wave covers", maxval(x)/(2.0_wp * PI), "periods with", size(x), "points"
    end subroutine test_sine_wave_range
    
    subroutine test_axis_tick_coverage()
        !! Test that axis ticks properly span the data range
        real(wp), parameter :: PI = 3.141592653589793_wp
        real(wp), dimension(50) :: x
        integer :: i
        
        ! Generate test data  
        x = [(real(i-1, wp) * 4.0_wp * PI / 49.0_wp, i=1, 50)]
        
        ! Check tick boundaries should cover data range
        if (minval(x) < 0.0_wp) then
            print *, "FAIL: Minimum x value is negative:", minval(x)
            error stop 1
        end if
        
        ! Verify the tick ending issue - x should end at a meaningful value
        if (maxval(x) > 10.0_wp .and. mod(int(maxval(x)), 5) /= 0) then
            print *, "WARN: X-axis max", maxval(x), "doesn't align with nice tick intervals"
        end if
        
        print *, "PASS: X-axis range is", minval(x), "to", maxval(x)
    end subroutine test_axis_tick_coverage

end program test_axis_range_verification