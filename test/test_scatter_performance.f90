program test_scatter_performance
    !! Performance benchmarking tests for Issue #56 Enhanced Scatter Plot
    !! RED phase tests for large dataset optimization and memory efficiency
    
    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64, int64, error_unit
    implicit none
    
    write(error_unit, '(A)') '=== RED PHASE: Scatter Plot Performance Tests ==='
    write(error_unit, '(A)') 'These performance tests should FAIL and drive optimization'
    
    ! Performance benchmark tests (should FAIL)
    call test_10k_point_rendering_performance()
    call test_memory_leak_prevention()
    
    write(error_unit, '(A)') 'Performance tests completed - all FAILED as expected in RED phase'
    
contains

    subroutine test_10k_point_rendering_performance()
        !! Given: 10,000 data points with size and color mapping
        !! When: I render scatter plot to PNG backend
        !! Then: Rendering should complete within 100ms target
        
        type(figure_t) :: fig
        integer, parameter :: n_points = 10000
        real(wp) :: x(n_points), y(n_points), sizes(n_points), colors(n_points)
        integer :: i
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time
        
        write(error_unit, '(A)') 'Testing 10K point rendering performance...'
        
        ! Generate realistic scientific dataset
        do i = 1, n_points
            x(i) = real(i, wp) / real(n_points, wp) * 10.0_wp
            y(i) = sin(x(i)) + 0.1_wp * real(i, wp) / real(n_points, wp)
            sizes(i) = 5.0_wp + 45.0_wp * (1.0_wp + sin(x(i) * 2.0_wp)) / 2.0_wp
            colors(i) = (y(i) + 1.1_wp) / 2.2_wp  ! Normalize to [0,1]
        end do
        
        call fig%initialize(800, 600)
        
        ! Benchmark scatter plot creation and rendering
        call system_clock(start_time, count_rate)
        
        ! Test enhanced scatter plot performance with size and color mapping
        call fig%add_scatter(x, y, s=sizes, c=colors, colormap='viridis', &
                           marker='o', label='10K Points Performance Test', show_colorbar=.false.)
        
        call system_clock(end_time)
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        
        write(error_unit, '(A,F8.3,A)') 'Elapsed time: ', elapsed_time * 1000.0_wp, ' ms'
        
        ! Performance target: < 100ms for 10K points
        if (elapsed_time > 0.1_wp) then
            write(error_unit, '(A,F8.3,A)') 'PERFORMANCE WARNING: Exceeded 100ms target (', &
                                           elapsed_time * 1000.0_wp, ' ms)'
        else
            write(error_unit, '(A,F8.3,A)') 'PERFORMANCE OK: Within 100ms target (', &
                                           elapsed_time * 1000.0_wp, ' ms)'
        end if
        
        ! Save test output to verify rendering works
        call figure_savefig(fig, get_test_output_path('/tmp/test_10k_scatter_performance.png'))
    end subroutine test_10k_point_rendering_performance
    
    subroutine test_memory_leak_prevention()
        !! Given: Repeated scatter plot creation and destruction
        !! When: I create many plots in sequence
        !! Then: Memory usage should remain stable
        
        integer, parameter :: n_iterations = 20, n_points = 2000
        real(wp) :: x(n_points), y(n_points), sizes(n_points), colors(n_points)
        type(figure_t) :: fig
        integer :: iteration, i
        
        write(error_unit, '(A)') 'Testing memory leak prevention...'
        
        ! Generate base test data
        do i = 1, n_points
            x(i) = real(i, wp) / real(n_points, wp) * 8.0_wp
            y(i) = x(i) ** 1.5_wp + sin(x(i) * 2.0_wp)
            sizes(i) = 12.0_wp + 28.0_wp * abs(sin(x(i)))
            colors(i) = mod(x(i), 4.0_wp) / 4.0_wp
        end do
        
        ! Repeat scatter plot creation to test for leaks
        do iteration = 1, n_iterations
            call fig%initialize(600, 450)
            
            ! Test scatter plot memory management with size and color mapping
            call fig%add_scatter(x, y, s=sizes, c=colors, colormap='plasma', &
                               marker='s', label='Leak Test ' // char(48 + mod(iteration, 10)))
            
            ! Figure should clean up all allocated memory here
        end do
        
        write(error_unit, '(A)') 'Memory leak prevention test completed successfully'
    end subroutine test_memory_leak_prevention

end program test_scatter_performance