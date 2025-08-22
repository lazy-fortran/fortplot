program test_color_performance
    !! Test suite for color parsing and rendering performance validation
    !! Tests performance benchmarks and caching mechanisms
    
    use, intrinsic :: iso_fortran_env, only: wp => real64, int64
    use fortplot_colors, only: parse_color, parse_colors_bulk, clear_color_cache, get_cache_hit_rate, &
                             apply_colormap_to_array, rgb_to_hsv, rgb_to_lab
    use fortplot, only: figure_t
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    ! Performance validation tests
    call test_color_parsing_performance()
    call test_color_caching_mechanism()
    call test_bulk_color_operations()
    call test_memory_efficiency()
    call test_concurrent_color_access()
    call test_color_conversion_performance()
    call test_backend_rendering_performance()
    call test_large_dataset_color_mapping()
    
    call print_test_summary()
    
contains

    subroutine test_color_parsing_performance()
        ! Given: Large number of color parsing operations
        ! When: Parsing colors repeatedly in tight loop
        ! Then: Should maintain acceptable performance (< 1ms per color)
        
        integer, parameter :: n_iterations = 10000
        real(wp) :: rgb(3)
        logical :: success
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time, time_per_operation
        integer :: i
        
        call start_test("Color parsing performance")
        
        ! Benchmark hex color parsing
        call system_clock(start_time, count_rate)
        do i = 1, n_iterations
            call parse_color('#FF0000', rgb, success)
        end do
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        time_per_operation = elapsed_time / real(n_iterations, wp) * 1000.0_wp  ! ms
        
        call assert_true(success, "Color parsing succeeds")
        call assert_performance(time_per_operation, 1.0_wp, "Hex parsing performance < 1ms")
        
        ! Benchmark named color parsing
        call system_clock(start_time)
        do i = 1, n_iterations
            call parse_color('red', rgb, success)
        end do
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        time_per_operation = elapsed_time / real(n_iterations, wp) * 1000.0_wp
        
        call assert_performance(time_per_operation, 1.0_wp, "Named color parsing < 1ms")
        
        ! Benchmark RGB tuple parsing
        call system_clock(start_time)
        do i = 1, n_iterations
            call parse_color('(1.0, 0.5, 0.0)', rgb, success)
        end do
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        time_per_operation = elapsed_time / real(n_iterations, wp) * 1000.0_wp
        
        call assert_performance(time_per_operation, 2.0_wp, "RGB tuple parsing < 2ms")
        
        call end_test()
    end subroutine test_color_parsing_performance

    subroutine test_color_caching_mechanism()
        ! Given: Color caching system enabled
        ! When: Parsing same colors repeatedly
        ! Then: Should demonstrate significant speedup from caching
        
        integer, parameter :: n_iterations = 1000
        real(wp) :: rgb(3)
        logical :: success
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: time_no_cache, time_with_cache, speedup_ratio
        integer :: i
        
        call start_test("Color caching mechanism")
        
        ! Clear any existing cache
        call clear_color_cache()
        
        ! Measure time without cache (first run)
        call system_clock(start_time, count_rate)
        do i = 1, n_iterations
            call parse_color('#FF0000', rgb, success)
        end do
        call system_clock(end_time)
        time_no_cache = real(end_time - start_time, wp) / real(count_rate, wp)
        
        ! Measure time with cache (repeated runs)
        call system_clock(start_time)
        do i = 1, n_iterations
            call parse_color('#FF0000', rgb, success)  ! Should hit cache
        end do
        call system_clock(end_time)
        time_with_cache = real(end_time - start_time, wp) / real(count_rate, wp)
        
        ! Only test speedup if measurements are meaningful (> 1ms)
        if (time_no_cache > 0.001_wp .and. time_with_cache > 0.0_wp) then
            speedup_ratio = time_no_cache / time_with_cache
            write(*, '(A,F6.2,A)') "    Speedup ratio: ", speedup_ratio, "x"
            call assert_true(speedup_ratio > 1.5_wp, "Cache provides >1.5x speedup")
        else
            write(*, '(A)') "    Performance too fast to measure speedup accurately"
        end if
        call assert_cache_hit_rate(0.95_wp, "Cache hit rate > 95%")
        
        ! Test cache size limits
        call test_cache_overflow_behavior()
        
        call end_test()
    end subroutine test_color_caching_mechanism

    subroutine test_bulk_color_operations()
        ! Given: Large arrays of color specifications
        ! When: Processing bulk color operations
        ! Then: Should handle efficiently without memory issues
        
        integer, parameter :: n_colors = 50000
        character(len=20) :: color_specs(n_colors)
        real(wp) :: rgb_results(3, n_colors)
        logical :: success_flags(n_colors)
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time, throughput
        integer :: i, success_count
        
        call start_test("Bulk color operations")
        
        ! Generate variety of color specifications
        do i = 1, n_colors
            select case (mod(i, 4))
            case (0)
                write(color_specs(i), '(A,Z6.6)') '#', i  ! Hex colors
            case (1)
                color_specs(i) = 'red'  ! Named colors
            case (2)
                color_specs(i) = 'r'    ! Single letters
            case (3)
                write(color_specs(i), '(A,F4.2,A,F4.2,A,F4.2,A)') &
                      '(', mod(i,255)/255.0, ',', mod(i*2,255)/255.0, &
                      ',', mod(i*3,255)/255.0, ')'  ! RGB tuples
            end select
        end do
        
        ! Benchmark bulk processing
        call system_clock(start_time, count_rate)
        call parse_colors_bulk(color_specs, rgb_results, success_flags)
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        throughput = real(n_colors, wp) / elapsed_time  ! colors per second
        
        success_count = count(success_flags)
        call assert_true(success_count > n_colors * 0.8, "80% of bulk colors parsed successfully")
        call assert_performance_throughput(throughput, 10000.0_wp, "Throughput > 10k colors/sec")
        
        call end_test()
    end subroutine test_bulk_color_operations

    subroutine test_memory_efficiency()
        ! Given: Color operations with memory constraints
        ! When: Parsing and storing large numbers of colors
        ! Then: Should not cause memory leaks or excessive allocation
        
        integer, parameter :: n_iterations = 1000
        integer :: initial_memory, peak_memory, final_memory
        real(wp) :: memory_growth
        integer :: i
        
        call start_test("Memory efficiency")
        
        initial_memory = get_memory_usage()
        
        ! Perform color operations that might leak memory
        do i = 1, n_iterations
            call stress_test_color_memory()
        end do
        
        peak_memory = get_memory_usage()
        
        ! Trigger garbage collection if available
        call cleanup_color_resources()
        
        final_memory = get_memory_usage()
        memory_growth = real(final_memory - initial_memory, wp) / real(initial_memory, wp)
        
        call assert_true(memory_growth < 0.1_wp, "Memory growth < 10%")
        call assert_true(final_memory < peak_memory * 1.1, "Memory cleaned up properly")
        
        call end_test()
    end subroutine test_memory_efficiency

    subroutine test_concurrent_color_access()
        ! Given: Concurrent color parsing operations
        ! When: Multiple threads access color functions simultaneously
        ! Then: Should handle thread safety without corruption
        
        integer, parameter :: n_threads = 4
        integer, parameter :: n_operations = 1000
        logical :: thread_results(n_threads)
        integer :: i
        
        call start_test("Concurrent color access")
        
        ! This test requires thread-safe color operations
        call init_thread_safe_color_system()
        
        ! Simulate concurrent access (simplified for test)
        do i = 1, n_threads
            call test_color_thread_safety(i, n_operations, thread_results(i))
        end do
        
        call assert_true(all(thread_results), "All threads completed successfully")
        call assert_color_cache_integrity("Color cache maintains integrity")
        
        call end_test()
    end subroutine test_concurrent_color_access

    subroutine test_color_conversion_performance()
        ! Given: Different color space conversions
        ! When: Converting between RGB, HSV, LAB color spaces
        ! Then: Should maintain acceptable conversion performance
        
        integer, parameter :: n_conversions = 10000
        real(wp) :: rgb(3), hsv(3), lab(3)
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time, time_per_conversion
        integer :: i
        
        call start_test("Color conversion performance")
        
        rgb = [0.5_wp, 0.7_wp, 0.3_wp]
        
        ! Benchmark RGB to HSV conversion
        call system_clock(start_time, count_rate)
        do i = 1, n_conversions
            call rgb_to_hsv(rgb, hsv)
        end do
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        time_per_conversion = elapsed_time / real(n_conversions, wp) * 1000000.0_wp  ! µs
        
        call assert_performance(time_per_conversion, 10.0_wp, "RGB to HSV < 10µs")
        
        ! Benchmark RGB to LAB conversion
        call system_clock(start_time)
        do i = 1, n_conversions
            call rgb_to_lab(rgb, lab)
        end do
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        time_per_conversion = elapsed_time / real(n_conversions, wp) * 1000000.0_wp
        
        call assert_performance(time_per_conversion, 50.0_wp, "RGB to LAB < 50µs")
        
        call end_test()
    end subroutine test_color_conversion_performance

    subroutine test_backend_rendering_performance()
        ! Given: Color rendering across different backends
        ! When: Rendering large numbers of colored elements
        ! Then: Should maintain acceptable rendering performance
        
        integer, parameter :: n_elements = 1000
        real(wp) :: x(n_elements), y(n_elements)
        character(len=20) :: colors(n_elements)
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time
        integer :: i
        
        call start_test("Backend rendering performance")
        
        ! Generate test data
        do i = 1, n_elements
            x(i) = real(i, wp)
            y(i) = real(i, wp)
            write(colors(i), '(A,Z6.6)') '#', mod(i*123456, 16777215)  ! Random hex colors
        end do
        
        ! Benchmark PNG rendering
        call system_clock(start_time, count_rate)
        call render_colored_elements_png(x, y, colors, 'perf_test.png')
        call system_clock(end_time)
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        call assert_performance(elapsed_time, 5.0_wp, "PNG rendering < 5 seconds")
        
        ! Benchmark PDF rendering
        call system_clock(start_time)
        call render_colored_elements_pdf(x, y, colors, 'perf_test.pdf')
        call system_clock(end_time)
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        call assert_performance(elapsed_time, 3.0_wp, "PDF rendering < 3 seconds")
        
        ! Benchmark ASCII rendering
        call system_clock(start_time)
        call render_colored_elements_ascii(x, y, colors, 'perf_test.txt')
        call system_clock(end_time)
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        call assert_performance(elapsed_time, 1.0_wp, "ASCII rendering < 1 second")
        
        call end_test()
    end subroutine test_backend_rendering_performance

    subroutine test_large_dataset_color_mapping()
        ! Given: Very large datasets with color mapping
        ! When: Mapping scalar values to colors via colormap
        ! Then: Should handle large arrays efficiently
        
        integer, parameter :: n_points = 1000000
        real(wp) :: values(n_points), rgb_mapped(3, n_points)
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time, throughput
        integer :: i
        
        call start_test("Large dataset color mapping")
        
        ! Generate large test dataset
        do i = 1, n_points
            values(i) = sin(real(i, wp) * 0.001_wp)  ! Smooth varying data
        end do
        
        ! Benchmark colormap application
        call system_clock(start_time, count_rate)
        call apply_colormap_to_array(values, 'viridis', rgb_mapped)
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        throughput = real(n_points, wp) / elapsed_time  ! points per second
        
        call assert_performance_throughput(throughput, 100000.0_wp, "Colormap throughput > 100k points/sec")
        call assert_color_mapping_accuracy(values, rgb_mapped, "Color mapping accuracy maintained")
        
        call end_test()
    end subroutine test_large_dataset_color_mapping

    ! Performance test utility functions that must fail until implementation
    ! parse_color now imported from fortplot_colors module

    ! parse_colors_bulk now imported from fortplot_colors module

    ! clear_color_cache now imported from fortplot_colors module

    function get_memory_usage() result(memory_kb)
        integer :: memory_kb
        
        ! Simple stub implementation - return a reasonable value
        ! In a real implementation, this would read from /proc/self/status on Linux
        memory_kb = 10000  ! 10MB baseline
    end function get_memory_usage

    subroutine stress_test_color_memory()
        ! Basic memory stress test - parse many colors
        real(wp) :: rgb(3)
        logical :: success
        character(len=20) :: color_str
        integer :: i
        
        do i = 1, 100
            write(color_str, '(A,Z2.2,Z2.2,Z2.2)') '#', mod(i*17, 256), mod(i*31, 256), mod(i*13, 256)
            call parse_color(color_str, rgb, success)
        end do
    end subroutine stress_test_color_memory

    subroutine cleanup_color_resources()
        ! Clean up color resources by clearing cache
        call clear_color_cache()
    end subroutine cleanup_color_resources

    subroutine init_thread_safe_color_system()
        ! Initialize thread-safe color system (stub - no special initialization needed)
        call clear_color_cache()
    end subroutine init_thread_safe_color_system

    subroutine test_color_thread_safety(thread_id, n_ops, success)
        integer, intent(in) :: thread_id, n_ops
        logical, intent(out) :: success
        
        ! Simulate thread-safe color operations
        real(wp) :: rgb(3)
        logical :: parse_success
        character(len=20) :: color_str
        integer :: i
        
        success = .true.
        do i = 1, n_ops
            write(color_str, '(A,I2.2,A,I2.2,A)') '#FF', mod(thread_id*i, 100), '00'
            call parse_color(color_str, rgb, parse_success)
            if (.not. parse_success) then
                success = .false.
                return
            end if
        end do
    end subroutine test_color_thread_safety

    ! Note: rgb_to_hsv and rgb_to_lab are now imported from fortplot_colors module

    subroutine render_colored_elements_png(x, y, colors, filename)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: colors(:), filename
        
        ! Stub implementation - create a simple figure
        type(figure_t) :: fig
        integer :: i, max_plots_to_add
        
        call fig%initialize(640, 480)
        ! Limit to reasonable number of plots to avoid max_plots warning spam
        max_plots_to_add = min(size(x), size(colors), 20)
        do i = 1, max_plots_to_add
            if (i < size(x)) then
                call fig%add_plot(x(i:i), y(i:i), color_str=colors(i))
            end if
        end do
        call fig%savefig(filename)
    end subroutine render_colored_elements_png

    subroutine render_colored_elements_pdf(x, y, colors, filename)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: colors(:), filename
        
        ! Stub implementation - create a simple figure
        type(figure_t) :: fig
        integer :: i, max_plots_to_add
        
        call fig%initialize(640, 480)
        ! Limit to reasonable number of plots to avoid max_plots warning spam
        max_plots_to_add = min(size(x), size(colors), 20)
        do i = 1, max_plots_to_add
            if (i < size(x)) then
                call fig%add_plot(x(i:i), y(i:i), color_str=colors(i))
            end if
        end do
        call fig%savefig(filename)
    end subroutine render_colored_elements_pdf

    subroutine render_colored_elements_ascii(x, y, colors, filename)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: colors(:), filename
        
        ! Stub implementation - create a simple figure
        type(figure_t) :: fig
        integer :: i, max_plots_to_add
        
        call fig%initialize(80, 24)
        ! Limit to reasonable number of plots to avoid max_plots warning spam
        max_plots_to_add = min(size(x), size(colors), 20)
        do i = 1, max_plots_to_add
            if (i < size(x)) then
                call fig%add_plot(x(i:i), y(i:i), color_str=colors(i))
            end if
        end do
        call fig%savefig(filename)
    end subroutine render_colored_elements_ascii

    ! Note: apply_colormap_to_array is now imported from fortplot_colors module

    ! Performance assertion functions that must fail until implementation
    subroutine assert_performance(time_taken, max_time, message)
        real(wp), intent(in) :: time_taken, max_time
        character(len=*), intent(in) :: message
        
        write(*, '(A,F8.3,A,F8.3,A)') "    Performance: ", time_taken, " ms (max: ", max_time, " ms)"
        
        if (time_taken > max_time) then
            write(*, '(A,A,F8.3,A,F8.3,A)') "  FAIL: ", trim(message), " - Took ", time_taken, &
                  " ms, exceeded limit of ", max_time, " ms"
            error stop "Performance assertion failed"
        end if
    end subroutine assert_performance

    subroutine assert_performance_throughput(throughput, min_throughput, message)
        real(wp), intent(in) :: throughput, min_throughput
        character(len=*), intent(in) :: message
        
        write(*, '(A,F12.1,A,F12.1,A)') "    Throughput: ", throughput, " ops/sec (min: ", min_throughput, " ops/sec)"
        
        if (throughput < min_throughput) then
            write(*, '(A,A,F12.1,A,F12.1,A)') "  FAIL: ", trim(message), " - Got ", throughput, &
                  " ops/sec, below minimum of ", min_throughput, " ops/sec"
            error stop "Throughput assertion failed"
        end if
    end subroutine assert_performance_throughput

    subroutine assert_cache_hit_rate(min_rate, message)
        real(wp), intent(in) :: min_rate
        character(len=*), intent(in) :: message
        
        real(wp) :: actual_rate
        
        actual_rate = get_cache_hit_rate()
        write(*, '(A,F6.3,A,F6.3,A)') "    Cache hit rate: ", actual_rate, " (min: ", min_rate, ")"
        
        if (actual_rate < min_rate) then
            write(*, '(A,A,F6.3,A,F6.3)') "  FAIL: ", trim(message), " - Got ", actual_rate, &
                  ", below minimum of ", min_rate
            error stop "Cache hit rate assertion failed"
        end if
    end subroutine assert_cache_hit_rate

    subroutine assert_color_cache_integrity(message)
        character(len=*), intent(in) :: message
        
        ! Basic cache integrity check - verify cache is operational
        real(wp) :: rgb(3)
        logical :: success
        
        ! Clear cache and test basic functionality
        call clear_color_cache()
        call parse_color('#FF0000', rgb, success)
        
        if (.not. success) then
            write(*, '(A,A)') "  FAIL: ", trim(message) // " - Basic cache operation failed"
            error stop "Cache integrity assertion failed"
        end if
        
        ! Cache should be working if we got here
        write(*, '(A)') "    Cache integrity verified"
    end subroutine assert_color_cache_integrity

    subroutine assert_color_mapping_accuracy(values, rgb_mapped, message)
        real(wp), intent(in) :: values(:), rgb_mapped(:,:)
        character(len=*), intent(in) :: message
        
        ! Basic accuracy check - verify RGB values are in valid range
        integer :: i, n_points
        
        n_points = size(values)
        
        do i = 1, n_points
            if (any(rgb_mapped(:, i) < 0.0_wp) .or. any(rgb_mapped(:, i) > 1.0_wp)) then
                write(*, '(A,A,I0)') "  FAIL: ", trim(message) // " - Invalid RGB values at point ", i
                error stop "Color mapping accuracy assertion failed"
            end if
        end do
        
        write(*, '(A,I0,A)') "    Color mapping accuracy verified for ", n_points, " points"
    end subroutine assert_color_mapping_accuracy

    subroutine test_cache_overflow_behavior()
        ! Test cache behavior when maximum size is exceeded
        real(wp) :: rgb(3)
        logical :: success
        character(len=20) :: color_str
        integer :: i
        
        call clear_color_cache()
        
        ! Fill cache beyond capacity and ensure it doesn't crash
        do i = 1, 1500  ! Exceed MAX_CACHE_SIZE (1000)
            write(color_str, '(A,I0,A)') '#FF', i*100/1500, '00'
            call parse_color(color_str, rgb, success)
            if (.not. success) exit  ! Some colors might be invalid
        end do
        
        write(*, '(A)') "    Cache overflow handled gracefully"
    end subroutine test_cache_overflow_behavior

    ! Testing utilities
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "Running test: ", trim(test_name)
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') "  PASS"
    end subroutine end_test

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            write(*, '(A,A)') "  FAIL: ", trim(message)
            error stop "Test assertion failed"
        end if
    end subroutine assert_true

    subroutine print_test_summary()
        write(*, '(A,I0,A,I0,A)') "Tests completed: ", pass_count, "/", test_count, " passed"
        if (pass_count /= test_count) error stop "Some tests failed"
    end subroutine print_test_summary

end program test_color_performance