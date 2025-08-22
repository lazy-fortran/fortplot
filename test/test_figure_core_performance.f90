program test_figure_core_performance
    !! Performance regression test suite for fortplot_figure_core
    !! 
    !! GIVEN: Current fortplot_figure_core has specific performance characteristics
    !! WHEN: Module is refactored into focused modules
    !! THEN: Performance must not degrade significantly
    !!
    !! Tests performance-critical operations to ensure refactoring
    !! maintains or improves performance.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    ! Performance tests
    call test_figure_initialization_performance()
    call test_large_dataset_performance()
    call test_multiple_plot_performance()
    call test_contour_data_performance()
    call test_memory_allocation_performance()
    call test_repeated_operations_performance()
    call test_complex_plot_performance()
    
    call print_test_summary()
    
contains

    subroutine test_figure_initialization_performance()
        type(figure_t) :: fig
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time
        integer, parameter :: num_iterations = 1000
        integer :: i
        
        call start_test("Figure initialization performance")
        
        ! GIVEN: Multiple figure initializations
        call system_clock(start_time, count_rate)
        
        ! WHEN: Many figures are initialized
        do i = 1, num_iterations
            call fig%initialize(640, 480)
        end do
        
        call system_clock(end_time)
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        
        ! THEN: Should complete within reasonable time
        call assert_true(elapsed_time < 1.0_wp, "Initialization performance")
        write(*, '(A, F8.4, A)') '  Initialization time: ', elapsed_time, ' seconds'
        
        call end_test()
    end subroutine test_figure_initialization_performance

    subroutine test_large_dataset_performance()
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:)
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time
        integer, parameter :: dataset_size = 50000
        integer :: i
        
        call start_test("Large dataset performance")
        
        call fig%initialize()
        
        ! Create large dataset
        allocate(x(dataset_size))
        allocate(y(dataset_size))
        
        do i = 1, dataset_size
            x(i) = real(i, wp) * 0.001_wp
            y(i) = sin(x(i)) * cos(x(i) * 0.5_wp)
        end do
        
        ! GIVEN: Large dataset
        call system_clock(start_time, count_rate)
        
        ! WHEN: Large dataset is added to plot
        call fig%add_plot(x, y, label="large_dataset")
        
        call system_clock(end_time)
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        
        ! THEN: Should handle large data efficiently
        call assert_true(elapsed_time < 0.1_wp, "Large dataset performance")
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Large dataset added")
        write(*, '(A, F8.4, A)') '  Large dataset time: ', elapsed_time, ' seconds'
        
        call end_test()
    end subroutine test_large_dataset_performance

    subroutine test_multiple_plot_performance()
        type(figure_t) :: fig
        real(wp) :: x(100), y(100)
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time
        integer, parameter :: num_plots = 100
        integer :: i, j
        
        call start_test("Multiple plot performance")
        
        call fig%initialize()
        
        ! Create test data
        do i = 1, 100
            x(i) = real(i, wp)
            y(i) = sin(real(i, wp) * 0.1_wp)
        end do
        
        ! GIVEN: Multiple plots to add
        call system_clock(start_time, count_rate)
        
        ! WHEN: Many plots are added
        do j = 1, num_plots
            call fig%add_plot(x, y + real(j, wp), label="plot_"//trim(str(j)))
        end do
        
        call system_clock(end_time)
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        
        ! THEN: Should add multiple plots efficiently
        call assert_true(elapsed_time < 0.5_wp, "Multiple plot performance")
        write(*, '(A, F8.4, A)') '  Multiple plots time: ', elapsed_time, ' seconds'
        
        call end_test()
    end subroutine test_multiple_plot_performance

    subroutine test_contour_data_performance()
        type(figure_t) :: fig
        real(wp) :: x_grid(50), y_grid(50), z_grid(50,50)
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time
        integer :: i, j
        
        call start_test("Contour data performance")
        
        call fig%initialize()
        
        ! Create contour data
        do i = 1, 50
            x_grid(i) = real(i, wp) * 0.1_wp
            y_grid(i) = real(i, wp) * 0.1_wp
            do j = 1, 50
                z_grid(i,j) = sin(x_grid(i)) * cos(y_grid(j))
            end do
        end do
        
        ! GIVEN: Contour grid data
        call system_clock(start_time, count_rate)
        
        ! WHEN: Contour plot is added
        call figure_add_contour_filled(fig, x_grid, y_grid, z_grid)
        
        call system_clock(end_time)
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        
        ! THEN: Should handle contour data efficiently
        call assert_true(elapsed_time < 0.05_wp, "Contour data performance")
        write(*, '(A, F8.4, A)') '  Contour data time: ', elapsed_time, ' seconds'
        
        call end_test()
    end subroutine test_contour_data_performance

    subroutine test_memory_allocation_performance()
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:)
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time
        integer, parameter :: alloc_size = 10000
        integer, parameter :: num_allocations = 50
        integer :: i, j
        
        call start_test("Memory allocation performance")
        
        call fig%initialize()
        
        ! GIVEN: Multiple memory allocations
        call system_clock(start_time, count_rate)
        
        ! WHEN: Multiple allocations and deallocations occur
        do i = 1, num_allocations
            allocate(x(alloc_size))
            allocate(y(alloc_size))
            
            do j = 1, alloc_size
                x(j) = real(j, wp)
                y(j) = real(j**2, wp)
            end do
            
            call fig%add_plot(x, y, label="alloc_test_"//trim(str(i)))
            
            deallocate(x)
            deallocate(y)
        end do
        
        call system_clock(end_time)
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        
        ! THEN: Memory operations should be efficient
        call assert_true(elapsed_time < 2.0_wp, "Memory allocation performance")
        write(*, '(A, F8.4, A)') '  Memory allocation time: ', elapsed_time, ' seconds'
        
        call end_test()
    end subroutine test_memory_allocation_performance

    subroutine test_repeated_operations_performance()
        type(figure_t) :: fig
        real(wp) :: x(10), y(10)
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time
        integer, parameter :: num_operations = 1000
        integer :: i
        
        call start_test("Repeated operations performance")
        
        call fig%initialize()
        
        ! Create small dataset
        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = real(i, wp)
        end do
        
        call fig%add_plot(x, y, label="base_plot")
        
        ! GIVEN: Repeated figure operations
        call system_clock(start_time, count_rate)
        
        ! WHEN: Many operations are performed
        do i = 1, num_operations
            call fig%set_xlim(0.0_wp, 10.0_wp)
            call fig%set_ylim(0.0_wp, 10.0_wp)
            call fig%set_xlabel("X Label")
            call fig%set_ylabel("Y Label")
            call fig%set_title("Title")
        end do
        
        call system_clock(end_time)
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        
        ! THEN: Repeated operations should be fast
        call assert_true(elapsed_time < 0.1_wp, "Repeated operations performance")
        write(*, '(A, F8.4, A)') '  Repeated operations time: ', elapsed_time, ' seconds'
        
        call end_test()
    end subroutine test_repeated_operations_performance

    subroutine test_complex_plot_performance()
        type(figure_t) :: fig
        real(wp) :: x(1000), y(1000), z(1000)
        real(wp) :: x_grid(20), y_grid(20), z_grid(20,20)
        real(wp) :: scatter_sizes(1000), scatter_colors(1000)
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_time
        integer :: i, j
        
        call start_test("Complex plot performance")
        
        call fig%initialize()
        
        ! Create complex data
        do i = 1, 1000
            x(i) = real(i, wp) * 0.01_wp
            y(i) = sin(x(i)) + cos(x(i) * 2.0_wp)
            z(i) = x(i) * y(i)
            scatter_sizes(i) = 10.0_wp + real(mod(i, 50), wp)
            scatter_colors(i) = real(mod(i, 100), wp) / 100.0_wp
        end do
        
        do i = 1, 20
            x_grid(i) = real(i, wp) * 0.1_wp
            y_grid(i) = real(i, wp) * 0.1_wp
            do j = 1, 20
                z_grid(i,j) = sin(x_grid(i)) * cos(y_grid(j))
            end do
        end do
        
        ! GIVEN: Complex multi-plot figure
        call system_clock(start_time, count_rate)
        
        ! WHEN: Multiple complex plots are added
        call fig%add_plot(x, y, label="line_plot")
        call fig%add_3d_plot(x, y, z, label="3d_plot")
        call fig%add_scatter_2d(x, y, label="scatter_plot")
        call figure_add_contour_filled(fig, x_grid, y_grid, z_grid)
        call fig%set_xlabel("Complex X")
        call fig%set_ylabel("Complex Y")
        call fig%set_title("Complex Plot")
        
        call system_clock(end_time)
        elapsed_time = real(end_time - start_time, wp) / real(count_rate, wp)
        
        ! THEN: Complex operations should complete reasonably fast
        call assert_true(elapsed_time < 0.2_wp, "Complex plot performance")
        call assert_equal(real(fig%plot_count, wp), 4.0_wp, "Complex plots added")
        write(*, '(A, F8.4, A)') '  Complex plot time: ', elapsed_time, ' seconds'
        
        call end_test()
    end subroutine test_complex_plot_performance

    ! Helper function to convert integer to string
    function str(i) result(s)
        integer, intent(in) :: i
        character(len=20) :: s
        write(s, '(I0)') i
        s = trim(s)
    end function str

    ! Test helper subroutines
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        write(*, '(A, A)') 'Running test: ', test_name
    end subroutine start_test

    subroutine end_test()
        write(*, '(A)') 'Test completed'
        write(*, *)
    end subroutine end_test

    subroutine assert_equal(actual, expected, description)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: description
        real(wp), parameter :: tolerance = 1.0e-10_wp
        
        test_count = test_count + 1
        if (abs(actual - expected) < tolerance) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A, F12.6, A, F12.6)') '  FAIL: ', description, actual, ' != ', expected
        end if
    end subroutine assert_equal

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (condition) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A)') '  FAIL: ', description
        end if
    end subroutine assert_true

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A, I0, A, I0, A)') 'Test Summary: ', pass_count, ' of ', test_count, ' tests passed'
        if (pass_count == test_count) then
            write(*, '(A)') 'All tests PASSED!'
        else
            write(*, '(A)') 'Some tests FAILED!'
        end if
    end subroutine print_test_summary

end program test_figure_core_performance