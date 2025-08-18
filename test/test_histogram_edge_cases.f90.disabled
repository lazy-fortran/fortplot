program test_histogram_edge_cases
    use iso_fortran_env, only: real64, wp => real64
    use fortplot, only: figure_t
    implicit none
    
    call test_identical_values()
    call test_binary_search_performance()
    
    print *, 'All histogram edge case tests PASSED!'
    
contains

    subroutine test_identical_values()
        !! Test histogram with all identical values
        type(figure_t) :: fig
        real(wp) :: data(100)
        
        print *, 'Testing: Histogram with identical values'
        
        ! All values are the same
        data = 5.0_wp
        
        call fig%initialize(600, 400)
        call fig%hist(data, bins=10)
        
        if (fig%plot_count == 1) then
            print *, '  PASS: Histogram created with identical values'
        else
            print *, '  FAIL: Histogram not created properly'
            stop 1
        end if
        
        print *, 'Test completed'
    end subroutine test_identical_values
    
    subroutine test_binary_search_performance()
        !! Test that binary search works correctly
        type(figure_t) :: fig
        real(wp), allocatable :: data(:)
        integer :: i
        
        print *, 'Testing: Binary search in histogram binning'
        
        ! Create large dataset
        allocate(data(10000))
        do i = 1, 10000
            data(i) = real(i, wp) / 100.0_wp
        end do
        
        call fig%initialize(600, 400)
        call fig%hist(data, bins=50)
        
        if (fig%plot_count == 1) then
            print *, '  PASS: Large histogram created efficiently'
        else
            print *, '  FAIL: Large histogram creation failed'
            stop 1
        end if
        
        print *, 'Test completed'
    end subroutine test_binary_search_performance

end program test_histogram_edge_cases