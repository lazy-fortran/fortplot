program test_matplotlib_stubs
    !! Test that matplotlib stub implementations work correctly
    use fortplot_matplotlib
    use fortplot_logging, only: log_info
    implicit none
    
    real(8) :: x(10), y(10), data(100)
    integer :: i
    logical :: test_passed
    
    ! Generate test data
    do i = 1, 10
        x(i) = real(i-1, 8)
        y(i) = real((i-1)**2, 8)
    end do
    
    do i = 1, 100
        data(i) = real(i, 8) + 0.5d0 * sin(real(i, 8))
    end do
    
    test_passed = .true.
    call log_info("Testing matplotlib stub implementations...")
    
    ! Test scatter plot
    call test_scatter()
    
    ! Test bar plot
    call test_bar()
    
    ! Test horizontal bar plot
    call test_barh()
    
    ! Test histogram
    call test_hist()
    
    ! Test text annotation
    call test_text()
    
    ! Test arrow annotation
    call test_annotate()
    
    if (test_passed) then
        call log_info("All tests passed!")
    else
        call log_info("Some tests failed")
        stop 1
    end if
    
contains

    subroutine test_scatter()
        call log_info("Testing scatter plot...")
        call figure()
        call scatter(x, y, label="Test scatter")
        call title("Scatter Plot Test")
        call xlabel("X values")
        call ylabel("Y values")
        call savefig("test_scatter.png")
        call log_info("  scatter plot test complete")
    end subroutine test_scatter
    
    subroutine test_bar()
        call log_info("Testing bar plot...")
        call figure()
        call bar(x, y, label="Test bars")
        call title("Bar Plot Test")
        call xlabel("Categories")
        call ylabel("Values")
        call savefig("test_bar.png")
        call log_info("  bar plot test complete")
    end subroutine test_bar
    
    subroutine test_barh()
        call log_info("Testing horizontal bar plot...")
        call figure()
        call barh(x, y, label="Test horizontal bars")
        call title("Horizontal Bar Plot Test")
        call xlabel("Values")
        call ylabel("Categories")
        call savefig("test_barh.png")
        call log_info("  horizontal bar plot test complete")
    end subroutine test_barh
    
    subroutine test_hist()
        call log_info("Testing histogram...")
        call figure()
        call hist(data, bins=20, label="Test histogram")
        call title("Histogram Test")
        call xlabel("Values")
        call ylabel("Frequency")
        call savefig("test_hist.png")
        call log_info("  histogram test complete")
    end subroutine test_hist
    
    subroutine test_text()
        call log_info("Testing text annotation...")
        call figure()
        call plot(x, y)
        call text(5.0d0, 25.0d0, "Test annotation")
        call title("Text Annotation Test")
        call savefig("test_text.png")
        call log_info("  text annotation test complete")
    end subroutine test_text
    
    subroutine test_annotate()
        call log_info("Testing arrow annotation...")
        call figure()
        call plot(x, y)
        call annotate("Peak", [5.0d0, 25.0d0], [3.0d0, 30.0d0])
        call title("Arrow Annotation Test")
        call savefig("test_annotate.png")
        call log_info("  arrow annotation test complete")
    end subroutine test_annotate
    
end program test_matplotlib_stubs