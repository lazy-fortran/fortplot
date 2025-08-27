program test_no_error_stop
    !! Test that stub functions no longer call error_stop
    use fortplot_matplotlib
    implicit none
    
    real(8) :: x(5), y(5), data(20)
    integer :: i
    logical :: success
    
    ! Generate test data
    do i = 1, 5
        x(i) = real(i, 8)
        y(i) = real(i*i, 8)
    end do
    
    do i = 1, 20
        data(i) = real(i, 8) + sin(real(i, 8))
    end do
    
    success = .true.
    
    ! Test each function that previously had error_stop
    print *, "Testing bar()..."
    call bar(x, y)
    print *, "  bar() passed (no error_stop)"
    
    print *, "Testing barh()..."  
    call barh(x, y)
    print *, "  barh() passed (no error_stop)"
    
    print *, "Testing hist()..."
    call hist(data)
    print *, "  hist() passed (no error_stop)"
    
    print *, "Testing histogram()..."
    call histogram(data)
    print *, "  histogram() passed (no error_stop)"
    
    print *, "Testing boxplot()..."
    call boxplot(data)
    print *, "  boxplot() passed (no error_stop)"
    
    print *, "Testing scatter()..."
    call scatter(x, y)
    print *, "  scatter() passed (no error_stop)"
    
    print *, "Testing text()..."
    call text(2.5d0, 6.0d0, "Test")
    print *, "  text() passed (no error_stop)"
    
    print *, "Testing annotate()..."
    call annotate("Arrow", [3.0d0, 9.0d0], [2.0d0, 10.0d0])
    print *, "  annotate() passed (no error_stop)"
    
    print *, "Testing errorbar()..."
    call errorbar(x, y)
    print *, "  errorbar() passed (no error_stop)"
    
    print *, ""
    print *, "SUCCESS: All previously stubbed functions work without error_stop!"
    print *, "Issue #444 is resolved - no more crashes from stub implementations."
    
end program test_no_error_stop