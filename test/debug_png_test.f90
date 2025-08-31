program debug_png_test
    use fortplot
    implicit none
    
    real(wp) :: x(10), y(10)
    integer :: i
    
    ! Create simple test data
    do i = 1, 10
        x(i) = real(i, wp)
        y(i) = real(i*i, wp)
    end do
    
    ! Create figure and plot
    call figure()  ! Initialize global figure
    call plot(x, y)
    call savefig('debug_test.png')
    
    print *, 'Debug PNG created: debug_test.png'
end program debug_png_test