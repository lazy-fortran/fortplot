program example
    use fortplot
    implicit none
    
    real(8) :: x(100), y(100)
    integer :: i
    
    ! Generate data
    do i = 1, 100
        x(i) = real(i-1) * 0.1
        y(i) = sin(x(i))
    end do
    
    ! Create plot
    call figure()
    call plot(x, y)
    call title("Sine Function")
    call xlabel("x")
    call ylabel("sin(x)")
    call savefig("sine_plot.png")
    
end program example