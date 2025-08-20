program subplot_example
    !! Demonstrate subplot functionality with multi-plot figures
    !! Shows various subplot layouts and plot types
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure, subplot, plot, xlabel, ylabel, title, savefig, scatter, bar
    implicit none
    
    integer, parameter :: n = 20
    real(wp) :: x(n), y1(n), y2(n), y3(n), y4(n)
    integer :: i
    
    ! Generate sample data
    do i = 1, n
        x(i) = real(i-1, wp) * 0.5_wp
        y1(i) = sin(x(i))
        y2(i) = cos(x(i))
        y3(i) = x(i)**2 * 0.1_wp
        y4(i) = exp(-x(i) * 0.2_wp)
    end do
    
    ! Example 1: Basic 2x1 subplot layout (Issue #150 example)
    call figure(800, 600)
    
    ! First subplot
    call subplot(2, 1, 1)
    call plot(x, y1, label="sin(x)")
    call xlabel("X")
    call ylabel("Y1")
    call title("First subplot")
    
    ! Second subplot
    call subplot(2, 1, 2)
    call plot(x, y2, label="cos(x)")
    call xlabel("X")
    call ylabel("Y2")
    call title("Second subplot")
    
    call savefig("subplot_2x1_example.png")
    
    ! Example 2: 2x2 grid with different plot types
    call figure(800, 600)
    
    ! Top-left: Line plot
    call subplot(2, 2, 1)
    call plot(x, y1, label="sine")
    call title("Sine Wave")
    call xlabel("X")
    call ylabel("sin(X)")
    
    ! Top-right: Another line plot
    call subplot(2, 2, 2)
    call plot(x, y2, label="cosine")
    call title("Cosine Wave")
    call xlabel("X")
    call ylabel("cos(X)")
    
    ! Bottom-left: Quadratic
    call subplot(2, 2, 3)
    call plot(x, y3, label="quadratic")
    call title("Quadratic Function")
    call xlabel("X")
    call ylabel("X²")
    
    ! Bottom-right: Exponential decay
    call subplot(2, 2, 4)
    call plot(x, y4, label="exponential")
    call title("Exponential Decay")
    call xlabel("X")
    call ylabel("exp(-X)")
    
    call savefig("subplot_2x2_example.png")
    
    print *, "Subplot examples saved:"
    print *, "- subplot_2x1_example.png"
    print *, "- subplot_2x2_example.png"
    
end program subplot_example