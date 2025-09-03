program subplots_demo
    !! Demonstrates the use of subplots() function for creating subplot grids
    !! 
    !! This example shows how to create a grid of subplots using the
    !! matplotlib-compatible stateful API subplots() function
    
    use fortplot
    implicit none
    
    real(8) :: x(100), y1(100), y2(100), y3(100), y4(100) 
    real(8) :: pi
    integer :: i
    
    pi = 4.0d0 * atan(1.0d0)
    
    ! Generate test data
    do i = 1, 100
        x(i) = (i - 1) * 2.0d0 * pi / 99.0d0
        y1(i) = sin(x(i))
        y2(i) = cos(x(i)) 
        y3(i) = sin(2*x(i))
        y4(i) = cos(2*x(i))
    end do
    
    ! Create a 2x2 grid of subplots
    print *, "Creating 2x2 subplot grid..."
    call subplots(2, 2)
    
    ! Note: Currently the matplotlib-style API doesn't support 
    ! targeting specific subplots after creation. This example
    ! demonstrates that the function can be called successfully
    ! and creates the subplot grid structure on the figure.
    
    ! Add a plot to demonstrate the grid exists
    call plot(x, y1, label="sin(x)")
    call xlabel("x")
    call ylabel("y")
    call title("Subplot Grid Demo")
    call legend()
    call grid()
    
    ! Save the figure
    call savefig("subplots_demo.png")
    print *, "Figure saved as subplots_demo.png"
    
    print *, "Demo completed successfully!"
    
end program subplots_demo