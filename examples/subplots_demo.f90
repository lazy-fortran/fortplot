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
    
    ! Create a 2x2 grid of subplots and target each panel
    print *, "Creating 2x2 subplot grid..."
    call subplots(2, 2)

    ! Panel (1,1)
    call subplot(2, 2, 1)
    call plot(x, y1, label="sin(x)")
    call xlabel("x")
    call ylabel("y")
    call title("sin(x)")
    call grid(.true.)

    ! Panel (1,2)
    call subplot(2, 2, 2)
    call plot(x, y2, label="cos(x)")
    call xlabel("x")
    call ylabel("y")
    call title("cos(x)")
    call grid(.true.)

    ! Panel (2,1)
    call subplot(2, 2, 3)
    call plot(x, y3, label="sin(2x)")
    call xlabel("x")
    call ylabel("y")
    call title("sin(2x)")
    call grid(.true.)

    ! Panel (2,2)
    call subplot(2, 2, 4)
    call plot(x, y4, label="cos(2x)")
    call xlabel("x")
    call ylabel("y")
    call title("cos(2x)")
    call grid(.true.)
    
    ! Save the figure
    call savefig("subplots_demo.png")
    print *, "Figure saved as subplots_demo.png"
    
    print *, "Demo completed successfully!"
    
end program subplots_demo
