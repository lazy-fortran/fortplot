program subplots_grid_demo
    !! Demonstrates the use of subplots_grid() to get handles for individual subplot access
    use fortplot
    implicit none
    
    integer :: i, j
    real(8) :: x(100), y1(100), y2(100), y3(100), y4(100)
    integer, allocatable :: axes(:,:)
    
    ! Create different data for each subplot
    do i = 1, 100
        x(i) = real(i-1, 8) * 0.1
        y1(i) = sin(x(i))
        y2(i) = cos(x(i))
        y3(i) = sin(2*x(i))
        y4(i) = cos(2*x(i))
    end do
    
    ! Create a 2x2 grid of subplots and get indices for individual access
    axes = subplots_grid(2, 2)
    
    ! First subplot: sin(x)
    call subplot(2, 2, axes(1,1))
    call plot(x, y1)
    call title('sin(x)')
    call xlabel('x')
    call ylabel('y')
    call grid(.true.)
    
    ! Second subplot: cos(x)
    call subplot(2, 2, axes(1,2))
    call plot(x, y2)
    call title('cos(x)')
    call xlabel('x')
    call ylabel('y')
    call grid(.true.)
    
    ! Third subplot: sin(2x)
    call subplot(2, 2, axes(2,1))
    call plot(x, y3)
    call title('sin(2x)')
    call xlabel('x')
    call ylabel('y')
    call grid(.true.)
    
    ! Fourth subplot: cos(2x)
    call subplot(2, 2, axes(2,2))
    call plot(x, y4)
    call title('cos(2x)')
    call xlabel('x')
    call ylabel('y')
    call grid(.true.)
    
    ! Save the figure
    call savefig('subplots_grid_demo.png')
    
    print *, 'Subplots grid demo saved to subplots_grid_demo.png'
    print *, 'Demonstrates individual subplot access using returned indices'
    
end program subplots_grid_demo