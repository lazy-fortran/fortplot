program label_positioning_demo
    ! Demonstrates proper label positioning within canvas bounds
    use fortplot
    implicit none
    
    real(wp), dimension(50) :: x, y
    integer :: i
    
    ! Generate test data
    x = [(real(i, wp) * 0.2_wp, i=1, 50)]
    do i = 1, 50
        y(i) = sin(x(i)) * exp(-x(i)/10.0_wp)
    end do
    
    ! Create figure with properly positioned labels
    call figure()
    call plot(x, y)
    call xlabel('Time (seconds)')
    call ylabel('Amplitude')  
    call title('Damped Sine Wave')
    
    ! Save figure
    call savefig('output/label_positioning_demo.png')
    
    print *, 'Label positioning demo saved to output/label_positioning_demo.png'
    print *, 'Labels should be fully visible within canvas bounds with proper spacing'
    
end program label_positioning_demo