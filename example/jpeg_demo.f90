program jpeg_demo
    use fortplot
    implicit none
    
    real(wp) :: x(100), y(100)
    integer :: i
    
    write(*,*) 'Creating JPEG demo plot...'
    
    ! Generate simple sine wave data
    do i = 1, 100
        x(i) = real(i-1, wp) * 0.1_wp
        y(i) = sin(x(i))
    end do
    
    ! Create figure and plot using functional API
    call figure()
    call plot(x, y, label='sin(x)')
    call xlabel('x')
    call ylabel('sin(x)')
    call title('JPEG Output Demo with Huffman Encoding')
    
    ! Save as JPEG 
    call savefig('jpeg_demo_output.jpg')
    
    write(*,*) 'JPEG demo completed! Output saved as jpeg_demo_output.jpg'
    write(*,*) 'The JPEG now uses proper Huffman encoding from STB implementation.'
    
end program jpeg_demo