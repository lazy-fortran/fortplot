program jpeg_quality_demo
    use fortplot
    implicit none
    
    real(wp) :: x(100), y(100)
    integer :: i
    
    write(*,*) 'Creating JPEG quality comparison...'
    
    ! Generate simple sine wave data
    do i = 1, 100
        x(i) = real(i-1, wp) * 0.1_wp
        y(i) = sin(x(i))
    end do
    
    ! Test very low quality (high compression, visible artifacts)
    call figure()
    call plot(x, y, 'b-')
    call xlabel('x')
    call ylabel('sin(x)')
    call title('JPEG Quality 1 - Maximum Compression Artifacts')
    call savefig('jpeg_quality_01.jpg')  ! Quality 1 (worst)
    
    ! Test low quality 
    call figure()
    call plot(x, y, 'r-')
    call xlabel('x') 
    call ylabel('sin(x)')
    call title('JPEG Quality 10 - Heavy Compression')
    call savefig('jpeg_quality_10.jpg')  ! Quality 10
    
    ! Test medium quality
    call figure()
    call plot(x, y, 'g-')
    call xlabel('x')
    call ylabel('sin(x)')
    call title('JPEG Quality 50 - Medium Compression')
    call savefig('jpeg_quality_50.jpg')  ! Quality 50
    
    ! Test high quality (minimal compression)
    call figure() 
    call plot(x, y, 'k-')
    call xlabel('x')
    call ylabel('sin(x)')
    call title('JPEG Quality 95 - Minimal Compression')
    call savefig('jpeg_quality_95.jpg')  ! Quality 95
    
    write(*,*) 'JPEG quality comparison completed!'
    write(*,*) 'Files created:'
    write(*,*) '  jpeg_quality_01.jpg - Quality 1 (maximum artifacts)'
    write(*,*) '  jpeg_quality_10.jpg - Quality 10 (heavy compression)'
    write(*,*) '  jpeg_quality_50.jpg - Quality 50 (medium compression)'
    write(*,*) '  jpeg_quality_95.jpg - Quality 95 (minimal artifacts)'
    write(*,*) ''
    write(*,*) 'Compare file sizes and visual quality to see JPEG compression in action!'
    
end program jpeg_quality_demo