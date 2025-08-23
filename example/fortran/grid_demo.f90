program grid_demo
    !! Example demonstrating grid line capabilities
    !! Shows basic grids, customization, and axis-specific grids
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fp
    real(wp) :: x(20), y1(20), y2(20)
    integer :: i
    
    ! Create test data
    do i = 1, 20
        x(i) = real(i - 1, wp) * 0.5_wp
        y1(i) = sin(x(i)) * exp(-x(i) * 0.1_wp)
        y2(i) = cos(x(i)) * 0.8_wp
    end do
    
    ! Basic plot (PNG)
    call fp%figure(figsize=[8.0_wp, 6.0_wp])
    call fp%add_plot(x, y1, label='Damped sine')
    call fp%add_plot(x, y2, label='Cosine')
    call fp%legend()
    call fp%title('Basic Plot')
    call fp%xlabel('Time (s)')
    call fp%ylabel('Amplitude')
    call fp%savefig('build/example/grid_demo/grid_basic.png')
    write(*,*) 'Created grid_basic.png'
    
    ! Basic plot (PDF)
    call fp%figure(figsize=[8.0_wp, 6.0_wp])
    call fp%add_plot(x, y1, label='Damped sine')
    call fp%add_plot(x, y2, label='Cosine')
    call fp%legend()
    call fp%title('Basic Plot')
    call fp%xlabel('Time (s)')
    call fp%ylabel('Amplitude')
    call fp%savefig('build/example/grid_demo/grid_basic.pdf')
    write(*,*) 'Created grid_basic.pdf'
    
    write(*,*) 'Grid demonstration completed!'
    
end program grid_demo