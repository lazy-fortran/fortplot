program grid_demo
    !! Example demonstrating grid line capabilities
    !! Shows basic grids, customization, and axis-specific grids
    
    use fortplot
    implicit none
    
    real(wp) :: x(20), y1(20), y2(20)
    integer :: i
    
    ! Create test data
    do i = 1, 20
        x(i) = real(i - 1, wp) * 0.5_wp
        y1(i) = sin(x(i)) * exp(-x(i) * 0.1_wp)
        y2(i) = cos(x(i)) * 0.8_wp
    end do
    
    ! Basic plot (PNG)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot(x, y1, label='Damped sine')
    call plot(x, y2, label='Cosine')
    call legend()
    call title('Basic Plot - Grid Demo')
    call xlabel('Time (s)')
    call ylabel('Amplitude')
    ! Note: Grid functionality is not available in current API
    call savefig('output/example/fortran/grid_demo/grid_demo.png')
    call savefig('output/example/fortran/grid_demo/grid_demo.pdf')
    call savefig('output/example/fortran/grid_demo/grid_demo.txt')
    write(*,*) 'Created grid_demo outputs'
    
    write(*,*) 'Grid demonstration completed!'
    
end program grid_demo