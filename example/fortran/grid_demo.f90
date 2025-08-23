program grid_demo
    !! Example demonstrating grid line capabilities
    !! Shows basic grids, customization, and axis-specific grids
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(20), y1(20), y2(20)
    integer :: i
    
    ! Create test data
    do i = 1, 20
        x(i) = real(i - 1, wp) * 0.5_wp
        y1(i) = sin(x(i)) * exp(-x(i) * 0.1_wp)
        y2(i) = cos(x(i)) * 0.8_wp
    end do
    
    ! Basic plot with default grid (PNG)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_plot(x, y1, label='Damped sine')
    call add_plot(x, y2, label='Cosine')
    ! ! call grid(.true.)  ! Grid function not implemented yet
    call legend()
    call title('Basic Grid Lines')
    call xlabel('Time (s)')
    call ylabel('Amplitude')
    call savefig('build/example/grid_demo/grid_basic.png')
    write(*,*) 'Created grid_basic.png'
    
    ! Basic plot with default grid (PDF)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_plot(x, y1, label='Damped sine')
    call add_plot(x, y2, label='Cosine')
    ! ! call grid(.true.)  ! Grid function not implemented yet
    call legend()
    call title('Basic Grid Lines')
    call xlabel('Time (s)')
    call ylabel('Amplitude')
    call savefig('build/example/grid_demo/grid_basic.pdf')
    write(*,*) 'Created grid_basic.pdf'
    
    ! Grid with custom transparency
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_plot(x, y1, label='Damped sine')
    call add_plot(x, y2, label='Cosine')
    ! call grid(alpha=0.6_wp)
    call legend()
    call title('Grid with Custom Transparency (alpha=0.6)')
    call xlabel('Time (s)')
    call ylabel('Amplitude')
    call savefig('build/example/grid_demo/grid_custom_alpha.png')
    write(*,*) 'Created grid_custom_alpha.png'
    
    ! Grid with custom line style
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_plot(x, y1, label='Damped sine')
    call add_plot(x, y2, label='Cosine')
    ! call grid(linestyle='--', alpha=0.4_wp)
    call legend()
    call title('Grid with Dashed Lines')
    call xlabel('Time (s)')
    call ylabel('Amplitude')
    call savefig('build/example/grid_demo/grid_dashed.png')
    write(*,*) 'Created grid_dashed.png'
    
    ! X-axis grid only
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_plot(x, y1, label='Damped sine')
    call add_plot(x, y2, label='Cosine')
    ! call grid(axis='x')
    call legend()
    call title('X-Axis Grid Lines Only')
    call xlabel('Time (s)')
    call ylabel('Amplitude')
    call savefig('build/example/grid_demo/grid_x_only.png')
    write(*,*) 'Created grid_x_only.png'
    
    ! Y-axis grid only
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_plot(x, y1, label='Damped sine')
    call add_plot(x, y2, label='Cosine')
    ! call grid(axis='y')
    call legend()
    call title('Y-Axis Grid Lines Only')
    call xlabel('Time (s)')
    call ylabel('Amplitude')
    call savefig('build/example/grid_demo/grid_y_only.png')
    write(*,*) 'Created grid_y_only.png'
    
    ! Minor grid lines
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_plot(x, y1, label='Damped sine')
    call add_plot(x, y2, label='Cosine')
    ! call grid(which='minor', alpha=0.2_wp)
    call legend()
    call title('Minor Grid Lines')
    call xlabel('Time (s)')
    call ylabel('Amplitude')
    call savefig('build/example/grid_demo/grid_minor.png')
    write(*,*) 'Created grid_minor.png'
    
    write(*,*) 'Grid lines demonstration completed!'
    
end program grid_demo