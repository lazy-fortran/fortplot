program test_3d_tick_orientation
    !! Test to verify 3D axis tick orientations
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    real(wp), allocatable :: x(:), y(:), z(:)
    integer :: i, n
    
    print *, "Testing 3D tick orientations..."
    
    ! Create simple 3D helix data
    n = 50
    allocate(x(n), y(n), z(n))
    
    do i = 1, n
        x(i) = cos(real(i-1, wp) * 0.2_wp)
        y(i) = sin(real(i-1, wp) * 0.2_wp)
        z(i) = real(i-1, wp) * 0.1_wp
    end do
    
    ! Create 3D plot with clear axis ranges to test tick orientation
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_3d_plot(x, y, z, label="Test Helix")
    call title("3D Tick Orientation Test")
    
    ! Save to verify tick directions
    call savefig('test_3d_ticks.png')
    call savefig('test_3d_ticks.pdf')
    
    print *, "Created test_3d_ticks.png and test_3d_ticks.pdf"
    print *, "✓ Z-axis ticks should point horizontally (leftward)"
    print *, "✓ Y-axis ticks should point horizontally (leftward)"  
    print *, "✓ X-axis ticks should point vertically (downward)"
    print *, "✓ Labels should be properly spaced away from ticks"
    
    deallocate(x, y, z)
end program test_3d_tick_orientation