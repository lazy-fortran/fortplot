program test_pcolormesh_validation
    ! Test pcolormesh dimension validation that was supposedly fixed in issue #698
    use fortplot
    implicit none
    
    real(wp), dimension(4) :: x_coords
    real(wp), dimension(3) :: y_coords  
    real(wp), dimension(2,3) :: z_data  ! Wrong dimensions on purpose
    
    print *, "Testing pcolormesh with wrong dimensions (should fail gracefully)"
    
    ! Set up test data with dimension mismatch
    x_coords = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]  ! 4 elements
    y_coords = [0.0_wp, 1.0_wp, 2.0_wp]          ! 3 elements
    z_data = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp], [2,3])  ! Wrong size
    
    call figure()
    
    ! This should fail with a clear error message, not segfault
    call pcolormesh(x_coords, y_coords, z_data)
    call savefig("test/output/test_pcolormesh_dimensions.png")
    
    print *, "Pcolormesh dimension test completed"
    
end program test_pcolormesh_validation