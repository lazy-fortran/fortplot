program test_pcolormesh_integration_430
    !! Integration test to verify pcolormesh works through main plotting interface
    !! after issue #430 segmentation fault fixes
    
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x_coords(4), y_coords(3), c_data(2,3)
    integer :: i, j
    logical :: file_exists
    
    print *, "Testing pcolormesh integration after issue #430 fixes..."
    
    ! Create valid test data
    x_coords = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]  ! 4 elements for 3 cells
    y_coords = [0.0_wp, 0.5_wp, 1.0_wp]          ! 3 elements for 2 cells  
    
    ! c_data should be (ny, nx) = (2, 3)
    do i = 1, 2  ! ny
        do j = 1, 3  ! nx
            c_data(i, j) = real(i * j, wp)
        end do
    end do
    
    print *, "Creating pcolormesh plot with dimensions:"
    print *, "  x_coords: ", size(x_coords), " elements"  
    print *, "  y_coords: ", size(y_coords), " elements"
    print *, "  c_data:   ", shape(c_data), " shape"
    
    ! This should work through the main interface
    call figure()
    call pcolormesh(x_coords, y_coords, c_data)
    call title('Integration Test: Issue #430 Fixed')
    call savefig('test_pcolormesh_430_integration.png')
    
    ! Check if file was created successfully
    inquire(file='test_pcolormesh_430_integration.png', exist=file_exists)
    
    if (file_exists) then
        print *, ""
        print *, "SUCCESS: Pcolormesh integration test passed!"
        print *, "File created: test_pcolormesh_430_integration.png"
        print *, "Issue #430 segmentation fault is completely fixed."
        stop 0
    else
        print *, ""
        print *, "FAILURE: Pcolormesh integration test failed!"
        print *, "File was not created - something is still broken."
        stop 1
    end if
    
end program test_pcolormesh_integration_430