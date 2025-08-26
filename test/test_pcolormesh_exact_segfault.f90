program test_pcolormesh_exact_segfault
    !! Test to reproduce exact segfault scenario from issue #430
    !! Force the path where dimension validation fails but get_data_range is called
    
    use fortplot_pcolormesh, only: pcolormesh_t
    use iso_fortran_env, only: wp => real64, error_unit
    implicit none
    
    type(pcolormesh_t) :: mesh
    real(wp) :: x_coords(6), y_coords(5), c_data(5,4)
    integer :: i, j
    
    print *, "Testing exact segfault scenario from issue #430..."
    print *, "Using mismatched arrays: x(6), y(5), c(5,4)"
    
    ! Initialize test data  
    do i = 1, 6
        x_coords(i) = real(i-1, wp)
    end do
    
    do i = 1, 5
        y_coords(i) = real(i-1, wp)  
    end do
    
    do i = 1, 5
        do j = 1, 4
            c_data(i, j) = real(i + j, wp)
        end do
    end do
    
    print *, "Attempting initialization with dimension mismatch..."
    
    ! This should fail validation, but let's see what happens
    ! if we force dimensions and call get_data_range directly
    
    ! Force dimensions to be wrong
    mesh%nx = 5  ! This will be wrong for x(6)
    mesh%ny = 4  ! This will be wrong for y(5)
    
    ! Allocate wrong-sized arrays to create mismatch
    allocate(mesh%c_values(4, 5))  ! Note: this is (ny, nx) = (4, 5)
    mesh%c_values = c_data  ! This should cause issues due to shape mismatch
    
    print *, "Calling get_data_range with mismatched dimensions..."
    call mesh%get_data_range()
    
    print *, "Test completed without crash"
    
end program test_pcolormesh_exact_segfault