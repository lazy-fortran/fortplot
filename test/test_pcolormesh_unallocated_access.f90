program test_pcolormesh_unallocated_access
    !! Test direct access to get_data_range on unallocated arrays
    !! This reproduces the actual segfault scenario from issue #430
    
    use fortplot_pcolormesh, only: pcolormesh_t
    use iso_fortran_env, only: wp => real64, error_unit
    implicit none
    
    type(pcolormesh_t) :: mesh
    logical :: test_passed
    
    test_passed = .true.
    
    print *, "Testing unallocated array access in get_data_range..."
    print *, "WARNING: This may cause segmentation fault before fix!"
    
    ! Create empty mesh object with unallocated arrays
    ! This simulates what happens when initialization fails
    ! but get_data_range is called anyway
    
    print *, "Calling get_data_range on unallocated c_values array..."
    print *, "(This should handle gracefully, not crash)"
    
    ! This is the actual line that causes the segfault!
    ! Without protection, this will crash with segmentation fault
    call mesh%get_data_range()
    
    print *, "PASS: get_data_range handled unallocated arrays gracefully"
    
end program test_pcolormesh_unallocated_access