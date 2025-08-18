program test_histogram_user_edge_cases
    !! Test edge cases that users might realistically encounter
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    real(wp) :: empty_data(0)
    real(wp) :: single_value(1) = [5.0_wp]
    real(wp) :: identical_values(10) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, &
                                        1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]
    real(wp) :: extreme_values(4) = [1.0e-10_wp, 1.0e10_wp, -1.0e10_wp, 1.0e-10_wp]
    type(figure_t) :: fig
    
    write(*,*) 'Testing user edge cases...'
    
    ! Test 1: Empty data - should not crash
    write(*,*) '  Testing empty data...'
    call fig%initialize(400, 300)
    call fig%hist(empty_data)
    call fig%set_title('Empty Data Test')
    call fig%savefig('edge_empty.png')
    write(*,*) '  ✓ Empty data handled gracefully'
    
    ! Test 2: Single value - should create meaningful histogram
    write(*,*) '  Testing single value...'
    call fig%initialize(400, 300)
    call fig%hist(single_value)
    call fig%set_title('Single Value Test')
    call fig%savefig('edge_single.png')
    write(*,*) '  ✓ Single value handled gracefully'
    
    ! Test 3: All identical values - should create meaningful histogram
    write(*,*) '  Testing identical values...'
    call fig%initialize(400, 300)
    call fig%hist(identical_values)
    call fig%set_title('Identical Values Test')
    call fig%savefig('edge_identical.png')
    write(*,*) '  ✓ Identical values handled gracefully'
    
    ! Test 4: Extreme value ranges - should handle numerical precision
    write(*,*) '  Testing extreme values...'
    call fig%initialize(400, 300)
    call fig%hist(extreme_values)
    call fig%set_title('Extreme Values Test')
    call fig%savefig('edge_extreme.png')
    write(*,*) '  ✓ Extreme values handled gracefully'
    
    ! Test 5: Invalid bins parameter - should handle gracefully
    write(*,*) '  Testing invalid bins...'
    call fig%initialize(400, 300)
    call fig%hist(identical_values, bins=0)  ! Should handle this gracefully
    call fig%set_title('Invalid Bins Test')
    call fig%savefig('edge_invalid_bins.png')
    write(*,*) '  ✓ Invalid bins handled gracefully'
    
    write(*,*) 'All edge case tests completed successfully!'
    
end program test_histogram_user_edge_cases