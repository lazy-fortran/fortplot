program test_histogram_docs
    !! Test examples exactly as documented in the API
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    real(wp) :: data_values(100)
    type(figure_t) :: fig
    integer :: i
    
    write(*,*) '=== TESTING DOCUMENTED EXAMPLES ==='
    
    ! Generate test data
    do i = 1, 100
        data_values(i) = real(i, wp) * 0.1_wp + sin(real(i, wp) * 0.1_wp) * 2.0_wp
    end do
    
    ! Test the exact example from the hist() documentation (needs figure initialization)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call hist(data_values, bins=20, label='Distribution')
    call savefig('build/doc_example_hist.png')
    write(*,*) '  ✓ hist() documented example works (with figure init)'
    
    ! Test the exact example from the histogram() documentation (needs figure initialization)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call histogram(data_values, bins=20, label='Distribution')
    call savefig('build/doc_example_histogram.png')
    write(*,*) '  ✓ histogram() documented example works (with figure init)'
    
    ! Test object-oriented API as documented
    call fig%initialize(800, 600)
    call fig%hist(data_values, bins=20, label='Distribution')
    call fig%savefig('build/doc_example_oo.png')
    write(*,*) '  ✓ Object-oriented API works'
    
    write(*,*) '=== DOCUMENTATION EXAMPLES VALIDATED ==='
    
end program test_histogram_docs