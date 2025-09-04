program test_super_both
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(2) = [0.0_wp, 1.0_wp]
    real(wp) :: y(2) = [0.0_wp, 1.0_wp]
    
    call figure()
    
    ! Test title with superscript
    call title('Title with e^{2}')
    
    ! Test legend with superscript  
    call plot(x, y, label='Legend e^{2}')
    
    call legend()
    call savefig('test_super_both.pdf')
    
    print *, 'Created test_super_both.pdf'
    
end program test_super_both