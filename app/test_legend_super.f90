program test_legend_super
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(2) = [0.0_wp, 1.0_wp]
    real(wp) :: y(2) = [0.0_wp, 1.0_wp]
    
    call figure()
    
    ! Simple test with just superscript
    call plot(x, y, label='Test e^{2}')
    
    call legend()
    call savefig('test_legend_super.pdf')
    
    print *, 'Created test_legend_super.pdf'
    
end program test_legend_super