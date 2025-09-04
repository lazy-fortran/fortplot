program test_superscript_debug
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(2) = [0.0_wp, 1.0_wp]
    real(wp) :: y1(2) = [0.0_wp, 1.0_wp]
    real(wp) :: y2(2) = [0.2_wp, 1.2_wp]
    real(wp) :: y3(2) = [0.4_wp, 1.4_wp]
    
    call figure()
    
    ! Test multiple superscripts
    call title('Title: x^2 + y^3 = z^{10}')
    call xlabel('x-axis with subscript x_i')
    call ylabel('y-axis with mix y_0^2')
    
    ! Test legend with various notations
    call plot(x, y1, label='Simple: e^2')
    call plot(x, y2, label='Complex: e^{-\mu\tau}')
    call plot(x, y3, label='Mix: \alpha_0^{2\pi}')
    
    call legend()
    call savefig('test_superscript_debug.pdf')
    
    print *, 'Created test_superscript_debug.pdf'
    print *, 'Check if superscripts appear above baseline'
    print *, 'Check if subscripts appear below baseline'
    
end program test_superscript_debug