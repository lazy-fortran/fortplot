program test_text_rendering
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(2) = [0.0_wp, 1.0_wp]
    real(wp) :: y1(2) = [0.0_wp, 1.0_wp]
    real(wp) :: y2(2) = [0.2_wp, 1.2_wp]
    real(wp) :: y3(2) = [0.4_wp, 1.4_wp]
    
    call figure()
    
    ! Test Unicode in title
    call title('\alpha \beta \gamma with x^2')
    call xlabel('Plain text with subscript x_i')
    call ylabel('Mixed \mu_0^2')
    
    ! Test Unicode and superscripts in legend
    call plot(x, y1, label='\alpha plain')
    call plot(x, y2, label='\beta with e^2')  
    call plot(x, y3, label='Mixed \gamma_0^{2\pi}')
    
    call legend()
    call savefig('test_text_rendering.pdf')
    
    print *, 'Created test_text_rendering.pdf'
    print *, 'Check:'
    print *, '1. Title should show Greek letters AND superscript'
    print *, '2. Legend should show Greek letters AND superscripts'
    
end program test_text_rendering