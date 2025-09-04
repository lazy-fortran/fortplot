program test_pdf_chars
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(2) = [0.0_wp, 1.0_wp]
    real(wp) :: y(2) = [0.0_wp, 1.0_wp]
    
    call figure()
    
    ! Test specific characters
    call title('Test: mc² Δ ν × ±')
    call xlabel('Direct Unicode: ² ³ ¹')
    call ylabel('Mathtext: x^2 y^3 z^1')
    
    ! Test in legend
    call plot(x, y, label='Unicode ²')
    call plot(x, y*2, label='Mathtext ^2')
    call plot(x, y*3, label='Mixed ² and ^3')
    
    call legend()
    call savefig('test_pdf_chars.pdf')
    
    print *, 'Created test_pdf_chars.pdf'
    
end program test_pdf_chars