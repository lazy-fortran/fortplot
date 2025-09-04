program test_pdf_mathtext
    !! Test mathematical text rendering in PDF backend
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(100), y1(100), y2(100), y3(100)
    integer :: i
    
    print *, "Testing PDF mathematical text rendering with superscripts and subscripts..."
    
    ! Generate test data
    do i = 1, 100
        x(i) = real(i-1, wp) * 0.1_wp
        y1(i) = x(i)**2
        y2(i) = exp(-x(i)/2.0_wp)
        y3(i) = sqrt(x(i))
    end do
    
    ! Test 1: Basic superscript in title and labels
    call figure()
    call plot(x, y1, label='y = x^2')
    call xlabel('x')
    call ylabel('y = x^2')
    call title('Quadratic Function: y = x^2')
    call savefig('test_pdf_mathtext_superscript.pdf')
    print *, "Created test_pdf_mathtext_superscript.pdf"
    
    ! Test 2: Subscripts in labels
    call figure()
    call plot(x, y2, label='y = e^{-x/2}')
    call xlabel('x_i')
    call ylabel('y_i')
    call title('Exponential Decay: A_0 e^{-t/\tau}')
    call savefig('test_pdf_mathtext_subscript.pdf')
    print *, "Created test_pdf_mathtext_subscript.pdf"
    
    ! Test 3: Combined superscripts and subscripts
    call figure()
    call plot(x, y1, label='x^2')
    call plot(x, y2*10.0_wp, label='10e^{-x/2}')
    call plot(x, y3*3.0_wp, label='3x^{0.5}')
    call xlabel('Position x_i (meters)')
    call ylabel('Amplitude A_n^2 (m^2)')
    call title('Mixed Math: E = mc^2, \chi^2_{\nu}, R_{ij}^{kl}')
    call legend()
    call savefig('test_pdf_mathtext_combined.pdf')
    print *, "Created test_pdf_mathtext_combined.pdf"
    
    ! Test 4: Complex mathematical expressions
    call figure()
    call plot(x, y1/10.0_wp)
    call xlabel('Temperature T_c (K)')
    call ylabel('Pressure P_{atm} (bar)')
    call title('Phase Transition: T_c^{critical} = 647.096 K for H_2O')
    call savefig('test_pdf_mathtext_complex.pdf')
    print *, "Created test_pdf_mathtext_complex.pdf"
    
    print *, "SUCCESS: PDF mathematical text rendering tests completed"
    
end program test_pdf_mathtext