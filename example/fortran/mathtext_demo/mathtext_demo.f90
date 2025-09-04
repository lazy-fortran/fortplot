program mathtext_demo
    !! Demonstration of mathematical text rendering with superscripts and subscripts
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(100), y1(100), y2(100)
    integer :: i
    
    print *, "=== Mathematical Text Demo ==="
    
    ! Generate data
    do i = 1, 100
        x(i) = real(i-1, wp) * 0.1_wp
        y1(i) = x(i)**2
        y2(i) = exp(-x(i)/3.0_wp) * 10.0_wp
    end do
    
    ! Create plot with mathematical notation
    call figure()
    call plot(x, y1, label='f(x) = x^2')
    call plot(x, y2, label='g(x) = 10e^{-x/3}')
    
    ! Add labels with mathematical notation
    call xlabel('x_i')
    call ylabel('y = f(x_i)')
    call title('Mathematical Functions: x^2 and e^{-x/3}')
    call legend()
    call grid(.true.)
    
    ! Save in both PNG and PDF formats to compare backends
    call savefig('output/example/fortran/mathtext_demo/mathtext_demo.png')
    print *, "Created mathtext_demo.png"
    
    call savefig('output/example/fortran/mathtext_demo/mathtext_demo.pdf')
    print *, "Created mathtext_demo.pdf"
    
    print *, "SUCCESS: Mathematical text demo completed"
    
end program mathtext_demo