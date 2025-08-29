program test_public_api
    !! Test program for the public fortplot API
    use fortplot
    use fortplot_security, only: get_test_output_path
    implicit none

    real(wp), dimension(100) :: x, y
    integer :: i

    ! Generate test data
    x = [(real(i, wp), i=0, size(x) - 1)]/10.0_wp
    y = sin(x)

    ! Test pyplot-style API
    call figure()
    call plot(x, y, label='sin(x)')
    call title('Simple Plot Test')
    call xlabel('x')
    call ylabel('y')
    call savefig('test/output/simple_plot.png')
    
    print *, "Public API test completed - simple_plot.png created"

end program test_public_api