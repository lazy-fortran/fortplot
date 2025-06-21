program test_public_api
    !! Test program for the public fortplot API
    use fortplot
    implicit none

    real(wp), dimension(100) :: x, y
    integer :: i

    ! Generate test data
    x = [(real(i, wp), i=0, size(x) - 1)]/10.0_wp
    y = sin(x)

    ! Test simple plot function
    call plot(x, y, 'simple_plot.png', label='sin(x)', title_text='Simple Plot Test')
    
    print *, "Public API test completed - simple_plot.png created"

end program test_public_api