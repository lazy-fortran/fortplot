program basic_user_program
    !> Basic user program demonstrating fortplot compilation and usage
    !! This program serves as a template for users to verify their
    !! fortplot setup and learn basic plotting functionality.
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    integer, parameter :: n = 100
    real(wp), dimension(n) :: x, y_sin, y_cos
    integer :: i
    
    print *, "=== Basic User Program Demo ==="
    print *, "Testing fortplot compilation and basic functionality..."
    
    ! Generate sample data
    do i = 1, n
        x(i) = real(i-1, wp) * 0.1_wp  ! 0.0 to 9.9
        y_sin(i) = sin(x(i))
        y_cos(i) = cos(x(i))
    end do
    
    ! Create a basic plot
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot(x, y_sin, label="sin(x)", linestyle="b-")
    call plot(x, y_cos, label="cos(x)", linestyle="r--")
    call title("Basic User Program - Trigonometric Functions")
    call xlabel("x")
    call ylabel("y")
    call xlim(0.0_wp, 10.0_wp)
    call ylim(-1.2_wp, 1.2_wp)
    call legend()
    
    ! Save in multiple formats
    call savefig("basic_user_plot.png")
    call savefig("basic_user_plot.pdf")
    call savefig("basic_user_plot.txt")
    
    print *, "SUCCESS: Plot saved as basic_user_plot.png, .pdf, and .txt"
    print *, "Your fortplot compilation setup is working correctly!"
    
end program basic_user_program