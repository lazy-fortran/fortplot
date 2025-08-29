program test_user_workflow
    use fortplot  ! Should provide wp => real64 for precision
    implicit none
    real(wp), dimension(50) :: x, y
    integer :: i

    ! Test basic README example - Generate sample data
    x = [(real(i-1, wp) * 0.2_wp, i=1, 50)]
    y = sin(x)

    ! Test stateful API
    call figure()
    call plot(x, y)
    call title("Function Plot")
    call xlabel("x")
    call ylabel("y") 
    call xlim(0.0_wp, 10.0_wp)
    call ylim(-1.0_wp, 1.0_wp)
    call savefig("test_plot.png")

    print *, "Test completed successfully"
end program test_user_workflow