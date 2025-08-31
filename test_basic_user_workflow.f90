program test_basic_user_workflow
    ! Test basic user workflow from README
    use fortplot
    implicit none
    
    real(wp), dimension(50) :: x, y
    integer :: i
    
    ! Basic stateful API from README
    x = [(real(i-1, wp) * 0.2_wp, i=1, 50)]
    y = sin(x)
    
    call figure()
    call plot(x, y)
    call title("Function Plot")
    call xlabel("x")  
    call ylabel("y")
    call xlim(0.0_wp, 10.0_wp)
    call ylim(-1.0_wp, 1.0_wp)
    call savefig("basic_user_test.png")
    
    print *, "Basic user workflow test completed"
    
end program test_basic_user_workflow