program test_basic_user_workflow
    ! Test basic user workflow from README
    use fortplot
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    real(wp), dimension(50) :: x, y
    character(len=:), allocatable :: output_dir
    integer :: i

    call ensure_test_output_dir('basic_user_workflow', output_dir)

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
    call savefig(trim(output_dir)//'basic_user_test.png')

    print *, "Basic user workflow test completed"

end program test_basic_user_workflow
