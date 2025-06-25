program test_ylabel_positioning
    !! Test Y-axis label positioning with the new rotation approach
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: my_fig
    real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    real(wp) :: y(5) = [1.0_wp, 4.0_wp, 2.0_wp, 5.0_wp, 3.0_wp]
    
    ! Create a simple plot with Y-axis label to test positioning
    call my_fig%initialize(400, 300)
    call my_fig%add_plot(x, y, label="test")
    call my_fig%set_xlabel("X values")
    call my_fig%set_ylabel("Y values")
    call my_fig%set_title("Y-label Position Test")
    
    ! Save to check Y-axis label positioning
    call my_fig%savefig('test_ylabel_position.png')
    
    print *, "Created test_ylabel_position.png to check Y-axis label positioning"
    
end program test_ylabel_positioning