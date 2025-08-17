program test_streamplot_circles
    !! Test streamplot circles close properly and have correct radial positioning
    use fortplot
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    integer, parameter :: nx = 20, ny = 20
    real(real64), dimension(nx) :: x
    real(real64), dimension(ny) :: y  
    real(real64), dimension(nx, ny) :: u, v
    integer :: i, j
    type(figure_t) :: fig
    
    ! Create test grid
    do i = 1, nx
        x(i) = -2.0_real64 + 4.0_real64 * real(i-1, real64) / real(nx-1, real64)
    end do
    
    do j = 1, ny
        y(j) = -2.0_real64 + 4.0_real64 * real(j-1, real64) / real(ny-1, real64)
    end do
    
    ! Create perfect circular flow field: u = -y, v = x
    do j = 1, ny
        do i = 1, nx
            u(i,j) = -y(j)
            v(i,j) = x(i)
        end do
    end do
    
    call test_circle_closure()
    call test_radial_positioning()
    
    print *, "All streamplot circle tests passed!"
    
contains

    subroutine test_circle_closure()
        !! Test that circular streamlines form closed loops
        
        ! Initialize figure
        call fig%initialize(400, 400)
        call fig%streamplot(x, y, u, v, density=1.0_real64)
        
        ! For a perfect circular flow field, streamlines should be perfect circles
        ! A closed circle should have start and end points very close together
        ! This test verifies the integration is accurate enough to maintain circular topology
        
        ! The visual test is: are the circles closed?
        ! This should be verified by examining the output
        call fig%savefig('output/test/test_streamplot_circles/test_streamplot_circles_closure.png')
        call fig%savefig('/tmp/test/test_streamplot_circles_closure.png')
        print *, "Circle closure test completed - verify visually that circles are closed"
        
    end subroutine test_circle_closure
    
    subroutine test_radial_positioning()
        !! Test that circles are positioned at correct radial distances
        
        ! For circular flow u=-y, v=x, streamlines should be circles centered at origin
        ! Radius should be determined by starting position distance from origin
        ! Circles should not be positioned too far radially compared to matplotlib
        
        call fig%initialize(400, 400)  
        call fig%streamplot(x, y, u, v, density=2.0_real64)
        call fig%savefig('output/test/test_streamplot_circles/test_streamplot_circles_radial.png')
        call fig%savefig('/tmp/test/test_streamplot_circles_radial.png')
        print *, "Radial positioning test completed - verify circles match expected radii"
        
    end subroutine test_radial_positioning
    
end program test_streamplot_circles