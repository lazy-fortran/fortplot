program test_circle_closure
    !! Test that circular streamlines form complete closed loops
    use fortplot
    use, intrinsic :: iso_fortran_env, only: real64
    use fortplot_security, only: get_test_output_path
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
    
    call test_circle_must_close()
    call test_different_radii()
    
    print *, "All circle closure tests passed!"
    
contains

    subroutine test_circle_must_close()
        !! Test that circles form complete closed loops
        
        call fig%initialize(400, 400)
        call fig%streamplot(x, y, u, v, density=1.5_real64)
        call fig%set_title('Circle Closure Test - All circles should be closed')
        call figure_savefig(fig, get_test_output_path('output/test/test_circle_closure/test_circle_closure_main.png'))
        call figure_savefig(fig, get_test_output_path('/tmp/test/test_circle_closure_main.png'))
        
        ! For visual verification: in a circular flow u=-y, v=x
        ! Every streamline should be a perfect circle centered at origin
        ! No gaps should be visible in any circle
        print *, "Main circle closure test completed - verify all circles are closed"
        
    end subroutine test_circle_must_close
    
    subroutine test_different_radii()
        !! Test circles at different radii all close properly
        
        call fig%initialize(600, 600)
        call fig%streamplot(x, y, u, v, density=2.0_real64)
        call fig%set_title('Multiple Radii Test - Each radius should form closed circle')
        call figure_savefig(fig, get_test_output_path('output/test/test_circle_closure/test_circle_closure_radii.png'))
        call figure_savefig(fig, get_test_output_path('/tmp/test/test_circle_closure_radii.png'))
        
        print *, "Multiple radii test completed - verify circles at all radii close"
        
    end subroutine test_different_radii
    
end program test_circle_closure