program test_matplotlib_equivalence
    !! Test that our streamplot implementation produces equivalent results to matplotlib
    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    integer, parameter :: nx = 20, ny = 20
    real(real64), dimension(nx) :: x
    real(real64), dimension(ny) :: y  
    real(real64), dimension(nx, ny) :: u, v
    integer :: i, j
    type(figure_t) :: fig
    
    ! Create identical grid to matplotlib test
    do i = 1, nx
        x(i) = -2.0_real64 + 4.0_real64 * real(i-1, real64) / real(nx-1, real64)
    end do
    
    do j = 1, ny
        y(j) = -2.0_real64 + 4.0_real64 * real(j-1, real64) / real(ny-1, real64)
    end do
    
    ! Create identical circular flow field: u = -y, v = x
    do j = 1, ny
        do i = 1, nx
            u(i,j) = -y(j)
            v(i,j) = x(i)
        end do
    end do
    
    call test_same_parameters()
    call test_same_behavior()
    
    print *, "Matplotlib equivalence tests completed!"
    
contains

    subroutine test_same_parameters()
        !! Test with identical parameters to matplotlib defaults
        
        call fig%initialize(800, 600)
        ! Use exact matplotlib defaults: density=1.0, broken_streamlines=True, maxlength=4.0
        call fig%streamplot(x, y, u, v, density=1.0_real64)
        call fig%set_title('Fortplotlib - Should match matplotlib behavior')
        call figure_savefig(fig, get_test_output_path('output/test/test_matplotlib_equivalence/test_matplotlib_equivalent.png'))
        call fig%set_title('Fortplot - Should match matplotlib behavior')
        call figure_savefig(fig, get_test_output_path('/tmp/test/test_matplotlib_equivalent.png'))
        
        print *, "Same parameters test completed - compare with matplotlib reference"
        
    end subroutine test_same_parameters
    
    subroutine test_same_behavior()
        !! Test that behavior matches for different scenarios
        
        ! Test 1: Higher density
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, density=1.5_real64)
        call fig%set_title('Higher density test')
        call figure_savefig(fig, get_test_output_path('output/test/test_matplotlib_equivalence/test_matplotlib_higher_density.png'))
        call figure_savefig(fig, get_test_output_path('/tmp/test/test_matplotlib_higher_density.png'))
        
        ! Test 2: Lower density  
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, density=0.7_real64)
        call fig%set_title('Lower density test')
        call figure_savefig(fig, get_test_output_path('output/test/test_matplotlib_equivalence/test_matplotlib_lower_density.png'))
        call figure_savefig(fig, get_test_output_path('/tmp/test/test_matplotlib_lower_density.png'))
        
        print *, "Behavior tests completed - verify quality matches matplotlib"
        
    end subroutine test_same_behavior
    
end program test_matplotlib_equivalence