program pcolormesh_demo
    !! Comprehensive demo of pcolormesh functionality
    !! Shows different colormap and data patterns
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t, pcolormesh, savefig, figure, xlabel, ylabel, title
    implicit none
    
    call demo_basic_gradient()
    call demo_sinusoidal_pattern()
    call demo_different_colormaps()
    
contains

    subroutine demo_basic_gradient()
        !! Basic pcolormesh with simple gradient
        real(wp) :: x(6), y(5)
        real(wp) :: c(4, 5)
        integer :: i, j
        
        ! Create coordinate arrays for regular grid
        do i = 1, 6
            x(i) = real(i-1, wp) * 0.4_wp
        end do
        do i = 1, 5
            y(i) = real(i-1, wp) * 0.3_wp
        end do
        
        ! Create test data - simple gradient
        do i = 1, 4
            do j = 1, 5
                c(i, j) = real(i, wp) + real(j, wp) * 0.5_wp
            end do
        end do
        
        ! Create pcolormesh plot
        call figure(640, 480)
        call title('Basic Pcolormesh - Linear Gradient')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='viridis')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_basic.png')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_basic.pdf')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_basic.txt')
        
        print *, "Basic pcolormesh saved to example/fortran/pcolormesh_demo/pcolormesh_basic.{png,pdf,txt}"
    end subroutine demo_basic_gradient

    subroutine demo_sinusoidal_pattern()
        !! Pcolormesh with sinusoidal pattern
        real(wp) :: x(9), y(9)
        real(wp) :: c(8, 8)
        real(wp) :: xi, yj
        integer :: i, j
        real(wp), parameter :: pi = 3.14159265359_wp
        
        ! Create coordinate arrays
        do i = 1, 9
            x(i) = real(i-1, wp) * 0.2_wp
        end do
        do i = 1, 9
            y(i) = real(i-1, wp) * 0.15_wp
        end do
        
        ! Create sinusoidal pattern  
        do i = 1, 8
            do j = 1, 8
                xi = (x(i) + x(i+1)) * 0.5_wp  ! Center of quad
                yj = (y(j) + y(j+1)) * 0.5_wp  ! Center of quad 
                c(i, j) = sin(2.0_wp * pi * xi) * cos(3.0_wp * pi * yj)
            end do
        end do
        
        call figure(640, 480)
        call title('Pcolormesh - Sinusoidal Pattern')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='coolwarm')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.png')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.pdf')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.txt')
        
        print *, "Sinusoidal pcolormesh saved to example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.{png,pdf,txt}"
    end subroutine demo_sinusoidal_pattern

    subroutine demo_different_colormaps()
        !! Demo different colormaps
        real(wp) :: x(6), y(6)
        real(wp) :: c(5, 5)
        real(wp) :: r
        integer :: i, j
        real(wp), parameter :: pi = 3.14159265359_wp
        
        ! Create coordinate arrays
        do i = 1, 6
            x(i) = real(i-1, wp) * 0.3_wp
        end do
        do i = 1, 6
            y(i) = real(i-1, wp) * 0.25_wp
        end do
        
        ! Create radial pattern
        do i = 1, 5
            do j = 1, 5
                r = sqrt((x(i) - 1.0_wp)**2 + (y(j) - 0.6_wp)**2)
                c(i, j) = exp(-r)
            end do
        end do
        
        call figure(640, 480)
        call title('Pcolormesh - Radial Pattern (Plasma)')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='plasma')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_plasma.png')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_plasma.pdf')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_plasma.txt')
        
        print *, "Plasma colormap pcolormesh saved to example/fortran/pcolormesh_demo/pcolormesh_plasma.{png,pdf,txt}"
    end subroutine demo_different_colormaps
    
end program pcolormesh_demo