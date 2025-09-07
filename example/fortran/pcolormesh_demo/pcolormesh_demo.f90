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
        !! Basic pcolormesh with simple gradient - enhanced 51x51 grid
        real(wp) :: x(51), y(51)
        real(wp) :: c(50, 50)
        integer :: i, j
        real(wp) :: dx, dy
        
        ! Create coordinate arrays for high-resolution regular grid
        dx = 2.0_wp / 50.0_wp  ! Total range 2.0, 50 intervals
        dy = 1.2_wp / 50.0_wp  ! Total range 1.2, 50 intervals
        
        do i = 1, 51
            x(i) = real(i-1, wp) * dx
        end do
        do i = 1, 51
            y(i) = real(i-1, wp) * dy
        end do
        
        ! Create test data - smooth gradient across high-res grid
        do i = 1, 50
            do j = 1, 50
                c(j, i) = real(i, wp) / 50.0_wp + real(j, wp) / 50.0_wp * 0.5_wp
            end do
        end do
        
        ! Create pcolormesh plot
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Basic Pcolormesh - Linear Gradient (50x50 resolution)')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='viridis')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_basic.png')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_basic.pdf')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_basic.txt')
        
    end subroutine demo_basic_gradient

    subroutine demo_sinusoidal_pattern()
        !! Pcolormesh with sinusoidal pattern - enhanced 51x51 grid
        real(wp) :: x(51), y(51)
        real(wp) :: c(50, 50)
        real(wp) :: xi, yj
        integer :: i, j
        real(wp), parameter :: pi = 3.14159265359_wp
        real(wp) :: dx, dy
        
        ! Create coordinate arrays for high-resolution grid
        dx = 1.6_wp / 50.0_wp  ! Total range 1.6, 50 intervals
        dy = 1.2_wp / 50.0_wp  ! Total range 1.2, 50 intervals
        
        do i = 1, 51
            x(i) = real(i-1, wp) * dx
        end do
        do i = 1, 51
            y(i) = real(i-1, wp) * dy
        end do
        
        ! Create sinusoidal pattern with proper sampling
        do i = 1, 50
            do j = 1, 50
                xi = (x(i) + x(i+1)) * 0.5_wp  ! Center of cell
                yj = (y(j) + y(j+1)) * 0.5_wp  ! Center of cell
                c(j, i) = sin(2.0_wp * pi * xi) * cos(3.0_wp * pi * yj)
            end do
        end do
        
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Pcolormesh - Sinusoidal Pattern (50x50 resolution)')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='coolwarm')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.png')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.pdf')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.txt')
        
    end subroutine demo_sinusoidal_pattern

    subroutine demo_different_colormaps()
        !! Demo different colormaps - enhanced 51x51 grid
        real(wp) :: x(51), y(51)
        real(wp) :: c(50, 50)
        real(wp) :: r, xi, yj
        integer :: i, j
        real(wp), parameter :: pi = 3.14159265359_wp
        real(wp) :: dx, dy
        
        ! Create coordinate arrays for high-resolution grid
        dx = 1.5_wp / 50.0_wp  ! Total range 1.5, 50 intervals
        dy = 1.25_wp / 50.0_wp ! Total range 1.25, 50 intervals
        
        do i = 1, 51
            x(i) = real(i-1, wp) * dx
        end do
        do i = 1, 51
            y(i) = real(i-1, wp) * dy
        end do
        
        ! Create radial pattern with proper sampling
        do i = 1, 50
            do j = 1, 50
                xi = (x(i) + x(i+1)) * 0.5_wp  ! Center of cell
                yj = (y(j) + y(j+1)) * 0.5_wp  ! Center of cell
                r = sqrt((xi - 0.75_wp)**2 + (yj - 0.625_wp)**2)
                c(j, i) = exp(-r)
            end do
        end do
        
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Pcolormesh - Radial Pattern (Plasma) (50x50 resolution)')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='plasma')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_plasma.png')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_plasma.pdf')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_plasma.txt')
        
    end subroutine demo_different_colormaps
    
end program pcolormesh_demo
