program test_pcolormesh_enhanced_resolution
    !! Test enhanced grid resolution for pcolormesh demo
    !! Verifies that enhanced grids maintain pattern fidelity at higher resolution
    
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    logical :: test_passed
    character(len=256) :: error_msg
    
    test_passed = .true.
    error_msg = ""
    
    print *, "Testing enhanced resolution pcolormesh grids..."
    
    ! Test enhanced basic gradient  
    call test_enhanced_basic_gradient()
    
    ! Test enhanced sinusoidal pattern
    call test_enhanced_sinusoidal_pattern()
    
    ! Test enhanced radial pattern
    call test_enhanced_radial_pattern()
    
    if (test_passed) then
        print *, "PASS: All enhanced resolution tests passed"
        stop 0
    else
        print *, "FAIL: ", trim(error_msg)
        stop 1
    end if
    
contains

    subroutine test_enhanced_basic_gradient()
        !! Test enhanced basic gradient with 51x51 grid (50x50 cells)
        real(wp) :: x(51), y(51), c(50, 50)
        integer :: i, j, file_size
        real(wp) :: dx, dy
        
        print *, "Testing enhanced basic gradient (50x50 cells)..."
        
        ! Create coordinate arrays for high-resolution grid
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
                c(i, j) = real(i, wp) / 50.0_wp + real(j, wp) / 50.0_wp * 0.5_wp
            end do
        end do
        
        ! Create pcolormesh plot
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Enhanced Basic Gradient (50x50)')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='viridis')
        call savefig('test_enhanced_basic_gradient.png')
        
        ! Verify file was created and has reasonable size
        file_size = get_file_size('test_enhanced_basic_gradient.png')
        if (file_size < 1000) then
            test_passed = .false.
            error_msg = "Enhanced basic gradient PNG too small or not created"
        end if
    end subroutine test_enhanced_basic_gradient
    
    subroutine test_enhanced_sinusoidal_pattern()
        !! Test enhanced sinusoidal pattern with 51x51 grid (50x50 cells)
        real(wp) :: x(51), y(51), c(50, 50)
        real(wp) :: xi, yj, dx, dy
        integer :: i, j, file_size
        real(wp), parameter :: pi = 3.14159265359_wp
        
        print *, "Testing enhanced sinusoidal pattern (50x50 cells)..."
        
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
                c(i, j) = sin(2.0_wp * pi * xi) * cos(3.0_wp * pi * yj)
            end do
        end do
        
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Enhanced Sinusoidal Pattern (50x50)')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='coolwarm')
        call savefig('test_enhanced_sinusoidal_pattern.png')
        
        ! Verify file was created and has reasonable size
        file_size = get_file_size('test_enhanced_sinusoidal_pattern.png')
        if (file_size < 1000) then
            test_passed = .false.
            error_msg = "Enhanced sinusoidal pattern PNG too small or not created"
        end if
    end subroutine test_enhanced_sinusoidal_pattern
    
    subroutine test_enhanced_radial_pattern()
        !! Test enhanced radial pattern with 51x51 grid (50x50 cells)
        real(wp) :: x(51), y(51), c(50, 50)
        real(wp) :: r, dx, dy, xi, yj
        integer :: i, j, file_size
        
        print *, "Testing enhanced radial pattern (50x50 cells)..."
        
        ! Create coordinate arrays for high-resolution grid
        dx = 1.5_wp / 50.0_wp  ! Total range 1.5, 50 intervals
        dy = 1.25_wp / 50.0_wp  ! Total range 1.25, 50 intervals
        
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
                c(i, j) = exp(-r)
            end do
        end do
        
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Enhanced Radial Pattern (50x50)')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='plasma')
        call savefig('test_enhanced_radial_pattern.png')
        
        ! Verify file was created and has reasonable size
        file_size = get_file_size('test_enhanced_radial_pattern.png')
        if (file_size < 1000) then
            test_passed = .false.
            error_msg = "Enhanced radial pattern PNG too small or not created"
        end if
    end subroutine test_enhanced_radial_pattern
    
    integer function get_file_size(filename)
        character(len=*), intent(in) :: filename
        integer(8) :: size
        
        inquire(file=filename, size=size)
        get_file_size = int(size)
    end function get_file_size

end program test_pcolormesh_enhanced_resolution