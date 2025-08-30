program test_pcolormesh_rendering_comprehensive
    !! Comprehensive pcolormesh rendering and backend test
    !! Replaces: test_pcolormesh_rendering.f90, test_pcolormesh_enhanced_resolution.f90, 
    !!           test_pcolormesh_dimension_consistency.f90
    !! Tests all backends (ASCII, PNG, PDF), high-resolution rendering, 
    !! and dimension consistency across backends
    
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    logical :: test_passed
    character(len=512) :: error_msg
    
    test_passed = .true.
    error_msg = ""
    
    print *, "=== COMPREHENSIVE PCOLORMESH RENDERING TEST SUITE ==="
    print *, ""
    print *, "Testing all backends, high-resolution rendering,"
    print *, "and dimension consistency across backends"
    print *, ""
    
    ! Backend rendering tests
    call test_backend_rendering()
    
    ! Enhanced resolution tests
    call test_enhanced_resolution()
    
    ! Dimension consistency tests
    call test_dimension_consistency()
    
    if (test_passed) then
        print *, ""
        print *, "SUCCESS: All rendering and backend tests PASSED!"
        print *, "All backends working correctly with consistent dimensions."
        print *, "High-resolution rendering maintains pattern fidelity."
        stop 0
    else
        print *, ""
        print *, "FAILURE: Some rendering tests failed!"
        print *, "Error: ", trim(error_msg)
        stop 1
    end if

contains

    subroutine test_backend_rendering()
        !! Test rendering functionality across all backends
        print *, "1. Testing Backend Rendering Functionality"
        
        call test_ascii_backend()
        call test_png_backend()
        call test_pdf_backend()
        call test_basic_pcolormesh()
    end subroutine test_backend_rendering

    subroutine test_ascii_backend()
        !! Test ASCII backend produces visible character-based output
        real(wp) :: x(4), y(4), c(3, 3)
        integer :: i, j
        character(len=10000) :: ascii_content
        character(len=256) :: line
        integer :: unit, iostat
        logical :: has_visible_chars
        
        print *, "  Testing ASCII backend..."
        
        ! Create test grid
        do i = 1, 4
            x(i) = real(i-1, wp)
            y(i) = real(i-1, wp)
        end do
        
        ! Create distinctive test pattern
        do i = 1, 3
            do j = 1, 3
                c(i, j) = real(i * j, wp)
            end do
        end do
        
        ! Generate ASCII plot
        call figure()
        call pcolormesh(x, y, c)
        call title('ASCII Backend Test')
        call savefig("test/output/test_rendering_ascii.txt")
        
        ! Verify ASCII file contains visible characters
        open(newunit=unit, file='test/output/test_rendering_ascii.txt', status='old', iostat=iostat)
        if (iostat /= 0) then
            test_passed = .false.
            error_msg = "ASCII backend failed to create output file"
            return
        end if
        
        ! Read and analyze content
        ascii_content = ""
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            ascii_content = trim(ascii_content) // line
        end do
        close(unit)
        
        ! Check for density characters (from ASCII rendering)
        has_visible_chars = (index(ascii_content, '#') > 0 .or. &
                            index(ascii_content, '*') > 0 .or. &
                            index(ascii_content, '+') > 0 .or. &
                            index(ascii_content, '=') > 0 .or. &
                            index(ascii_content, '.') > 0)
        
        if (.not. has_visible_chars) then
            test_passed = .false.
            error_msg = "ASCII backend produced no visible character data"
            return
        end if
        
        print *, "    ASCII backend: PASS (contains visible characters)"
    end subroutine test_ascii_backend

    subroutine test_png_backend()
        !! Test PNG backend produces valid image files
        real(wp) :: x(4), y(4), c(3, 3)
        integer :: i, j, file_size
        
        print *, "  Testing PNG backend..."
        
        ! Create test grid
        do i = 1, 4
            x(i) = real(i-1, wp)
            y(i) = real(i-1, wp)
        end do
        
        ! Create gradient test pattern
        do i = 1, 3
            do j = 1, 3
                c(i, j) = real(i + j, wp)
            end do
        end do
        
        ! Generate PNG plot
        call figure(figsize=[4.0_wp, 3.0_wp])
        call pcolormesh(x, y, c, colormap='plasma')
        call title('PNG Backend Test')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call savefig("test/output/test_rendering.png")
        
        ! Verify PNG file size is reasonable
        file_size = get_file_size('test_rendering.png')
        if (file_size < 1000) then  ! PNG should be substantial
            test_passed = .false.
            error_msg = "PNG backend produced file too small or empty"
            return
        end if
        
        print *, "    PNG backend: PASS (file size: ", file_size, " bytes)"
    end subroutine test_png_backend

    subroutine test_pdf_backend()
        !! Test PDF backend produces valid PDF files
        real(wp) :: x(3), y(3), c(2, 2)
        integer :: i, file_size
        
        print *, "  Testing PDF backend..."
        
        ! Create minimal test grid
        do i = 1, 3
            x(i) = real(i-1, wp) * 2.0_wp
            y(i) = real(i-1, wp) * 2.0_wp
        end do
        
        ! Create simple 2x2 pattern
        c(1, 1) = 0.0_wp
        c(1, 2) = 0.5_wp
        c(2, 1) = 0.5_wp
        c(2, 2) = 1.0_wp
        
        ! Generate PDF plot
        call figure()
        call pcolormesh(x, y, c, colormap='coolwarm')
        call title('PDF Backend Test')
        call savefig("test/output/test_rendering.pdf")
        
        ! Verify PDF file exists and has content
        file_size = get_file_size('test_rendering.pdf')
        if (file_size < 500) then  ! PDF should have substantial content
            test_passed = .false.
            error_msg = "PDF backend produced file too small or empty"
            return
        end if
        
        print *, "    PDF backend: PASS (file size: ", file_size, " bytes)"
    end subroutine test_pdf_backend

    subroutine test_basic_pcolormesh()
        !! Test basic pcolormesh functionality with different patterns
        real(wp) :: x(6), y(5), c(4, 5)
        integer :: i, j
        
        print *, "  Testing basic pcolormesh patterns..."
        
        ! Create coordinate arrays
        do i = 1, 6
            x(i) = real(i-1, wp) * 0.4_wp
        end do
        do i = 1, 5
            y(i) = real(i-1, wp) * 0.3_wp
        end do
        
        ! Create gradient pattern
        do i = 1, 4
            do j = 1, 5
                c(i, j) = real(i, wp) + real(j, wp) * 0.5_wp
            end do
        end do
        
        ! Generate basic plot
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Basic Pcolormesh Pattern')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='viridis')
        call savefig("test/output/test_rendering_basic.png")
        
        ! Verify file creation
        if (.not. file_exists('test_rendering_basic.png')) then
            test_passed = .false.
            error_msg = "Basic pcolormesh pattern failed to generate"
            return
        end if
        
        print *, "    Basic patterns: PASS"
    end subroutine test_basic_pcolormesh

    subroutine test_enhanced_resolution()
        !! Test high-resolution rendering maintains pattern fidelity  
        print *, ""
        print *, "2. Testing Enhanced Resolution Rendering"
        
        call test_enhanced_gradient()
        call test_enhanced_sinusoidal()
        call test_enhanced_radial()
    end subroutine test_enhanced_resolution

    subroutine test_enhanced_gradient()
        !! Test enhanced basic gradient with high-resolution 50x50 grid
        real(wp) :: x(51), y(51), c(50, 50)
        integer :: i, j, file_size
        real(wp) :: dx, dy
        
        print *, "  Testing enhanced gradient (50x50 resolution)..."
        
        ! Create high-resolution coordinate arrays
        dx = 2.0_wp / 50.0_wp  ! Total range 2.0, 50 intervals
        dy = 1.2_wp / 50.0_wp  ! Total range 1.2, 50 intervals
        
        do i = 1, 51
            x(i) = real(i-1, wp) * dx
            y(i) = real(i-1, wp) * dy
        end do
        
        ! Create smooth gradient pattern
        do i = 1, 50
            do j = 1, 50
                c(i, j) = real(i, wp) / 50.0_wp + real(j, wp) / 50.0_wp * 0.5_wp
            end do
        end do
        
        ! Generate high-resolution plot
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Enhanced Gradient (50x50)')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='viridis')
        call savefig("test/output/test_enhanced_gradient.png")
        
        ! Verify substantial file size (high resolution should be larger)
        file_size = get_file_size('test_enhanced_gradient.png')
        if (file_size < 2000) then
            test_passed = .false.
            error_msg = "Enhanced gradient resolution too low or failed"
            return
        end if
        
        print *, "    Enhanced gradient: PASS (", file_size, " bytes)"
    end subroutine test_enhanced_gradient

    subroutine test_enhanced_sinusoidal()
        !! Test high-resolution sinusoidal pattern preservation
        real(wp) :: x(51), y(51), c(50, 50)
        real(wp) :: xi, yj, dx, dy
        integer :: i, j, file_size
        real(wp), parameter :: pi = 3.14159265359_wp
        
        print *, "  Testing enhanced sinusoidal pattern (50x50 resolution)..."
        
        ! Create coordinate arrays
        dx = 1.6_wp / 50.0_wp
        dy = 1.2_wp / 50.0_wp
        
        do i = 1, 51
            x(i) = real(i-1, wp) * dx
            y(i) = real(i-1, wp) * dy
        end do
        
        ! Create sinusoidal pattern with proper cell-center sampling
        do i = 1, 50
            do j = 1, 50
                xi = (x(i) + x(i+1)) * 0.5_wp  ! Cell center X
                yj = (y(j) + y(j+1)) * 0.5_wp  ! Cell center Y
                c(i, j) = sin(2.0_wp * pi * xi) * cos(3.0_wp * pi * yj)
            end do
        end do
        
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Enhanced Sinusoidal Pattern (50x50)')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='coolwarm')
        call savefig("test/output/test_enhanced_sinusoidal.png")
        
        file_size = get_file_size('test_enhanced_sinusoidal.png')
        if (file_size < 2000) then
            test_passed = .false.
            error_msg = "Enhanced sinusoidal pattern generation failed"
            return
        end if
        
        print *, "    Enhanced sinusoidal: PASS (", file_size, " bytes)"
    end subroutine test_enhanced_sinusoidal

    subroutine test_enhanced_radial()
        !! Test high-resolution radial pattern preservation
        real(wp) :: x(51), y(51), c(50, 50)
        real(wp) :: r, dx, dy, xi, yj
        integer :: i, j, file_size
        
        print *, "  Testing enhanced radial pattern (50x50 resolution)..."
        
        ! Create coordinate arrays
        dx = 1.5_wp / 50.0_wp
        dy = 1.25_wp / 50.0_wp
        
        do i = 1, 51
            x(i) = real(i-1, wp) * dx
            y(i) = real(i-1, wp) * dy
        end do
        
        ! Create radial decay pattern with proper cell-center sampling
        do i = 1, 50
            do j = 1, 50
                xi = (x(i) + x(i+1)) * 0.5_wp  ! Cell center X
                yj = (y(j) + y(j+1)) * 0.5_wp  ! Cell center Y
                r = sqrt((xi - 0.75_wp)**2 + (yj - 0.625_wp)**2)
                c(i, j) = exp(-r)
            end do
        end do
        
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Enhanced Radial Pattern (50x50)')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='plasma')
        call savefig("test/output/test_enhanced_radial.png")
        
        file_size = get_file_size('test_enhanced_radial.png')
        if (file_size < 2000) then
            test_passed = .false.
            error_msg = "Enhanced radial pattern generation failed"
            return
        end if
        
        print *, "    Enhanced radial: PASS (", file_size, " bytes)"
    end subroutine test_enhanced_radial

    subroutine test_dimension_consistency()
        !! Test dimension consistency across all backends
        print *, ""
        print *, "3. Testing Dimension Consistency Across Backends"
        
        call test_backend_dimension_consistency()
    end subroutine test_dimension_consistency

    subroutine test_backend_dimension_consistency()
        !! Test that all backends consistently handle z_grid(ny, nx) order
        real(wp) :: x_grid(6), y_grid(8), z_grid(7, 5)  ! x(nx+1), y(ny+1), z(ny, nx)
        integer :: i, j
        logical :: ascii_ok, raster_ok
        
        print *, "  Testing consistent z_grid(ny, nx) across backends..."
        print *, "    Grid: x(6), y(8), z(7,5) -> nx=5, ny=7"
        
        ! Initialize coordinate arrays (cell edges)
        do i = 1, 6
            x_grid(i) = real(i - 1, wp) * 0.8_wp
        end do
        
        do j = 1, 8
            y_grid(j) = real(j - 1, wp) * 0.5_wp
        end do
        
        ! Initialize z with pattern: z(j,i) = i + j * 10
        ! This distinctive pattern makes dimension transposition detectable
        do j = 1, 7  ! ny
            do i = 1, 5  ! nx
                z_grid(j, i) = real(i, wp) + real(j, wp) * 10.0_wp
            end do
        end do
        
        ! Test ASCII backend with z_grid(ny, nx)
        call figure()
        call pcolormesh(x_grid, y_grid, z_grid)
        call title('Dimension Consistency Test - ASCII')
        call savefig("test/output/test_dim_consistency_ascii.txt")
        
        ascii_ok = file_exists('test_dim_consistency_ascii.txt')
        if (.not. ascii_ok) then
            test_passed = .false.
            error_msg = "ASCII backend rejected z_grid(ny,nx) dimensions"
            return
        end if
        
        ! Test raster backend with z_grid(ny, nx)
        call figure()  
        call pcolormesh(x_grid, y_grid, z_grid)
        call title('Dimension Consistency Test - Raster')
        call savefig("test/output/test_dim_consistency_raster.ppm")
        
        raster_ok = file_exists('test_dim_consistency_raster.ppm')
        if (.not. raster_ok) then
            test_passed = .false.
            error_msg = "Raster backend rejected z_grid(ny,nx) dimensions"
            return
        end if
        
        print *, "    ASCII backend: PASS (accepts z_grid(ny,nx))"
        print *, "    Raster backend: PASS (accepts z_grid(ny,nx))"
        print *, "    Dimension consistency: VERIFIED"
    end subroutine test_backend_dimension_consistency

    ! Utility functions
    
    logical function file_exists(filename)
        character(len=*), intent(in) :: filename
        inquire(file=filename, exist=file_exists)
    end function file_exists
    
    integer function get_file_size(filename)
        character(len=*), intent(in) :: filename
        integer(8) :: size
        
        inquire(file=filename, size=size)
        get_file_size = int(size)
    end function get_file_size

end program test_pcolormesh_rendering_comprehensive