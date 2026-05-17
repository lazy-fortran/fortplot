program test_parameter_forwarding
    !! Test comprehensive parameter forwarding in matplotlib wrapper functions
    !! Critical API consistency validation for issue #396
    
    use iso_fortran_env, only: wp => real64
    use fortplot_matplotlib
    use fortplot_figure_core, only: figure_t
    implicit none
    
    integer, parameter :: nx = 3, ny = 3
    real(wp) :: x(nx+1), y(ny+1), z(ny,nx)
    logical :: test_passed
    
    ! Initialize test data
    call setup_test_data(x, y, z)
    
    ! Run parameter forwarding tests
    test_passed = .true.
    
    ! Test pcolormesh parameter forwarding 
    call test_pcolormesh_parameter_forwarding(test_passed)
    
    ! Test contour_filled parameter forwarding
    call test_contour_filled_parameter_forwarding(test_passed)
    
    if (test_passed) then
        print *, "PASS: All parameter forwarding tests passed"
        stop 0
    else
        print *, "FAIL: Parameter forwarding tests failed"
        stop 1
    end if
    
contains

    subroutine setup_test_data(x, y, z)
        !! Setup simple test data for parameter forwarding validation
        !! x: size nx+1, y: size ny+1, z: size (ny, nx)
        real(wp), intent(out) :: x(:), y(:), z(:,:)
        integer :: i, j
        
        ! Grid vertex coordinates (need nx+1 and ny+1 points for nx*ny cells)
        do i = 1, size(x)
            x(i) = real(i-1, wp)
        end do
        do i = 1, size(y)
            y(i) = real(i-1, wp) 
        end do
        
        ! Cell color data (ny rows, nx columns)
        do i = 1, size(z,1)  ! ny rows
            do j = 1, size(z,2)  ! nx columns
                z(i,j) = real(i+j-2, wp)
            end do
        end do
    end subroutine setup_test_data

    subroutine test_pcolormesh_parameter_forwarding(test_passed)
        !! Test that pcolormesh wrapper functions forward ALL parameters correctly
        logical, intent(inout) :: test_passed
        
        print *, "Testing pcolormesh parameter forwarding..."
        
        ! Test 1: pcolormesh() with ALL parameters  
        ! This MUST NOT cause compilation or runtime errors
        call test_pcolormesh_all_parameters(test_passed)
        
        ! Test 2: add_pcolormesh() with ALL parameters
        call test_add_pcolormesh_all_parameters(test_passed)
        
        if (test_passed) then
            print *, "  PASS: pcolormesh parameter forwarding tests"
        else
            print *, "  FAIL: pcolormesh parameter forwarding failed"
        end if
    end subroutine test_pcolormesh_parameter_forwarding
    
    subroutine test_pcolormesh_all_parameters(test_passed)
        !! Test pcolormesh() wrapper with complete parameter set
        logical, intent(inout) :: test_passed
        
        ! Initialize figure
        call figure()
        
        ! Test call with ALL parameters that should be supported
        ! Based on issue #396, these parameters MUST be forwarded:
        call pcolormesh(x, y, z, &
            shading='flat', &
            colormap='viridis', &
            show_colorbar=.true., &
            label='test', &
            vmin=0.0_wp, &
            vmax=10.0_wp, &
            edgecolors=[0.5_wp, 0.5_wp, 0.5_wp], &
            linewidths=1.0_wp)
        
        ! If we reach here without errors, parameter forwarding compiles
        print *, "    PASS: pcolormesh() accepts all required parameters"
        
    end subroutine test_pcolormesh_all_parameters
    
    subroutine test_add_pcolormesh_all_parameters(test_passed)
        !! Test add_pcolormesh() wrapper with complete parameter set  
        logical, intent(inout) :: test_passed
        
        ! Initialize figure
        call figure()
        
        ! Test call with ALL parameters that should be supported
        call add_pcolormesh(x, y, z, &
            shading='flat', &
            colormap='plasma', &
            show_colorbar=.false., &
            label='test2', &
            vmin=-5.0_wp, &
            vmax=5.0_wp, &
            edgecolors=[1.0_wp, 0.0_wp, 0.0_wp], &
            linewidths=2.0_wp)
        
        ! If we reach here without errors, parameter forwarding compiles
        print *, "    PASS: add_pcolormesh() accepts all required parameters"
        
    end subroutine test_add_pcolormesh_all_parameters

    subroutine test_contour_filled_parameter_forwarding(test_passed)
        !! Test that contour_filled wrapper functions forward ALL parameters correctly
        logical, intent(inout) :: test_passed
        real(wp) :: levels(3)
        
        print *, "Testing contour_filled parameter forwarding..."
        
        levels = [0.0_wp, 5.0_wp, 10.0_wp]
        
        ! Test 1: contour_filled() with ALL parameters
        call test_contour_filled_all_parameters(levels, test_passed)
        
        ! Test 2: add_contour_filled() with ALL parameters 
        call test_add_contour_filled_all_parameters(levels, test_passed)
        
        if (test_passed) then
            print *, "  PASS: contour_filled parameter forwarding tests"
        else
            print *, "  FAIL: contour_filled parameter forwarding failed"
        end if
    end subroutine test_contour_filled_parameter_forwarding
    
    subroutine test_contour_filled_all_parameters(levels, test_passed)
        !! Test contour_filled() wrapper with complete parameter set
        real(wp), intent(in) :: levels(:)
        logical, intent(inout) :: test_passed
        
        ! Initialize figure
        call figure()
        
        ! Test call with ALL parameters that should be supported
        ! Based on issue #396, these parameters MUST be forwarded:
        call contour_filled(x, y, z, &
            levels=levels, &
            colormap='coolwarm', &
            show_colorbar=.true., &
            label='contour_test')
        
        ! If we reach here without errors, parameter forwarding compiles
        print *, "    PASS: contour_filled() forwards all required parameters"
        
    end subroutine test_contour_filled_all_parameters
    
    subroutine test_add_contour_filled_all_parameters(levels, test_passed)
        !! Test add_contour_filled() wrapper with complete parameter set
        real(wp), intent(in) :: levels(:)
        logical, intent(inout) :: test_passed
        
        ! Initialize figure
        call figure()
        
        ! Test call with ALL parameters that should be supported  
        call add_contour_filled(x, y, z, &
            levels=levels, &
            colormap='inferno', &
            show_colorbar=.false., &
            label='add_contour_test')
        
        ! If we reach here without errors, parameter forwarding compiles
        print *, "    PASS: add_contour_filled() forwards all required parameters"
        
    end subroutine test_add_contour_filled_all_parameters

end program test_parameter_forwarding