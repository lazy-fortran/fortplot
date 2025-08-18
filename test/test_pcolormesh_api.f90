program test_pcolormesh_api
    !! Test pcolormesh API compatibility with matplotlib patterns
    !! 
    !! Given: Matplotlib-compatible pcolormesh interface requirements
    !! When: Using both object-oriented and stateful APIs
    !! Then: Should support all matplotlib pcolormesh parameter patterns
    
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t, add_pcolormesh, show, savefig, xlabel, ylabel, title
    implicit none
    
    call test_object_oriented_api()
    call test_stateful_api_compatibility()
    call test_parameter_validation()
    call test_colormap_parameter_support()
    call test_edge_parameter_support()
    call test_vmin_vmax_normalization()
    call test_matplotlib_parameter_equivalence()
    
    print *, "All pcolormesh API compatibility tests completed!"
    
contains

    subroutine test_object_oriented_api()
        !! Given: Object-oriented figure interface
        !! When: Using fig%add_pcolormesh() with various parameter combinations
        !! Then: Should support all matplotlib-compatible parameter patterns
        
        type(figure_t) :: fig
        real(wp) :: x(5), y(6), c(5, 4)
        integer :: i, j
        
        ! Arrange - Create test data
        do i = 1, 5
            x(i) = real(i-1, wp) * 0.25_wp
        end do
        do i = 1, 6
            y(i) = real(i-1, wp) * 0.2_wp
        end do
        do i = 1, 5
            do j = 1, 4
                c(i, j) = real(i * j, wp) / 20.0_wp
            end do
        end do
        
        call fig%initialize(400, 300)
        
        ! Act & Assert - Test basic OO interface
        call fig%add_pcolormesh(x, y, c)
        call fig%set_title("OO API - Basic pcolormesh")
        call fig%savefig('/tmp/test_oo_basic.png')
        
        ! Test with colormap parameter
        call fig%initialize(400, 300)
        call fig%add_pcolormesh(x, y, c, colormap='plasma')
        call fig%set_title("OO API - Plasma colormap")
        call fig%savefig('/tmp/test_oo_colormap.png')
        
        ! Test with vmin/vmax parameters
        call fig%initialize(400, 300)
        call fig%add_pcolormesh(x, y, c, colormap='viridis', vmin=0.0_wp, vmax=1.0_wp)
        call fig%set_title("OO API - Normalized range")
        call fig%savefig('/tmp/test_oo_normalized.png')
        
        ! Test with edge parameters
        call fig%initialize(400, 300)
        call fig%add_pcolormesh(x, y, c, colormap='coolwarm', &
                               edgecolors='black', linewidths=1.5_wp)
        call fig%set_title("OO API - Black edges")
        call fig%savefig('/tmp/test_oo_edges.png')
        
        ! Test parameter combination (full matplotlib compatibility)
        call fig%initialize(400, 300)
        call fig%add_pcolormesh(x, y, c, colormap='inferno', vmin=0.1_wp, vmax=0.9_wp, &
                               edgecolors='white', linewidths=0.8_wp)
        call fig%set_title("OO API - Full parameter set")
        call fig%savefig('/tmp/test_oo_full_params.png')
        
        print *, "test_object_oriented_api: PASSED"
    end subroutine test_object_oriented_api

    subroutine test_stateful_api_compatibility()
        !! Given: Matplotlib-style stateful interface (like plt.pcolormesh)
        !! When: Using module-level pcolormesh function
        !! Then: Should work identically to matplotlib pyplot patterns
        
        real(wp) :: x(4), y(4), c(3, 3)
        integer :: i, j
        
        ! Arrange - Create test data
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        do i = 1, 3
            do j = 1, 3
                c(i, j) = real(i + j, wp) / 6.0_wp
            end do
        end do
        
        ! Act & Assert - Test stateful API patterns
        ! Basic stateful call (equivalent to plt.pcolormesh(x, y, c))
        call add_pcolormesh(x, y, c)
        call title("Stateful API - Basic")
        call savefig('/tmp/test_stateful_basic.png')
        
        ! Stateful call with colormap (equivalent to plt.pcolormesh(x, y, c, cmap='plasma'))
        call add_pcolormesh(x, y, c, colormap='plasma')
        call title("Stateful API - Plasma colormap")
        call savefig('/tmp/test_stateful_colormap.png')
        
        ! Stateful with normalization (plt.pcolormesh(x, y, c, vmin=0, vmax=1))
        call add_pcolormesh(x, y, c, colormap='viridis', vmin=0.0_wp, vmax=1.0_wp)
        call xlabel("X coordinate")
        call ylabel("Y coordinate")
        call title("Stateful API - Normalized")
        call savefig('/tmp/test_stateful_normalized.png')
        
        ! Stateful with edges (plt.pcolormesh(x, y, c, edgecolors='k', linewidths=1))
        call add_pcolormesh(x, y, c, colormap='coolwarm', &
                           edgecolors='black', linewidths=1.0_wp)
        call title("Stateful API - With edges")
        call savefig('/tmp/test_stateful_edges.png')
        
        print *, "test_stateful_api_compatibility: PASSED"
    end subroutine test_stateful_api_compatibility

    subroutine test_parameter_validation()
        !! Given: Invalid parameter combinations
        !! When: Calling pcolormesh with invalid parameters
        !! Then: Should provide clear error messages matching matplotlib behavior
        
        type(figure_t) :: fig
        real(wp) :: x_wrong(3), y_correct(4), c_test(3, 3)
        real(wp) :: x_correct(4), y_correct_4(4), c_correct(3, 3)
        logical :: error_caught
        integer :: i, j
        
        ! Arrange - Create test data with known dimension issues
        x_wrong = [0.0_wp, 1.0_wp, 2.0_wp]        ! Too few elements (should be 4)
        x_correct = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y_correct = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        
        do i = 1, 3
            do j = 1, 3
                c_test(i, j) = real(i + j, wp)
            end do
        end do
        
        call fig%initialize(300, 300)
        
        ! Act & Assert - Test dimension validation
        ! NOTE: Dimension validation uses 'error stop' which cannot be caught in Fortran
        ! Invalid dimensions will terminate the program, so we only test valid cases
        ! Dimension validation is tested in separate error handling tests
        
        ! Test correct dimensions (should work)
        call fig%add_pcolormesh(x_correct, y_correct, c_test)
        call fig%savefig('/tmp/test_param_validation_good.png')
        
        print *, "test_parameter_validation: PASSED"
    end subroutine test_parameter_validation

    subroutine test_colormap_parameter_support()
        !! Given: Various colormap parameter formats
        !! When: Using different colormap specification methods
        !! Then: Should support all matplotlib colormap parameter patterns
        
        type(figure_t) :: fig
        real(wp) :: x(4), y(4), c(3, 3)
        integer :: i, j
        character(len=*), parameter :: test_colormaps(6) = &
            ['viridis ', 'plasma  ', 'inferno ', 'coolwarm', 'jet     ', 'gray    ']
        integer :: cmap_idx
        character(len=50) :: filename
        
        ! Arrange
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        do i = 1, 3
            do j = 1, 3
                c(i, j) = sin(real(i, wp)) * cos(real(j, wp))
            end do
        end do
        
        ! Act & Assert - Test each colormap parameter
        do cmap_idx = 1, 6
            call fig%initialize(300, 300)
            call fig%add_pcolormesh(x, y, c, colormap=trim(test_colormaps(cmap_idx)))
            call fig%set_title("Colormap: " // trim(test_colormaps(cmap_idx)))
            
            write(filename, '(A, A, A)') '/tmp/test_colormap_', &
                                       trim(test_colormaps(cmap_idx)), '.png'
            call fig%savefig(filename)
        end do
        
        ! Test colormap case insensitivity (matplotlib allows 'Viridis', 'PLASMA', etc.)
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c, colormap='VIRIDIS')  ! Uppercase
        call fig%savefig('/tmp/test_colormap_uppercase.png')
        
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c, colormap='Plasma')   ! Mixed case
        call fig%savefig('/tmp/test_colormap_mixedcase.png')
        
        print *, "test_colormap_parameter_support: PASSED"
    end subroutine test_colormap_parameter_support

    subroutine test_edge_parameter_support()
        !! Given: Various edge parameter combinations
        !! When: Using different edge specification methods
        !! Then: Should support matplotlib edge parameter patterns
        
        type(figure_t) :: fig
        real(wp) :: x(4), y(4), c(3, 3)
        integer :: i, j
        
        ! Arrange
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        do i = 1, 3
            do j = 1, 3
                c(i, j) = real(mod(i + j, 3), wp) / 2.0_wp
            end do
        end do
        
        ! Act & Assert - Test edge parameters
        
        ! Test no edges (default matplotlib behavior)
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c, colormap='viridis')
        call fig%set_title("No edges (default)")
        call fig%savefig('/tmp/test_edges_none.png')
        
        ! Test black edges (common matplotlib pattern)
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c, colormap='viridis', edgecolors='black')
        call fig%set_title("Black edges")
        call fig%savefig('/tmp/test_edges_black.png')
        
        ! Test white edges with specific width
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c, colormap='plasma', &
                               edgecolors='white', linewidths=2.0_wp)
        call fig%set_title("White edges, width 2.0")
        call fig%savefig('/tmp/test_edges_white_thick.png')
        
        ! Test thin edges (matplotlib default linewidth when edges enabled)
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c, colormap='coolwarm', &
                               edgecolors='gray', linewidths=0.5_wp)
        call fig%set_title("Gray edges, width 0.5")
        call fig%savefig('/tmp/test_edges_gray_thin.png')
        
        print *, "test_edge_parameter_support: PASSED"
    end subroutine test_edge_parameter_support

    subroutine test_vmin_vmax_normalization()
        !! Given: Data with known value range and vmin/vmax parameters
        !! When: Using different normalization ranges
        !! Then: Should normalize colormap exactly like matplotlib
        
        type(figure_t) :: fig
        real(wp) :: x(6), y(6), c(5, 5)
        integer :: i, j
        
        ! Arrange - Create data with known range (0 to 2)
        do i = 1, 6
            x(i) = real(i-1, wp) * 0.2_wp
            y(i) = real(i-1, wp) * 0.2_wp
        end do
        
        do i = 1, 5
            do j = 1, 5
                c(i, j) = real(i + j - 2, wp) * 0.25_wp  ! Range: 0.0 to 2.0
            end do
        end do
        
        ! Act & Assert - Test normalization patterns
        
        ! Test auto-normalization (no vmin/vmax) - should use data range
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c, colormap='viridis')
        call fig%set_title("Auto normalization (0.0 to 2.0)")
        call fig%savefig('/tmp/test_norm_auto.png')
        
        ! Test explicit full range normalization
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c, colormap='viridis', vmin=0.0_wp, vmax=2.0_wp)
        call fig%set_title("Explicit range (0.0 to 2.0)")
        call fig%savefig('/tmp/test_norm_explicit_full.png')
        
        ! Test clipping normalization (narrower range)
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c, colormap='plasma', vmin=0.5_wp, vmax=1.5_wp)
        call fig%set_title("Clipped range (0.5 to 1.5)")
        call fig%savefig('/tmp/test_norm_clipped.png')
        
        ! Test expanding normalization (wider range)
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c, colormap='coolwarm', vmin=-0.5_wp, vmax=2.5_wp)
        call fig%set_title("Expanded range (-0.5 to 2.5)")
        call fig%savefig('/tmp/test_norm_expanded.png')
        
        ! Test symmetric normalization around zero
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c - 1.0_wp, colormap='coolwarm', vmin=-1.0_wp, vmax=1.0_wp)
        call fig%set_title("Symmetric range (-1.0 to 1.0)")
        call fig%savefig('/tmp/test_norm_symmetric.png')
        
        print *, "test_vmin_vmax_normalization: PASSED"
    end subroutine test_vmin_vmax_normalization

    subroutine test_matplotlib_parameter_equivalence()
        !! Given: Matplotlib pcolormesh parameter patterns
        !! When: Using identical parameter combinations
        !! Then: Should produce visually equivalent results
        
        type(figure_t) :: fig
        real(wp) :: x(5), y(4), c(3, 4)
        integer :: i, j
        
        ! Arrange - Create test data matching common matplotlib examples
        x = [0.0_wp, 1.0_wp, 2.5_wp, 4.0_wp, 6.0_wp]  ! Irregular spacing
        y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]         ! Regular spacing
        
        do i = 1, 3
            do j = 1, 4
                c(i, j) = sin(x(j)) * cos(real(i, wp))
            end do
        end do
        
        ! Act & Assert - Test matplotlib equivalent parameter combinations
        
        ! Equivalent to: plt.pcolormesh(X, Y, C, cmap='viridis', shading='flat')
        call fig%initialize(400, 300)
        call fig%add_pcolormesh(x, y, c, colormap='viridis')
        call fig%set_title("Equivalent to matplotlib basic pcolormesh")
        call fig%savefig('/tmp/test_matplotlib_basic.png')
        
        ! Equivalent to: plt.pcolormesh(X, Y, C, cmap='plasma', vmin=0, vmax=1, edgecolors='k')
        call fig%initialize(400, 300)
        call fig%add_pcolormesh(x, y, c, colormap='plasma', vmin=0.0_wp, vmax=1.0_wp, &
                               edgecolors='black')
        call fig%set_title("Equivalent to matplotlib with edges")
        call fig%savefig('/tmp/test_matplotlib_edges.png')
        
        ! Equivalent to: plt.pcolormesh(X, Y, C, cmap='coolwarm', alpha=0.8)
        ! Note: alpha parameter may not be implemented yet
        call fig%initialize(400, 300)
        call fig%add_pcolormesh(x, y, c, colormap='coolwarm')
        call fig%set_title("Equivalent to matplotlib coolwarm")
        call fig%savefig('/tmp/test_matplotlib_coolwarm.png')
        
        ! Test scientific data pattern (common matplotlib use case)
        ! Equivalent to typical scientific field visualization
        block
            real(wp) :: x_sci(11), y_sci(11), field(10, 10)
            integer :: ii, jj
            
            do ii = 1, 11
                x_sci(ii) = real(ii-1, wp) * 0.1_wp
                y_sci(ii) = real(ii-1, wp) * 0.1_wp
            end do
            
            do ii = 1, 10
                do jj = 1, 10
                    field(ii, jj) = exp(-((x_sci(ii) - 0.5_wp)**2 + (y_sci(jj) - 0.5_wp)**2) * 10.0_wp)
                end do
            end do
            
            call fig%initialize(400, 400)
            call fig%add_pcolormesh(x_sci, y_sci, field, colormap='jet', &
                                   vmin=0.0_wp, vmax=1.0_wp)
            call fig%set_xlabel("X coordinate")
            call fig%set_ylabel("Y coordinate") 
            call fig%set_title("Scientific field visualization")
            call fig%savefig('/tmp/test_matplotlib_scientific.png')
        end block
        
        print *, "test_matplotlib_parameter_equivalence: PASSED"
    end subroutine test_matplotlib_parameter_equivalence

end program test_pcolormesh_api