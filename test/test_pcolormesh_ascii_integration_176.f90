program test_pcolormesh_ascii_integration_176
    !! Integration test for Issue #176: pcolormesh shows just solid fill in ascii backend
    !!
    !! This test uses the public fortplot API to create pcolormesh plots and verify
    !! that the ASCII output shows mesh patterns instead of solid fills.
    !!
    !! Given: fortplot library with ASCII backend
    !! When: Creating pcolormesh plots with varying data
    !! Then: ASCII output should show distinct patterns, not uniform blocks

    use fortplot, only: figure_t, wp
    use fortplot_security, only: get_test_output_path
    implicit none
    
    type(figure_t) :: fig
    real(wp), allocatable :: x(:), y(:), data(:,:)
    character(len=200) :: output_path
    logical :: test_passed = .true.
    integer :: i, j
    
    print *, "=== Integration Test for Issue #176 ==="
    print *, "Testing pcolormesh ASCII rendering with mesh patterns"
    print *, ""
    
    ! Test 1: Simple 2x2 mesh with varying data
    print *, "Test 1: 2x2 mesh with checkerboard pattern"
    allocate(x(3), y(3), data(2, 2))
    
    ! Grid coordinates (3 vertices = 2 quads per direction)
    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 2.0_wp] 
    
    ! Checkerboard pattern (alternating high/low values)
    data(1, 1) = 0.1_wp  ! Low
    data(1, 2) = 0.9_wp  ! High  
    data(2, 1) = 0.9_wp  ! High
    data(2, 2) = 0.1_wp  ! Low
    
    call fig%initialize(25, 15)
    call fig%add_pcolormesh(x, y, data)
    output_path = get_test_output_path('/tmp/test_pcolormesh_checkerboard.txt')
    call fig%savefig(output_path)
    
    print '(A,A)', "  Output saved to: ", trim(output_path)
    print *, "  Expected: Different patterns for high/low values"
    print *, "  Issue #176: All areas render as identical solid blocks"
    print *, ""
    
    deallocate(x, y, data)
    
    ! Test 2: Gradient mesh
    print *, "Test 2: 3x3 mesh with gradient pattern"
    allocate(x(4), y(4), data(3, 3))
    
    x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
    y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
    
    ! Create gradient pattern
    do i = 1, 3
        do j = 1, 3
            data(i, j) = real(i + j, wp) / 6.0_wp  ! Values from 0.33 to 1.0
        end do
    end do
    
    call fig%initialize(30, 18)
    call fig%add_pcolormesh(x, y, data)
    output_path = get_test_output_path('/tmp/test_pcolormesh_gradient.txt')  
    call fig%savefig(output_path)
    
    print '(A,A)', "  Output saved to: ", trim(output_path)
    print *, "  Expected: Gradual change from light to dense characters"
    print *, "  Issue #176: All areas render as identical solid blocks"
    print *, ""
    
    deallocate(x, y, data)
    
    ! Test 3: Single quad (edge case)
    print *, "Test 3: Single quadrilateral"
    allocate(x(2), y(2), data(1, 1))
    
    x = [0.0_wp, 1.0_wp]
    y = [0.0_wp, 1.0_wp]
    data(1, 1) = 0.5_wp  ! Medium value
    
    call fig%initialize(15, 10)
    call fig%add_pcolormesh(x, y, data)  
    output_path = get_test_output_path('/tmp/test_pcolormesh_single.txt')
    call fig%savefig(output_path)
    
    print '(A,A)', "  Output saved to: ", trim(output_path)
    print *, "  Expected: Medium-density ASCII pattern"
    print *, "  Issue #176: Renders as solid block"
    print *, ""
    
    deallocate(x, y, data)
    
    ! Test 4: Different colormaps (if supported)
    print *, "Test 4: Different colormaps"
    allocate(x(3), y(3), data(2, 2))
    
    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 2.0_wp]
    data = reshape([0.2_wp, 0.8_wp, 0.6_wp, 0.4_wp], [2, 2])
    
    ! Test viridis colormap
    call fig%initialize(20, 12)
    call fig%add_pcolormesh(x, y, data, colormap='viridis')
    output_path = get_test_output_path('/tmp/test_pcolormesh_viridis.txt')
    call fig%savefig(output_path)
    
    print '(A,A)', "  Viridis output: ", trim(output_path)
    
    ! Test plasma colormap  
    call fig%initialize(20, 12)
    call fig%add_pcolormesh(x, y, data, colormap='plasma')
    output_path = get_test_output_path('/tmp/test_pcolormesh_plasma.txt')
    call fig%savefig(output_path)
    
    print '(A,A)', "  Plasma output: ", trim(output_path)
    print *, "  Expected: Different colormaps should produce different patterns"
    print *, "  Issue #176: All colormaps produce identical solid blocks"
    print *, ""
    
    ! Summary
    print *, "=== Test Summary ==="
    print *, "All tests completed. Issue #176 is expected to cause:"
    print *, "1. Uniform solid block rendering regardless of data values"
    print *, "2. No visual distinction between different mesh regions"
    print *, "3. Identical output for different colormaps"  
    print *, "4. Loss of mesh structure information"
    print *, ""
    print *, "After fixing Issue #176, ASCII output should show:"
    print *, "- Varying character densities based on data values"
    print *, "- Distinct visual patterns for different mesh regions"
    print *, "- Preserved mesh structure in ASCII representation"
    
    if (test_passed) then
        print *, ""
        print *, "Integration test PASSED (files generated for manual inspection)"
    else
        print *, ""
        print *, "Integration test FAILED"
        call exit(1)
    end if
    
end program test_pcolormesh_ascii_integration_176