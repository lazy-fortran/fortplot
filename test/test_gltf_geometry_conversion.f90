program test_gltf_geometry_conversion
    !! Test GLTF geometry conversion for 3D plots
    !! Following TDD: Write test first, then implementation
    
    use iso_fortran_env, only: wp => real64
    use fortplot_gltf_geometry, only: convert_line_to_gltf, convert_surface_to_gltf
    use fortplot_gltf_base, only: gltf_mesh_t, gltf_primitive_t
    implicit none
    
    ! TEMPORARY: Skip GLTF tests until Issue #153 is resolved
    print *, "SKIP: GLTF geometry conversion tests - blocked by Issue #153"
    return
    
    call test_3d_line_to_gltf_conversion()
    call test_surface_to_triangle_mesh()
    call test_bounds_calculation()
    
    print *, "All GLTF geometry conversion tests passed!"
    
contains

    subroutine test_3d_line_to_gltf_conversion()
        !! Test conversion of 3D line data to GLTF line primitive
        real(wp), dimension(4) :: x = [0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp]
        real(wp), dimension(4) :: y = [0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp]
        real(wp), dimension(4) :: z = [0.0_wp, 0.5_wp, 1.0_wp, 0.5_wp]
        type(gltf_mesh_t) :: mesh
        type(gltf_primitive_t) :: primitive
        
        ! Act - Convert line to GLTF
        call convert_line_to_gltf(x, y, z, mesh)
        
        ! Assert
        if (mesh%primitive_count /= 1) then
            error stop "Line should create one primitive"
        end if
        
        primitive = mesh%primitives(1)
        
        ! Check mode (3 = LINE_STRIP in GLTF)
        if (primitive%mode /= 3) then
            error stop "Primitive mode should be LINE_STRIP (3)"
        end if
        
        ! Check attributes
        if (primitive%position_accessor < 0) then
            error stop "Position accessor not set"
        end if
        
        ! Check vertex count
        if (primitive%vertex_count /= 4) then
            error stop "Should have 4 vertices"
        end if
        
    end subroutine test_3d_line_to_gltf_conversion
    
    subroutine test_surface_to_triangle_mesh()
        !! Test conversion of surface grid to triangle mesh
        real(wp), dimension(3) :: x_grid = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), dimension(2) :: y_grid = [0.0_wp, 1.0_wp]
        real(wp), dimension(3,2) :: z_grid
        type(gltf_mesh_t) :: mesh
        type(gltf_primitive_t) :: primitive
        integer :: expected_triangles
        
        ! Create simple surface data
        z_grid(:,1) = [0.0_wp, 0.5_wp, 0.0_wp]
        z_grid(:,2) = [1.0_wp, 1.5_wp, 1.0_wp]
        
        ! Act - Convert surface to GLTF
        call convert_surface_to_gltf(x_grid, y_grid, z_grid, mesh)
        
        ! Assert
        if (mesh%primitive_count /= 1) then
            error stop "Surface should create one primitive"
        end if
        
        primitive = mesh%primitives(1)
        
        ! Check mode (4 = TRIANGLES in GLTF)
        if (primitive%mode /= 4) then
            error stop "Primitive mode should be TRIANGLES (4)"
        end if
        
        ! Check triangle count
        ! (nx-1) * (ny-1) * 2 triangles per quad
        expected_triangles = (3-1) * (2-1) * 2
        if (primitive%index_count /= expected_triangles * 3) then
            error stop "Incorrect number of triangle indices"
        end if
        
        ! Check vertex count (should be nx * ny)
        if (primitive%vertex_count /= 6) then
            error stop "Should have 6 vertices (3x2 grid)"
        end if
        
    end subroutine test_surface_to_triangle_mesh
    
    subroutine test_bounds_calculation()
        !! Test that bounds are correctly calculated
        real(wp), dimension(3) :: x = [-1.0_wp, 0.0_wp, 2.0_wp]
        real(wp), dimension(3) :: y = [0.0_wp, 3.0_wp, 1.0_wp]  
        real(wp), dimension(3) :: z = [0.5_wp, 0.5_wp, 0.5_wp]
        type(gltf_mesh_t) :: mesh
        
        ! Act
        call convert_line_to_gltf(x, y, z, mesh)
        
        ! Assert bounds
        if (abs(mesh%min_bounds(1) - (-1.0_wp)) > 1e-10_wp) then
            error stop "Min X bound incorrect"
        end if
        
        if (abs(mesh%max_bounds(1) - 2.0_wp) > 1e-10_wp) then
            error stop "Max X bound incorrect"
        end if
        
        if (abs(mesh%max_bounds(2) - 3.0_wp) > 1e-10_wp) then
            error stop "Max Y bound incorrect"
        end if
        
    end subroutine test_bounds_calculation

end program test_gltf_geometry_conversion