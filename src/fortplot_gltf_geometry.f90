module fortplot_gltf_geometry
    !! Module for converting plot geometry to GLTF format
    !! Following SRP - each routine handles specific geometry conversion
    !! Following KISS - straightforward conversion algorithms
    
    use iso_fortran_env, only: wp => real64
    use fortplot_gltf_base, only: gltf_mesh_t, gltf_primitive_t, &
                                 GLTF_MODE_TRIANGLES, GLTF_MODE_LINE_STRIP
    implicit none
    
    private
    public :: convert_line_to_gltf
    public :: convert_surface_to_gltf
    
contains

    subroutine convert_line_to_gltf(x, y, z, mesh)
        !! Convert 3D line data to GLTF line strip primitive
        !! Following KISS - simple direct conversion
        real(wp), intent(in) :: x(:), y(:), z(:)
        type(gltf_mesh_t), intent(out) :: mesh
        
        integer :: n_points
        
        n_points = size(x)
        
        ! Allocate mesh with one primitive
        mesh%primitive_count = 1
        allocate(mesh%primitives(1))
        
        ! Set up line strip primitive
        mesh%primitives(1)%mode = GLTF_MODE_LINE_STRIP
        mesh%primitives(1)%vertex_count = n_points
        mesh%primitives(1)%position_accessor = 0  ! Will be set later
        
        ! Calculate bounds
        call calculate_bounds(x, y, z, mesh%min_bounds, mesh%max_bounds)
        
    end subroutine convert_line_to_gltf
    
    subroutine convert_surface_to_gltf(x_grid, y_grid, z_grid, mesh)
        !! Convert surface grid to GLTF triangle mesh
        !! Following SRP - only handles surface triangulation
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        type(gltf_mesh_t), intent(out) :: mesh
        
        integer :: nx, ny, n_vertices, n_triangles
        
        nx = size(x_grid)
        ny = size(y_grid)
        n_vertices = nx * ny
        n_triangles = (nx-1) * (ny-1) * 2  ! Two triangles per quad
        
        ! Allocate mesh with one primitive
        mesh%primitive_count = 1
        allocate(mesh%primitives(1))
        
        ! Set up triangles primitive
        mesh%primitives(1)%mode = GLTF_MODE_TRIANGLES
        mesh%primitives(1)%vertex_count = n_vertices
        mesh%primitives(1)%index_count = n_triangles * 3
        mesh%primitives(1)%position_accessor = 0  ! Will be set later
        mesh%primitives(1)%indices_accessor = 1   ! Will be set later
        
        ! Calculate bounds from grid
        call calculate_grid_bounds(x_grid, y_grid, z_grid, &
                                  mesh%min_bounds, mesh%max_bounds)
        
    end subroutine convert_surface_to_gltf
    
    subroutine calculate_bounds(x, y, z, min_bounds, max_bounds)
        !! Calculate bounding box for 3D data
        !! Following KISS - simple min/max calculation
        real(wp), intent(in) :: x(:), y(:), z(:)
        real(wp), intent(out) :: min_bounds(3), max_bounds(3)
        
        min_bounds(1) = minval(x)
        max_bounds(1) = maxval(x)
        min_bounds(2) = minval(y)
        max_bounds(2) = maxval(y)
        min_bounds(3) = minval(z)
        max_bounds(3) = maxval(z)
        
    end subroutine calculate_bounds
    
    subroutine calculate_grid_bounds(x_grid, y_grid, z_grid, min_bounds, max_bounds)
        !! Calculate bounding box for grid data
        !! Following DRY - reuses logic but for grid structure
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(out) :: min_bounds(3), max_bounds(3)
        
        min_bounds(1) = minval(x_grid)
        max_bounds(1) = maxval(x_grid)
        min_bounds(2) = minval(y_grid)
        max_bounds(2) = maxval(y_grid)
        min_bounds(3) = minval(z_grid)
        max_bounds(3) = maxval(z_grid)
        
    end subroutine calculate_grid_bounds

end module fortplot_gltf_geometry