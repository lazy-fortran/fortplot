module fortplot_pcolormesh
    !! Pcolormesh (pseudocolor mesh) plotting functionality
    !! 
    !! Provides 2D scalar field visualization using colored quadrilaterals
    !! on regular or irregular grids. Compatible with matplotlib pcolormesh.
    !!
    !! Following SOLID principles:
    !! - Single Responsibility: Only handles pcolormesh data structures
    !! - Open/Closed: Extensible for different shading modes
    !! - Interface Segregation: Minimal, focused interface
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_errors, only: fortplot_error_t, SUCCESS, ERROR_DIMENSION_MISMATCH, log_error
    implicit none
    
    private
    public :: pcolormesh_t, validate_pcolormesh_grid, create_regular_mesh_grid
    
    type :: pcolormesh_t
        !! Pcolormesh data container
        !! Stores grid vertices and color data for quadrilateral mesh rendering
        
        ! Grid coordinates (vertices of quadrilaterals)  
        real(wp), allocatable :: x_vertices(:,:)    ! (ny+1, nx+1)
        real(wp), allocatable :: y_vertices(:,:)    ! (ny+1, nx+1)
        
        ! Color data (one value per quadrilateral)
        real(wp), allocatable :: c_values(:,:)      ! (ny, nx)
        
        ! Colormap properties
        character(len=20) :: colormap_name = 'viridis'
        real(wp) :: vmin = -huge(1.0_wp)
        real(wp) :: vmax = huge(1.0_wp)
        logical :: vmin_set = .false.
        logical :: vmax_set = .false.
        
        ! Edge properties
        logical :: show_edges = .false.
        real(wp), dimension(3) :: edge_color = [0.0_wp, 0.0_wp, 0.0_wp]
        real(wp) :: edge_width = 0.5_wp
        
        ! Transparency
        real(wp) :: alpha = 1.0_wp
        
        ! Grid dimensions for convenience
        integer :: nx = 0  ! number of columns in C
        integer :: ny = 0  ! number of rows in C
        
    contains
        procedure :: initialize_regular_grid
        procedure :: initialize_irregular_grid
        procedure :: get_data_range
        procedure :: get_quad_vertices
    end type pcolormesh_t
     
contains

    subroutine initialize_regular_grid(self, x_coords, y_coords, c_data, colormap, error)
        !! Initialize pcolormesh with regular grid from 1D coordinate arrays
        !! 
        !! Arguments:
        !!   x_coords: 1D array of x-coordinates (length nx+1)
        !!   y_coords: 1D array of y-coordinates (length ny+1)  
        !!   c_data: 2D color data array (ny, nx)
        !!   colormap: Optional colormap name
        !!   error: Error object containing status and message
        class(pcolormesh_t), intent(inout) :: self
        real(wp), intent(in) :: x_coords(:)
        real(wp), intent(in) :: y_coords(:)
        real(wp), intent(in) :: c_data(:,:)
        character(len=*), intent(in), optional :: colormap
        type(fortplot_error_t), intent(out), optional :: error
        
        integer :: i, j, temp_dim
        
        ! Flexible dimension validation for pcolormesh
        ! Handle both Fortran-style and C-style dimension conventions gracefully
        ! The key insight: if the dimensions allow valid mesh construction, proceed
        
        ! Try Fortran-style interpretation: z(ny, nx)
        self%ny = size(c_data, 1)
        self%nx = size(c_data, 2)
        
        ! Check if coordinate arrays match Fortran-style interpretation
        if (size(x_coords) == self%nx + 1 .and. size(y_coords) == self%ny + 1) then
            ! Fortran-style matches perfectly - proceed
            ! This is the preferred convention: z(ny, nx) with x(nx+1), y(ny+1)
        ! Try C-style interpretation: z(nx, ny) -> swap dimensions for validation only
        elseif (size(x_coords) == self%ny + 1 .and. size(y_coords) == self%nx + 1) then
            ! C-style dimension pattern detected - user provided z(nx, ny)
            ! This means they want x(ny+1), y(nx+1) which is what they provided
            ! We need to swap our internal interpretation to match their convention
            temp_dim = self%nx
            self%nx = self%ny 
            self%ny = temp_dim
            ! Now self%nx and self%ny reflect the user's intended dimensions
        else
            ! Neither convention works - this is a true dimension error
            if (present(error)) then
                call error%set_error(ERROR_DIMENSION_MISMATCH, &
                    "pcolormesh: coordinate arrays incompatible with data dimensions")
            else
                call log_error(ERROR_DIMENSION_MISMATCH, "initialize_regular_grid")
            end if
            return
        end if
        
        ! Allocate arrays (deallocate first if already allocated)
        if (allocated(self%x_vertices)) deallocate(self%x_vertices)
        if (allocated(self%y_vertices)) deallocate(self%y_vertices)
        if (allocated(self%c_values)) deallocate(self%c_values)
        
        allocate(self%x_vertices(self%ny+1, self%nx+1))
        allocate(self%y_vertices(self%ny+1, self%nx+1))
        allocate(self%c_values(self%ny, self%nx))
        
        ! Create 2D meshgrid from 1D arrays
        do i = 1, self%ny + 1
            do j = 1, self%nx + 1
                self%x_vertices(i, j) = x_coords(j)
                self%y_vertices(i, j) = y_coords(i)
            end do
        end do
        
        ! Copy color data
        self%c_values = c_data
        
        ! Set colormap
        if (present(colormap)) then
            self%colormap_name = trim(colormap)
        end if
        
        ! Compute data range for auto-scaling
        call self%get_data_range()
        
        ! Set success status
        if (present(error)) call error%clear_error()
    end subroutine initialize_regular_grid
    
    subroutine initialize_irregular_grid(self, x_verts, y_verts, c_data, colormap, error)
        !! Initialize pcolormesh with irregular grid from 2D vertex arrays
        !!
        !! Arguments:
        !!   x_verts, y_verts: 2D vertex coordinate arrays (ny+1, nx+1)
        !!   c_data: 2D color data array (ny, nx)
        !!   colormap: Optional colormap name
        !!   error: Error object containing status and message
        class(pcolormesh_t), intent(inout) :: self
        real(wp), intent(in) :: x_verts(:,:)
        real(wp), intent(in) :: y_verts(:,:)
        real(wp), intent(in) :: c_data(:,:)
        character(len=*), intent(in), optional :: colormap
        type(fortplot_error_t), intent(out), optional :: error
        
        ! Validate dimensions
        self%ny = size(c_data, 1)
        self%nx = size(c_data, 2)
        
        if (size(x_verts, 1) /= self%ny + 1 .or. size(x_verts, 2) /= self%nx + 1) then
            if (present(error)) then
                call error%set_error(ERROR_DIMENSION_MISMATCH, &
                    "pcolormesh: x_verts dimensions must be (ny+1, nx+1)")
            else
                call log_error(ERROR_DIMENSION_MISMATCH, "initialize_irregular_grid")
            end if
            return
        end if
        
        if (size(y_verts, 1) /= self%ny + 1 .or. size(y_verts, 2) /= self%nx + 1) then
            if (present(error)) then
                call error%set_error(ERROR_DIMENSION_MISMATCH, &
                    "pcolormesh: y_verts dimensions must be (ny+1, nx+1)")
            else
                call log_error(ERROR_DIMENSION_MISMATCH, "initialize_irregular_grid")
            end if
            return
        end if
        
        ! Allocate and copy data (deallocate first if already allocated)
        if (allocated(self%x_vertices)) deallocate(self%x_vertices)
        if (allocated(self%y_vertices)) deallocate(self%y_vertices)
        if (allocated(self%c_values)) deallocate(self%c_values)
        
        allocate(self%x_vertices(self%ny+1, self%nx+1))
        allocate(self%y_vertices(self%ny+1, self%nx+1))
        allocate(self%c_values(self%ny, self%nx))
        
        self%x_vertices = x_verts
        self%y_vertices = y_verts
        self%c_values = c_data
        
        ! Set colormap
        if (present(colormap)) then
            self%colormap_name = trim(colormap)
        end if
        
        ! Compute data range
        call self%get_data_range()
        
        ! Set success status
        if (present(error)) call error%clear_error()
    end subroutine initialize_irregular_grid
    
    subroutine get_data_range(self)
        !! Compute data range for colormap normalization
        !! Only computes range if c_values is properly allocated
        class(pcolormesh_t), intent(inout) :: self
        
        ! Safety check: ensure c_values is allocated before accessing
        if (.not. allocated(self%c_values)) then
            ! Set default range for uninitialized data
            if (.not. self%vmin_set) self%vmin = 0.0_wp
            if (.not. self%vmax_set) self%vmax = 1.0_wp
            return
        end if
        
        ! Additional safety: check array has valid size
        if (size(self%c_values) == 0) then
            if (.not. self%vmin_set) self%vmin = 0.0_wp  
            if (.not. self%vmax_set) self%vmax = 1.0_wp
            return
        end if
        
        if (.not. self%vmin_set) then
            self%vmin = minval(self%c_values)
        end if
        if (.not. self%vmax_set) then
            self%vmax = maxval(self%c_values)
        end if
    end subroutine get_data_range
    
    subroutine get_quad_vertices(self, i, j, x_quad, y_quad)
        !! Get vertices of quadrilateral (i,j) with bounds checking
        !! 
        !! Arguments:
        !!   i, j: Quadrilateral indices (1-based)
        !!   x_quad, y_quad: Output vertex coordinates (4 elements each)
        class(pcolormesh_t), intent(in) :: self
        integer, intent(in) :: i, j
        real(wp), intent(out) :: x_quad(4), y_quad(4)
        
        ! Safety checks: ensure vertex arrays are allocated and indices valid
        if (.not. allocated(self%x_vertices) .or. .not. allocated(self%y_vertices)) then
            ! Return zero coordinates for uninitialized mesh
            x_quad = 0.0_wp
            y_quad = 0.0_wp
            return
        end if
        
        ! Check bounds to prevent array access violations
        if (i < 1 .or. i > self%ny .or. j < 1 .or. j > self%nx) then
            ! Invalid indices - return zero coordinates
            x_quad = 0.0_wp  
            y_quad = 0.0_wp
            return
        end if
        
        ! Check array dimensions to ensure safe access to (i+1, j+1)
        if (i + 1 > size(self%x_vertices, 1) .or. j + 1 > size(self%x_vertices, 2)) then
            x_quad = 0.0_wp
            y_quad = 0.0_wp
            return
        end if
        
        ! Quadrilateral vertices in counter-clockwise order
        ! Bottom-left, bottom-right, top-right, top-left
        x_quad(1) = self%x_vertices(i, j)       ! bottom-left
        y_quad(1) = self%y_vertices(i, j)
        
        x_quad(2) = self%x_vertices(i, j+1)     ! bottom-right  
        y_quad(2) = self%y_vertices(i, j+1)
        
        x_quad(3) = self%x_vertices(i+1, j+1)   ! top-right
        y_quad(3) = self%y_vertices(i+1, j+1)
        
        x_quad(4) = self%x_vertices(i+1, j)     ! top-left
        y_quad(4) = self%y_vertices(i+1, j)
    end subroutine get_quad_vertices
    
    subroutine validate_pcolormesh_grid(x_coords, y_coords, c_data, error)
        !! Validate grid dimensions for pcolormesh with flexible dimension support
        !!
        !! Accepts both Fortran-style and C-style dimension conventions:
        !! - Fortran: C(ny,nx) with x(nx+1), y(ny+1)  
        !! - C-style: C(nx,ny) with x(nx+1), y(ny+1)
        real(wp), intent(in) :: x_coords(:)
        real(wp), intent(in) :: y_coords(:)
        real(wp), intent(in) :: c_data(:,:)
        type(fortplot_error_t), intent(out), optional :: error
        
        integer :: nx, ny
        
        ! Try Fortran-style interpretation first
        ny = size(c_data, 1)
        nx = size(c_data, 2)
        
        ! Check if dimensions match either valid convention
        if ((size(x_coords) == nx + 1 .and. size(y_coords) == ny + 1) .or. &  ! Fortran style
            (size(x_coords) == ny + 1 .and. size(y_coords) == nx + 1)) then   ! C-style
            ! Valid dimension pattern - set success
            if (present(error)) call error%clear_error()
        else
            ! Invalid dimension pattern
            if (present(error)) then
                call error%set_error(ERROR_DIMENSION_MISMATCH, &
                    "pcolormesh: coordinate arrays incompatible with data dimensions")
            else
                call log_error(ERROR_DIMENSION_MISMATCH, "validate_pcolormesh_grid")
            end if
        end if
    end subroutine validate_pcolormesh_grid
    
    subroutine create_regular_mesh_grid(x_1d, y_1d, x_2d, y_2d)
        !! Create 2D meshgrid from 1D coordinate arrays
        !! Used internally for regular grid setup
        real(wp), intent(in) :: x_1d(:), y_1d(:)
        real(wp), intent(out) :: x_2d(:,:), y_2d(:,:)
        
        integer :: i, j, nx, ny
        
        ny = size(y_1d)
        nx = size(x_1d)
        
        do i = 1, ny
            do j = 1, nx
                x_2d(i, j) = x_1d(j)
                y_2d(i, j) = y_1d(i)
            end do
        end do
    end subroutine create_regular_mesh_grid

end module fortplot_pcolormesh