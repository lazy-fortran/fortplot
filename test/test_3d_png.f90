program test_3d_png
    use fortplot
    use fortplot_testing
    use iso_fortran_env, only: wp => real64
    implicit none

    call test_surface_3d_png()
    call test_wireframe_3d_png()
    call test_scatter_3d_png()
    call test_projection_transformation()
    
    print *, "All 3D PNG tests passed!"

contains

    subroutine test_surface_3d_png()
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), z(:,:)
        integer :: i, j
        real(wp) :: xi, yi
        
        ! Create test data
        allocate(x(20), y(20), z(20,20))
        do i = 1, 20
            x(i) = -2.0 + 4.0 * (i-1) / 19.0
            y(i) = -2.0 + 4.0 * (i-1) / 19.0
        end do
        
        do i = 1, 20
            do j = 1, 20
                xi = x(i)
                yi = y(j)
                z(i,j) = exp(-(xi**2 + yi**2))
            end do
        end do
        
        call fig%initialize(width=800, height=600)
        call fig%add_surface(x, y, z)
        call fig%savefig('output/test/test_3d_png/test_surface_3d.png')
        
        ! Test file exists
        call assert_file_exists('output/test/test_3d_png/test_surface_3d.png')
    end subroutine

    subroutine test_wireframe_3d_png()
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), z(:,:)
        integer :: i, j
        
        ! Create test data
        allocate(x(10), y(10), z(10,10))
        do i = 1, 10
            x(i) = real(i-1, wp)
            y(i) = real(i-1, wp)
        end do
        
        do i = 1, 10
            do j = 1, 10
                z(i,j) = sin(sqrt(x(i)**2 + y(j)**2))
            end do
        end do
        
        call fig%initialize(width=800, height=600)
        ! Wireframe will be implemented later, for now use surface
        call fig%add_surface(x, y, z, label="wireframe")
        call fig%savefig('output/test/test_3d_png/test_wireframe_3d.png')
        
        ! Test file exists
        call assert_file_exists('output/test/test_3d_png/test_wireframe_3d.png')
    end subroutine

    subroutine test_scatter_3d_png()
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), z(:)
        integer :: i
        
        ! Create test data
        allocate(x(50), y(50), z(50))
        do i = 1, 50
            x(i) = cos(real(i, wp) * 0.2)
            y(i) = sin(real(i, wp) * 0.2)
            z(i) = real(i, wp) * 0.05
        end do
        
        call fig%initialize(width=800, height=600)
        call fig%add_3d_plot(x, y, z)
        call fig%savefig('output/test/test_3d_png/test_scatter_3d.png')
        
        ! Test file exists
        call assert_file_exists('output/test/test_3d_png/test_scatter_3d.png')
    end subroutine
    
    subroutine test_projection_transformation()
        type(figure_t) :: fig
        real(wp) :: x3d(3), y3d(3), z3d(3)
        real(wp) :: x2d(3), y2d(3)
        real(wp) :: azim, elev, dist
        real(wp), parameter :: PI = 3.14159265359
        
        ! Test projection with known values
        x3d = [1.0, 0.0, 0.0]
        y3d = [0.0, 1.0, 0.0]
        z3d = [0.0, 0.0, 1.0]
        
        ! Default matplotlib values
        azim = -60.0 * PI / 180.0  ! Convert to radians
        elev = 30.0 * PI / 180.0
        dist = 10.0
        
        ! The projection should place these points correctly
        ! We'll verify this works when implementation is complete
        call fig%initialize(width=800, height=600)
        call fig%add_3d_plot(x3d, y3d, z3d)
        call fig%savefig('output/test/test_3d_png/test_projection_3d.png')
        
        call assert_file_exists('output/test/test_3d_png/test_projection_3d.png')
    end subroutine

end program test_3d_png