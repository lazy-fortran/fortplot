program test_pcolormesh_extended
    use fortplot, only: figure_t, wp
    implicit none
    
    call test_pcolormesh_data_validation()
    call test_pcolormesh_coordinate_mapping()
    call test_pcolormesh_interpolation()
    call test_pcolormesh_backends()
    call test_pcolormesh_with_other_plots()
    call test_pcolormesh_performance_patterns()
    
    print *, "All extended pcolormesh tests passed!"
    
contains

    subroutine test_pcolormesh_data_validation()
        type(figure_t) :: fig
        real(wp) :: x(5), y(4), z(4, 3)
        integer :: i, j
        
        ! Correct dimensions: x(nx+1), y(ny+1), z(nx, ny)
        do i = 1, 5
            x(i) = real(i-1, wp) * 0.25_wp
        end do
        do i = 1, 4
            y(i) = real(i-1, wp) * 0.33_wp
        end do
        
        ! Fill z with test pattern
        do i = 1, 4
            do j = 1, 3
                z(i, j) = real(i, wp) * real(j, wp)
            end do
        end do
        
        call fig%initialize(60, 30)
        call fig%add_pcolormesh(x, y, z)
        call fig%savefig('test_pcolormesh_validation.txt')
        
        ! Test with different data patterns
        ! Diagonal gradient
        do i = 1, 4
            do j = 1, 3
                z(i, j) = real(i + j, wp) / 7.0_wp
            end do
        end do
        
        call fig%initialize(60, 30)
        call fig%add_pcolormesh(x, y, z)
        call fig%savefig('test_pcolormesh_diagonal.txt')
        
        print *, "test_pcolormesh_data_validation: PASSED"
    end subroutine
    
    subroutine test_pcolormesh_coordinate_mapping()
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), z(:,:)
        integer :: i, j
        
        ! Test log-spaced coordinates
        allocate(x(11), y(11), z(10, 10))
        
        ! Logarithmic spacing in x
        do i = 1, 11
            x(i) = 10.0_wp ** (real(i-1, wp) / 10.0_wp - 1.0_wp)  ! 0.1 to 1.0
        end do
        
        ! Linear spacing in y
        do i = 1, 11
            y(i) = real(i-1, wp) * 0.1_wp
        end do
        
        ! Create interesting pattern
        do i = 1, 10
            do j = 1, 10
                z(i, j) = log10(x(i)) * y(j)
            end do
        end do
        
        call fig%initialize(70, 35)
        call fig%add_pcolormesh(x, y, z)
        call fig%set_xlabel("X (log scale)")
        call fig%set_ylabel("Y (linear scale)")
        call fig%savefig('test_pcolormesh_logspace.txt')
        
        ! Test with negative coordinates
        do i = 1, 11
            x(i) = -5.0_wp + real(i-1, wp)
            y(i) = -5.0_wp + real(i-1, wp)
        end do
        
        do i = 1, 10
            do j = 1, 10
                z(i, j) = x(i) * y(j) / 25.0_wp
            end do
        end do
        
        call fig%initialize(70, 35)
        call fig%add_pcolormesh(x, y, z)
        call fig%savefig('test_pcolormesh_negative.txt')
        
        print *, "test_pcolormesh_coordinate_mapping: PASSED"
    end subroutine
    
    subroutine test_pcolormesh_interpolation()
        type(figure_t) :: fig
        real(wp) :: x_coarse(4), y_coarse(4), z_coarse(3, 3)
        real(wp) :: x_fine(21), y_fine(21), z_fine(20, 20)
        integer :: i, j
        
        ! Coarse grid
        x_coarse = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y_coarse = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        z_coarse = reshape([1.0_wp, 2.0_wp, 1.0_wp, &
                           2.0_wp, 4.0_wp, 2.0_wp, &
                           1.0_wp, 2.0_wp, 1.0_wp], [3, 3])
        
        call fig%initialize(50, 25)
        call fig%add_pcolormesh(x_coarse, y_coarse, z_coarse)
        call fig%savefig('test_pcolormesh_coarse.txt')
        
        ! Fine grid with smooth function
        do i = 1, 21
            x_fine(i) = real(i-1, wp) * 0.15_wp
            y_fine(i) = real(i-1, wp) * 0.15_wp
        end do
        
        do i = 1, 20
            do j = 1, 20
                z_fine(i, j) = exp(-(x_fine(i) - 1.5_wp)**2 - (y_fine(j) - 1.5_wp)**2)
            end do
        end do
        
        call fig%initialize(50, 25)
        call fig%add_pcolormesh(x_fine, y_fine, z_fine)
        call fig%savefig('test_pcolormesh_fine.txt')
        
        print *, "test_pcolormesh_interpolation: PASSED"
    end subroutine
    
    subroutine test_pcolormesh_backends()
        type(figure_t) :: fig
        real(wp) :: x(6), y(5), z(5, 4)
        integer :: i, j
        
        ! Create test data
        do i = 1, 6
            x(i) = real(i-1, wp) * 0.2_wp
        end do
        do i = 1, 5
            y(i) = real(i-1, wp) * 0.25_wp
        end do
        
        ! Create wave pattern
        do i = 1, 5
            do j = 1, 4
                z(i, j) = sin(x(i) * 6.28_wp) * cos(y(j) * 6.28_wp)
            end do
        end do
        
        ! Test ASCII backend
        call fig%initialize(60, 30)
        call fig%add_pcolormesh(x, y, z)
        call fig%savefig('test_pcolormesh_backend_ascii.txt')
        
        print *, "test_pcolormesh_backends: PASSED"
    end subroutine
    
    subroutine test_pcolormesh_with_other_plots()
        type(figure_t) :: fig
        real(wp) :: x_mesh(11), y_mesh(11), z_mesh(10, 10)
        real(wp) :: x_line(20), y_line(20)
        integer :: i, j
        
        ! Create mesh data
        do i = 1, 11
            x_mesh(i) = real(i-1, wp) * 0.1_wp
            y_mesh(i) = real(i-1, wp) * 0.1_wp
        end do
        
        do i = 1, 10
            do j = 1, 10
                z_mesh(i, j) = exp(-((x_mesh(i) - 0.5_wp)**2 + (y_mesh(j) - 0.5_wp)**2) * 10.0_wp)
            end do
        end do
        
        ! Create line data
        do i = 1, 20
            x_line(i) = real(i-1, wp) / 19.0_wp
            y_line(i) = 0.5_wp + 0.3_wp * sin(x_line(i) * 12.56_wp)
        end do
        
        ! Combine pcolormesh with line plot
        call fig%initialize(80, 40)
        call fig%add_pcolormesh(x_mesh, y_mesh, z_mesh)
        call fig%add_plot(x_line, y_line, label="Sine wave")
        call fig%set_title("Pcolormesh with Line Overlay")
        call fig%savefig('test_pcolormesh_combined.txt')
        
        print *, "test_pcolormesh_with_other_plots: PASSED"
    end subroutine
    
    subroutine test_pcolormesh_performance_patterns()
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), z(:,:)
        integer :: i, j, n
        
        ! Test with increasingly larger grids
        do n = 10, 30, 10
            allocate(x(n+1), y(n+1), z(n, n))
            
            do i = 1, n+1
                x(i) = real(i-1, wp) / real(n, wp)
                y(i) = real(i-1, wp) / real(n, wp)
            end do
            
            ! Create mandelbrot-like pattern
            do i = 1, n
                do j = 1, n
                    z(i, j) = mod(i*i + j*j, 10) / 10.0_wp
                end do
            end do
            
            call fig%initialize(60, 30)
            call fig%add_pcolormesh(x, y, z)
            
            write(*, '(A, I3, A)') "Testing ", n, "x", n, " grid"
            call fig%savefig('test_pcolormesh_' // char(iachar('0') + n/10) // '0x' // &
                            char(iachar('0') + n/10) // '0.txt')
            
            deallocate(x, y, z)
        end do
        
        print *, "test_pcolormesh_performance_patterns: PASSED"
    end subroutine

end program test_pcolormesh_extended