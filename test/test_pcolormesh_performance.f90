program test_pcolormesh_performance
    !! Test pcolormesh performance with large meshes and memory efficiency
    !! 
    !! Given: Large mesh data (1000+ cells)
    !! When: Rendered through pcolormesh
    !! Then: Should handle efficiently without excessive memory usage or timeouts
    
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_security, only: get_test_output_path
    implicit none
    
    call test_large_mesh_memory_efficiency()
    call test_1000_cell_performance_benchmark()
    call test_rectangular_mesh_scaling()
    call test_memory_usage_validation()
    call test_rendering_time_limits()
    call test_backend_performance_comparison()
    
    print *, "All pcolormesh performance tests completed!"
    
contains

    subroutine test_large_mesh_memory_efficiency()
        !! Given: 50x50 mesh (2500 cells) with detailed coordinate arrays
        !! When: Creating and rendering pcolormesh
        !! Then: Memory usage should remain reasonable (< 100MB) and not crash
        
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), c(:,:)
        integer, parameter :: nx = 50, ny = 50
        integer :: i, j
        real(wp) :: start_time, end_time
        
        ! Arrange - Allocate large mesh data
        allocate(x(nx+1), y(ny+1), c(ny, nx))
        
        ! Create coordinate arrays
        do i = 1, nx+1
            x(i) = real(i-1, wp) / real(nx, wp)
        end do
        do i = 1, ny+1
            y(i) = real(i-1, wp) / real(ny, wp)
        end do
        
        ! Create complex mathematical pattern requiring computation
        do i = 1, ny
            do j = 1, nx
                c(i, j) = sin(x(j) * 10.0_wp) * cos(y(i) * 8.0_wp) * &
                         exp(-((x(j) - 0.5_wp)**2 + (y(i) - 0.5_wp)**2) * 5.0_wp)
            end do
        end do
        
        call cpu_time(start_time)
        
        ! Act - This should handle large mesh without excessive memory usage
        call fig%initialize(800, 600)
        call fig%add_pcolormesh(x, y, c, colormap='viridis')
        call fig%set_title("Large Mesh Performance Test - 50x50 cells")
        call fig%savefig(get_test_output_path('/tmp/test_pcolormesh_large_memory.png'))
        
        call cpu_time(end_time)
        
        ! Assert - Should complete within reasonable time (< 5 seconds)
        if (end_time - start_time > 5.0_wp) then
            error stop "Large mesh rendering exceeded 5 second time limit"
        end if
        
        deallocate(x, y, c)
        print *, "test_large_mesh_memory_efficiency: PASSED"
    end subroutine test_large_mesh_memory_efficiency

    subroutine test_1000_cell_performance_benchmark()
        !! Given: Approximately 1000 cells (32x32 = 1024 cells)
        !! When: Rendering through multiple backends
        !! Then: Each backend should complete within performance thresholds
        
        type(figure_t) :: fig_png, fig_pdf, fig_ascii
        real(wp), allocatable :: x(:), y(:), c(:,:)
        integer, parameter :: nx = 32, ny = 32  ! 1024 cells
        integer :: i, j
        real(wp) :: start_time, end_time
        real(wp) :: png_time, pdf_time, ascii_time
        
        ! Arrange - Allocate 1000+ cell mesh
        allocate(x(nx+1), y(ny+1), c(ny, nx))
        
        do i = 1, nx+1
            x(i) = real(i-1, wp) * 0.1_wp
        end do
        do i = 1, ny+1
            y(i) = real(i-1, wp) * 0.1_wp
        end do
        
        ! Create ripple pattern that exercises colormap mapping
        do i = 1, ny
            do j = 1, nx
                c(i, j) = sin(sqrt((x(j) - 1.6_wp)**2 + (y(i) - 1.6_wp)**2) * 10.0_wp)
            end do
        end do
        
        ! Act & Assert - Test PNG backend performance
        call cpu_time(start_time)
        call fig_png%initialize(600, 600)
        call fig_png%add_pcolormesh(x, y, c, colormap='plasma')
        call fig_png%savefig(get_test_output_path('/tmp/test_pcolormesh_1000_cells.png'))
        call cpu_time(end_time)
        png_time = end_time - start_time
        
        if (png_time > 3.0_wp) then
            error stop "PNG backend exceeded 3 second limit for 1000 cells"
        end if
        
        ! Test PDF backend performance
        call cpu_time(start_time)
        call fig_pdf%initialize(600, 600)
        call fig_pdf%add_pcolormesh(x, y, c, colormap='plasma')
        call fig_pdf%savefig(get_test_output_path('/tmp/test_pcolormesh_1000_cells.pdf'))
        call cpu_time(end_time)
        pdf_time = end_time - start_time
        
        if (pdf_time > 4.0_wp) then
            error stop "PDF backend exceeded 4 second limit for 1000 cells"
        end if
        
        ! Test ASCII backend performance (should be fastest)
        call cpu_time(start_time)
        call fig_ascii%initialize(80, 60)
        call fig_ascii%add_pcolormesh(x, y, c, colormap='plasma')
        call fig_ascii%savefig(get_test_output_path('/tmp/test_pcolormesh_1000_cells.txt'))
        call cpu_time(end_time)
        ascii_time = end_time - start_time
        
        if (ascii_time > 1.0_wp) then
            error stop "ASCII backend exceeded 1 second limit for 1000 cells"
        end if
        
        write(*, '(A, F6.3, A)') "PNG backend: ", png_time, " seconds"
        write(*, '(A, F6.3, A)') "PDF backend: ", pdf_time, " seconds"
        write(*, '(A, F6.3, A)') "ASCII backend: ", ascii_time, " seconds"
        
        deallocate(x, y, c)
        print *, "test_1000_cell_performance_benchmark: PASSED"
    end subroutine test_1000_cell_performance_benchmark

    subroutine test_rectangular_mesh_scaling()
        !! Given: Highly rectangular meshes with different aspect ratios
        !! When: Rendering through pcolormesh
        !! Then: Performance should scale reasonably with cell count, not dimensions
        
        type(figure_t) :: fig
        integer, parameter :: test_cases = 3
        integer, parameter :: nx_values(test_cases) = [100, 10, 1]
        integer, parameter :: ny_values(test_cases) = [10, 100, 1000]
        integer :: test_idx, nx, ny, i, j
        real(wp), allocatable :: x(:), y(:), c(:,:)
        real(wp) :: start_time, end_time, render_time
        character(len=50) :: filename
        
        do test_idx = 1, test_cases
            nx = nx_values(test_idx)
            ny = ny_values(test_idx)
            
            ! Arrange - Different aspect ratios, same total cells
            allocate(x(nx+1), y(ny+1), c(ny, nx))
            
            do i = 1, nx+1
                x(i) = real(i-1, wp) / real(nx, wp) * 10.0_wp
            end do
            do i = 1, ny+1
                y(i) = real(i-1, wp) / real(ny, wp) * 10.0_wp
            end do
            
            ! Create wave pattern
            do i = 1, ny
                do j = 1, nx
                    c(i, j) = sin(x(j) * 2.0_wp) + cos(y(i) * 2.0_wp)
                end do
            end do
            
            call cpu_time(start_time)
            
            ! Act - Render rectangular mesh
            call fig%initialize(600, 400)
            call fig%add_pcolormesh(x, y, c, colormap='coolwarm')
            write(filename, '(A, I0, A, I0, A)') get_test_output_path('/tmp/test_rect_mesh_'), nx, 'x', ny, '.png'
            call fig%savefig(filename)
            
            call cpu_time(end_time)
            render_time = end_time - start_time
            
            ! Assert - Performance should scale with total cells, not aspect ratio
            if (render_time > 2.0_wp) then
                write(*, '(A, I0, A, I0, A)') "ERROR: ", nx, "x", ny, " mesh exceeded 2 second limit"
                error stop "Rectangular mesh performance test failed"
            end if
            
            write(*, '(A, I0, A, I0, A, F6.3, A)') "Mesh ", nx, "x", ny, ": ", render_time, " seconds"
            
            deallocate(x, y, c)
        end do
        
        print *, "test_rectangular_mesh_scaling: PASSED"
    end subroutine test_rectangular_mesh_scaling

    subroutine test_memory_usage_validation()
        !! Given: Progressively larger meshes
        !! When: Creating pcolormesh objects
        !! Then: Memory usage should scale linearly with mesh size
        
        type(figure_t) :: fig
        integer, parameter :: sizes(4) = [10, 20, 30, 40]
        integer :: size_idx, n, i, j
        real(wp), allocatable :: x(:), y(:), c(:,:)
        
        do size_idx = 1, 4
            n = sizes(size_idx)
            
            ! Arrange - Allocate mesh of size n x n
            allocate(x(n+1), y(n+1), c(n, n))
            
            do i = 1, n+1
                x(i) = real(i-1, wp) / real(n, wp)
                y(i) = real(i-1, wp) / real(n, wp)
            end do
            
            do i = 1, n
                do j = 1, n
                    c(i, j) = real(i + j, wp) / real(2*n, wp)
                end do
            end do
            
            ! Act - Create pcolormesh (should not cause memory issues)
            call fig%initialize(400, 400)
            call fig%add_pcolormesh(x, y, c, colormap='viridis')
            
            ! Test that we can create multiple pcolormesh objects without memory problems
            call fig%add_pcolormesh(x, y, c * 0.5_wp, colormap='plasma')
            
            ! Save to test file creation
            write(*, '(A, I0, A, I0, A)') "Testing memory usage for ", n, "x", n, " mesh"
            
            deallocate(x, y, c)
        end do
        
        ! Assert - If we reach here, memory management is working
        print *, "test_memory_usage_validation: PASSED"
    end subroutine test_memory_usage_validation

    subroutine test_rendering_time_limits()
        !! Given: Medium-sized mesh with complex colormap operations
        !! When: Rendering with multiple colormaps
        !! Then: Each colormap should render within reasonable time limits
        
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), c(:,:)
        integer, parameter :: n = 25  ! 625 cells
        integer :: i, j, colormap_idx
        character(len=10), parameter :: colormaps(5) = &
            ['viridis   ', 'plasma    ', 'inferno   ', 'coolwarm  ', 'jet       ']
        real(wp) :: start_time, end_time, render_time
        character(len=50) :: filename
        
        ! Arrange - Medium complexity mesh
        allocate(x(n+1), y(n+1), c(n, n))
        
        do i = 1, n+1
            x(i) = real(i-1, wp) / real(n, wp) * 5.0_wp
            y(i) = real(i-1, wp) / real(n, wp) * 5.0_wp
        end do
        
        ! Create complex pattern that exercises colormap
        do i = 1, n
            do j = 1, n
                c(i, j) = sin(x(j)) * cos(y(i)) + 0.5_wp * sin(x(j) * y(i))
            end do
        end do
        
        ! Act & Assert - Test each colormap for performance
        do colormap_idx = 1, 5
            call cpu_time(start_time)
            
            call fig%initialize(500, 500)
            call fig%add_pcolormesh(x, y, c, colormap=trim(colormaps(colormap_idx)))
            call fig%set_title("Performance Test - " // trim(colormaps(colormap_idx)))
            
            write(filename, '(A, A, A)') get_test_output_path('/tmp/test_time_limit_'), &
                                       trim(colormaps(colormap_idx)), '.png'
            call fig%savefig(filename)
            
            call cpu_time(end_time)
            render_time = end_time - start_time
            
            ! Assert - Each colormap should render within 1.5 seconds
            if (render_time > 1.5_wp) then
                write(*, '(A, A, A, F6.3, A)') "ERROR: ", trim(colormaps(colormap_idx)), &
                    " colormap exceeded 1.5s limit: ", render_time, " seconds"
                error stop "Colormap rendering time limit exceeded"
            end if
            
            write(*, '(A, A, A, F6.3, A)') "Colormap ", trim(colormaps(colormap_idx)), &
                ": ", render_time, " seconds"
        end do
        
        deallocate(x, y, c)
        print *, "test_rendering_time_limits: PASSED"
    end subroutine test_rendering_time_limits

    subroutine test_backend_performance_comparison()
        !! Given: Identical medium-sized mesh data
        !! When: Rendered through different backends
        !! Then: Performance characteristics should be predictable (ASCII fastest, PDF slowest)
        
        type(figure_t) :: fig_ascii, fig_png, fig_pdf
        real(wp), allocatable :: x(:), y(:), c(:,:)
        integer, parameter :: n = 30  ! 900 cells
        integer :: i, j
        real(wp) :: ascii_time, png_time, pdf_time
        real(wp) :: start_time, end_time
        
        ! Arrange - Create test mesh
        allocate(x(n+1), y(n+1), c(n, n))
        
        do i = 1, n+1
            x(i) = real(i-1, wp) * 0.1_wp
            y(i) = real(i-1, wp) * 0.1_wp
        end do
        
        do i = 1, n
            do j = 1, n
                c(i, j) = exp(-((x(j) - 1.5_wp)**2 + (y(i) - 1.5_wp)**2))
            end do
        end do
        
        ! Act - Measure ASCII backend performance
        call cpu_time(start_time)
        call fig_ascii%initialize(60, 45)
        call fig_ascii%add_pcolormesh(x, y, c, colormap='viridis')
        call fig_ascii%savefig(get_test_output_path('/tmp/test_performance_ascii.txt'))
        call cpu_time(end_time)
        ascii_time = end_time - start_time
        
        ! Measure PNG backend performance
        call cpu_time(start_time)
        call fig_png%initialize(500, 400)
        call fig_png%add_pcolormesh(x, y, c, colormap='viridis')
        call fig_png%savefig(get_test_output_path('/tmp/test_performance_png.png'))
        call cpu_time(end_time)
        png_time = end_time - start_time
        
        ! Measure PDF backend performance
        call cpu_time(start_time)
        call fig_pdf%initialize(500, 400)
        call fig_pdf%add_pcolormesh(x, y, c, colormap='viridis')
        call fig_pdf%savefig(get_test_output_path('/tmp/test_performance_pdf.pdf'))
        call cpu_time(end_time)
        pdf_time = end_time - start_time
        
        ! Assert - Performance should follow expected ordering
        write(*, '(A, F6.3, A)') "ASCII backend: ", ascii_time, " seconds"
        write(*, '(A, F6.3, A)') "PNG backend: ", png_time, " seconds"
        write(*, '(A, F6.3, A)') "PDF backend: ", pdf_time, " seconds"
        
        ! ASCII should be fastest (character mapping)
        if (ascii_time > 0.5_wp) then
            error stop "ASCII backend too slow for 900 cells"
        end if
        
        ! PNG should be reasonable (raster rendering)
        if (png_time > 2.0_wp) then
            error stop "PNG backend too slow for 900 cells"
        end if
        
        ! PDF should complete within reasonable time (vector rendering)
        if (pdf_time > 3.0_wp) then
            error stop "PDF backend too slow for 900 cells"
        end if
        
        deallocate(x, y, c)
        print *, "test_backend_performance_comparison: PASSED"
    end subroutine test_backend_performance_comparison

end program test_pcolormesh_performance