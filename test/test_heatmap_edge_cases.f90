program test_heatmap_edge_cases
    use fortplot, only: figure_t, wp
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf
    implicit none
    
    call test_mismatched_dimensions()
    call test_single_point_data()
    call test_extreme_values()
    call test_nan_and_inf_handling()
    call test_zero_size_figure()
    call test_irregular_grids()
    call test_dimension_mismatch_pcolormesh()
    
    print *, "All heatmap edge case tests passed!"
    
contains

    subroutine test_mismatched_dimensions()
        type(figure_t) :: fig
        real(wp) :: x(5), y(3), z_wrong(4, 3), z_correct(5, 3)
        integer :: i, j
        
        ! Setup coordinates
        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        
        ! Mismatched z dimensions (should handle gracefully)
        do i = 1, 4
            do j = 1, 3
                z_wrong(i, j) = real(i + j, wp)
            end do
        end do
        
        call fig%initialize(40, 20)
        call fig%add_contour_filled(x, y, z_wrong)  ! Should not crash
        call fig%savefig(get_test_output_path('/tmp/test_mismatch_dims.txt'))
        
        ! Correct dimensions
        do i = 1, 5
            do j = 1, 3
                z_correct(i, j) = real(i + j, wp)
            end do
        end do
        
        call fig%initialize(40, 20)
        call fig%add_contour_filled(x, y, z_correct)
        call fig%savefig(get_test_output_path('/tmp/test_correct_dims.txt'))
        
        print *, "test_mismatched_dimensions: PASSED"
    end subroutine
    
    subroutine test_single_point_data()
        type(figure_t) :: fig
        real(wp) :: x(1), y(1), z(1, 1)
        
        ! Single point data
        x = [0.0_wp]
        y = [0.0_wp]
        z(1, 1) = 1.0_wp
        
        call fig%initialize(30, 15)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig(get_test_output_path('/tmp/test_single_point.txt'))
        
        ! For pcolormesh, need 2x2 vertices for single cell
        block
            real(wp) :: x_mesh(2), y_mesh(2), z_mesh(1, 1)
            
            x_mesh = [0.0_wp, 1.0_wp]
            y_mesh = [0.0_wp, 1.0_wp]
            z_mesh(1, 1) = 0.5_wp
            
            call fig%initialize(30, 15)
            call fig%add_pcolormesh(x_mesh, y_mesh, z_mesh)
            call fig%savefig(get_test_output_path('/tmp/test_single_cell.txt'))
        end block
        
        print *, "test_single_point_data: PASSED"
    end subroutine
    
    subroutine test_extreme_values()
        type(figure_t) :: fig
        real(wp) :: x(4), y(4), z(4, 4)
        integer :: i, j
        
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        
        ! Very large values
        do i = 1, 4
            do j = 1, 4
                z(i, j) = 1.0e10_wp * real(i + j, wp)
            end do
        end do
        
        call fig%initialize(40, 20)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig(get_test_output_path('/tmp/test_large_values.txt'))
        
        ! Very small values
        do i = 1, 4
            do j = 1, 4
                z(i, j) = 1.0e-10_wp * real(i + j, wp)
            end do
        end do
        
        call fig%initialize(40, 20)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig(get_test_output_path('/tmp/test_small_values.txt'))
        
        ! Mix of positive and negative
        z = reshape([1.0e6_wp, -1.0e6_wp, 1.0e-6_wp, -1.0e-6_wp, &
                     -1.0e6_wp, 1.0e6_wp, -1.0e-6_wp, 1.0e-6_wp, &
                     1.0e-6_wp, -1.0e-6_wp, 1.0e6_wp, -1.0e6_wp, &
                     -1.0e-6_wp, 1.0e-6_wp, -1.0e6_wp, 1.0e6_wp], [4, 4])
        
        call fig%initialize(40, 20)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig(get_test_output_path('/tmp/test_mixed_extreme.txt'))
        
        print *, "test_extreme_values: PASSED"
    end subroutine
    
    subroutine test_nan_and_inf_handling()
        type(figure_t) :: fig
        real(wp) :: x(3), y(3), z(3, 3)
        real(wp) :: nan_val, inf_val
        
        x = [0.0_wp, 1.0_wp, 2.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp]
        
        ! Get NaN and Inf values
        nan_val = ieee_value(nan_val, ieee_quiet_nan)
        inf_val = ieee_value(inf_val, ieee_positive_inf)
        
        ! Test with NaN in data
        z = 1.0_wp
        z(2, 2) = nan_val
        
        call fig%initialize(30, 15)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig(get_test_output_path('/tmp/test_with_nan.txt'))
        
        ! Test with Inf in data
        z = 1.0_wp
        z(2, 2) = inf_val
        
        call fig%initialize(30, 15)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig(get_test_output_path('/tmp/test_with_inf.txt'))
        
        ! Test with mix of NaN, Inf, and normal values
        z = reshape([1.0_wp, nan_val, 3.0_wp, &
                     inf_val, 5.0_wp, -inf_val, &
                     7.0_wp, 8.0_wp, nan_val], [3, 3])
        
        call fig%initialize(30, 15)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig(get_test_output_path('/tmp/test_mixed_special.txt'))
        
        print *, "test_nan_and_inf_handling: PASSED"
    end subroutine
    
    subroutine test_zero_size_figure()
        type(figure_t) :: fig
        real(wp) :: x(3), y(3), z(3, 3)
        
        x = [0.0_wp, 1.0_wp, 2.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp]
        z = reshape([1.0_wp, 2.0_wp, 3.0_wp, &
                     4.0_wp, 5.0_wp, 6.0_wp, &
                     7.0_wp, 8.0_wp, 9.0_wp], [3, 3])
        
        ! Test with minimum reasonable size
        call fig%initialize(10, 5)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig(get_test_output_path('/tmp/test_tiny_figure.txt'))
        
        ! Test with very wide aspect ratio
        call fig%initialize(100, 10)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig(get_test_output_path('/tmp/test_wide_figure.txt'))
        
        ! Test with very tall aspect ratio
        call fig%initialize(20, 50)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig(get_test_output_path('/tmp/test_tall_figure.txt'))
        
        print *, "test_zero_size_figure: PASSED"
    end subroutine
    
    subroutine test_irregular_grids()
        type(figure_t) :: fig
        real(wp) :: x_irreg(5), y_irreg(4), z(5, 4)
        integer :: i, j
        
        ! Highly irregular spacing
        x_irreg = [0.0_wp, 0.01_wp, 0.5_wp, 5.0_wp, 100.0_wp]
        y_irreg = [0.0_wp, 1.0_wp, 1.1_wp, 10.0_wp]
        
        do i = 1, 5
            do j = 1, 4
                z(i, j) = sin(x_irreg(i)) + cos(y_irreg(j))
            end do
        end do
        
        call fig%initialize(60, 30)
        call fig%add_contour_filled(x_irreg, y_irreg, z)
        call fig%savefig(get_test_output_path('/tmp/test_irregular_grid.txt'))
        
        ! Test with reversed coordinates
        call fig%initialize(60, 30)
        call fig%add_contour_filled(x_irreg(5:1:-1), y_irreg(4:1:-1), z(5:1:-1, 4:1:-1))
        call fig%savefig(get_test_output_path('/tmp/test_reversed_coords.txt'))
        
        print *, "test_irregular_grids: PASSED"
    end subroutine
    
    subroutine test_dimension_mismatch_pcolormesh()
        type(figure_t) :: fig
        real(wp) :: x(3), y(4), z(3, 2)
        
        ! For pcolormesh: x(nx+1) × y(ny+1) vertices, z(nx, ny) cells
        ! This has correct dimensions
        x = [0.0_wp, 1.0_wp, 2.0_wp]                    ! 3 vertices
        y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]           ! 4 vertices
        z = reshape([1.0_wp, 2.0_wp, 3.0_wp, &
                     4.0_wp, 5.0_wp, 6.0_wp], [3, 2])  ! 3×2 cells
        
        call fig%initialize(40, 20)
        call fig%add_pcolormesh(x, y, z)
        call fig%savefig(get_test_output_path('/tmp/test_pcolormesh_correct.txt'))
        
        ! Test with different z dimensions (should handle gracefully)
        block
            real(wp) :: x_alt(4), y_alt(3), z_alt(2, 3)
            x_alt = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
            y_alt = [0.0_wp, 1.0_wp, 2.0_wp]
            z_alt = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp], [2, 3])
            
            call fig%initialize(40, 20)
            call fig%add_pcolormesh(x_alt, y_alt, z_alt)  ! Should not crash
            call fig%savefig(get_test_output_path('/tmp/test_pcolormesh_alt.txt'))
        end block
        
        print *, "test_dimension_mismatch_pcolormesh: PASSED"
    end subroutine

end program test_heatmap_edge_cases