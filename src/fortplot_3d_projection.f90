module fortplot_3d_projection
    !! 3D coordinate transformation and projection algorithms
    !! 
    !! This module contains mathematics for 3D coordinate transformations
    !! following the Single Responsibility Principle.
    !! 
    !! SOLID: Single responsibility for 3D coordinate transformations
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    implicit none

    private
    public :: normalize_3d_data_for_projection, normalize_coordinate_value

contains

    subroutine normalize_3d_data_for_projection(plot_data, x_min, x_max, y_min, y_max, &
                                              z_min, z_max, x_norm, y_norm, z_norm)
        !! Normalize 3D data to unit cube for consistent projection
        !! Following SOLID principles - focused on data normalization only
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        real(wp), intent(out) :: x_norm(:), y_norm(:), z_norm(:)
        
        integer :: i, n
        
        n = size(plot_data%x)
        
        do i = 1, n
            call normalize_coordinate_value(plot_data%x(i), x_min, x_max, x_norm(i))
            call normalize_coordinate_value(plot_data%y(i), y_min, y_max, y_norm(i))
            call normalize_coordinate_value(plot_data%z(i), z_min, z_max, z_norm(i))
        end do
        
    end subroutine normalize_3d_data_for_projection

    subroutine normalize_coordinate_value(value, min_val, max_val, normalized_value)
        !! Normalize single coordinate value to [0,1] range
        !! Following KISS principle - simple normalization formula
        real(wp), intent(in) :: value, min_val, max_val
        real(wp), intent(out) :: normalized_value
        
        real(wp) :: range
        
        range = max_val - min_val
        if (abs(range) < epsilon(1.0_wp)) then
            normalized_value = 0.5_wp  ! Center value for degenerate range
        else
            normalized_value = (value - min_val) / range
        end if
        
    end subroutine normalize_coordinate_value

end module fortplot_3d_projection