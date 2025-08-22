module fortplot_coordinates
    !! Coordinate transformation utilities (SOLID principles compliance)
    !! 
    !! This module handles coordinate transformations, projections, and
    !! coordinate system management, separated for better modularity.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_base, only: figure_t
    use fortplot_plot_data, only: plot_data_t
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    use fortplot_annotations, only: COORD_DATA, COORD_FIGURE, COORD_AXIS
    use fortplot_scales

    implicit none

    private
    public :: transform_annotation_coordinates, transform_quad_to_screen
    public :: normalize_coordinate_value, project_normalized_3d_data
    public :: setup_3d_coordinate_system, calculate_projection_bounds
    public :: save_and_set_backend_coordinates, restore_original_coordinate_system

contains

    subroutine transform_annotation_coordinates(self, x, y, coord_type, pixel_x, pixel_y)
        !! Transform annotation coordinates based on coordinate type (Issue #184)
        class(figure_t), intent(in) :: self
        real(wp), intent(in) :: x, y
        integer, intent(in) :: coord_type
        real(wp), intent(out) :: pixel_x, pixel_y
        
        select case (coord_type)
        case (COORD_DATA)
            call transform_data_to_pixel(self, x, y, pixel_x, pixel_y)
        case (COORD_FIGURE)
            call transform_figure_to_pixel(self, x, y, pixel_x, pixel_y)
        case (COORD_AXIS)
            call transform_axis_to_pixel(self, x, y, pixel_x, pixel_y)
        case default
            call transform_data_to_pixel(self, x, y, pixel_x, pixel_y)
        end select
    end subroutine transform_annotation_coordinates

    subroutine transform_data_to_pixel(self, x, y, pixel_x, pixel_y)
        !! Transform data coordinates to pixel coordinates
        class(figure_t), intent(in) :: self
        real(wp), intent(in) :: x, y
        real(wp), intent(out) :: pixel_x, pixel_y
        
        real(wp) :: plot_width, plot_height, plot_left, plot_bottom
        real(wp) :: norm_x, norm_y
        
        ! Calculate plot area dimensions
        plot_width = real(self%width, wp) * (1.0_wp - self%margin_left - self%margin_right)
        plot_height = real(self%height, wp) * (1.0_wp - self%margin_bottom - self%margin_top)
        plot_left = real(self%width, wp) * self%margin_left
        plot_bottom = real(self%height, wp) * self%margin_bottom
        
        ! Normalize data coordinates to [0,1] range
        if (self%x_max_transformed > self%x_min_transformed) then
            norm_x = (x - self%x_min_transformed) / (self%x_max_transformed - self%x_min_transformed)
        else
            norm_x = 0.5_wp
        end if
        
        if (self%y_max_transformed > self%y_min_transformed) then
            norm_y = (y - self%y_min_transformed) / (self%y_max_transformed - self%y_min_transformed)
        else
            norm_y = 0.5_wp
        end if
        
        ! Convert to pixel coordinates
        pixel_x = plot_left + norm_x * plot_width
        pixel_y = plot_bottom + norm_y * plot_height
    end subroutine transform_data_to_pixel

    subroutine transform_figure_to_pixel(self, x, y, pixel_x, pixel_y)
        !! Transform figure coordinates (0-1 range) to pixel coordinates
        class(figure_t), intent(in) :: self
        real(wp), intent(in) :: x, y
        real(wp), intent(out) :: pixel_x, pixel_y
        
        pixel_x = x * real(self%width, wp)
        pixel_y = y * real(self%height, wp)
    end subroutine transform_figure_to_pixel

    subroutine transform_axis_to_pixel(self, x, y, pixel_x, pixel_y)
        !! Transform axis coordinates (0-1 range within plot area) to pixel coordinates
        class(figure_t), intent(in) :: self
        real(wp), intent(in) :: x, y
        real(wp), intent(out) :: pixel_x, pixel_y
        
        real(wp) :: plot_width, plot_height, plot_left, plot_bottom
        
        ! Calculate plot area dimensions
        plot_width = real(self%width, wp) * (1.0_wp - self%margin_left - self%margin_right)
        plot_height = real(self%height, wp) * (1.0_wp - self%margin_bottom - self%margin_top)
        plot_left = real(self%width, wp) * self%margin_left
        plot_bottom = real(self%height, wp) * self%margin_bottom
        
        ! Convert to pixel coordinates within plot area
        pixel_x = plot_left + x * plot_width
        pixel_y = plot_bottom + y * plot_height
    end subroutine transform_axis_to_pixel

    subroutine transform_quad_to_screen(self, x_quad, y_quad, x_screen, y_screen)
        !! Transform quad coordinates to screen coordinates
        class(figure_t), intent(in) :: self
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        real(wp), intent(out) :: x_screen(4), y_screen(4)
        
        integer :: i
        
        do i = 1, 4
            call transform_data_to_pixel(self, x_quad(i), y_quad(i), x_screen(i), y_screen(i))
        end do
    end subroutine transform_quad_to_screen

    subroutine normalize_coordinate_value(value, min_val, max_val, normalized_value)
        !! Normalize a coordinate value to [0,1] range
        real(wp), intent(in) :: value, min_val, max_val
        real(wp), intent(out) :: normalized_value
        
        if (max_val > min_val) then
            normalized_value = (value - min_val) / (max_val - min_val)
        else
            normalized_value = 0.5_wp  ! Default to center if range is invalid
        end if
    end subroutine normalize_coordinate_value

    subroutine project_normalized_3d_data(x_norm, y_norm, z_norm, x2d, y2d)
        !! Project normalized 3D data to 2D using default viewing angles
        real(wp), intent(in) :: x_norm(:), y_norm(:), z_norm(:)
        real(wp), intent(out) :: x2d(:), y2d(:)
        
        real(wp) :: azim, elev, dist
        
        ! Get default viewing angles
        call get_default_view_angles(azim, elev, dist)
        
        ! Project to 2D
        call project_3d_to_2d(x_norm, y_norm, z_norm, azim, elev, dist, x2d, y2d)
    end subroutine project_normalized_3d_data

    subroutine setup_3d_coordinate_system(self, x2d, y2d, orig_x_min, orig_x_max, &
                                         orig_y_min, orig_y_max)
        !! Setup coordinate system for 3D projection rendering
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x2d(:), y2d(:)
        real(wp), intent(out) :: orig_x_min, orig_x_max, orig_y_min, orig_y_max
        
        real(wp) :: proj_x_min, proj_x_max, proj_y_min, proj_y_max
        
        ! Save original coordinate system
        call save_original_coordinates(self, orig_x_min, orig_x_max, orig_y_min, orig_y_max)
        
        ! Calculate projection bounds
        call calculate_projection_bounds(x2d, y2d, proj_x_min, proj_x_max, proj_y_min, proj_y_max)
        
        ! Set new coordinate system for projection
        call save_and_set_backend_coordinates(self, proj_x_min, proj_x_max, proj_y_min, proj_y_max, &
                                            orig_x_min, orig_x_max, orig_y_min, orig_y_max)
    end subroutine setup_3d_coordinate_system

    subroutine calculate_projection_bounds(x2d, y2d, proj_x_min, proj_x_max, &
                                         proj_y_min, proj_y_max)
        !! Calculate bounds of projected 2D data
        real(wp), intent(in) :: x2d(:), y2d(:)
        real(wp), intent(out) :: proj_x_min, proj_x_max, proj_y_min, proj_y_max
        
        proj_x_min = minval(x2d)
        proj_x_max = maxval(x2d)
        proj_y_min = minval(y2d)
        proj_y_max = maxval(y2d)
    end subroutine calculate_projection_bounds

    subroutine save_and_set_backend_coordinates(self, proj_x_min, proj_x_max, &
                                              proj_y_min, proj_y_max, &
                                              orig_x_min, orig_x_max, &
                                              orig_y_min, orig_y_max)
        !! Save original coordinates and set new backend coordinate system
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: proj_x_min, proj_x_max, proj_y_min, proj_y_max
        real(wp), intent(in) :: orig_x_min, orig_x_max, orig_y_min, orig_y_max
        
        ! Set backend to use projection coordinate system
        call self%backend%set_coordinates(proj_x_min, proj_x_max, proj_y_min, proj_y_max)
    end subroutine save_and_set_backend_coordinates

    subroutine restore_original_coordinate_system(self, orig_x_min, orig_x_max, &
                                                 orig_y_min, orig_y_max)
        !! Restore original coordinate system after 3D rendering
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: orig_x_min, orig_x_max, orig_y_min, orig_y_max
        
        ! Restore original coordinate system
        call self%backend%set_coordinates(orig_x_min, orig_x_max, orig_y_min, orig_y_max)
    end subroutine restore_original_coordinate_system

    subroutine save_original_coordinates(self, orig_x_min, orig_x_max, orig_y_min, orig_y_max)
        !! Save the current coordinate system
        class(figure_t), intent(in) :: self
        real(wp), intent(out) :: orig_x_min, orig_x_max, orig_y_min, orig_y_max
        
        orig_x_min = self%x_min_transformed
        orig_x_max = self%x_max_transformed
        orig_y_min = self%y_min_transformed
        orig_y_max = self%y_max_transformed
    end subroutine save_original_coordinates

    subroutine apply_scale_transformation(self, scale_type, values, transformed_values)
        !! Apply scale transformation (linear, log, symlog) to values
        class(figure_t), intent(in) :: self
        character(len=*), intent(in) :: scale_type
        real(wp), intent(in) :: values(:)
        real(wp), intent(out) :: transformed_values(:)
        
        select case (trim(scale_type))
        case ('linear')
            transformed_values = values
        case ('log')
            call apply_log_scale(values, transformed_values)
        case ('symlog')
            call apply_symlog_scale(values, self%symlog_threshold, transformed_values)
        case default
            transformed_values = values
        end select
    end subroutine apply_scale_transformation

    subroutine apply_log_scale(values, transformed_values)
        !! Apply logarithmic scale transformation
        real(wp), intent(in) :: values(:)
        real(wp), intent(out) :: transformed_values(:)
        
        integer :: i
        
        do i = 1, size(values)
            if (values(i) > 0.0_wp) then
                transformed_values(i) = log10(values(i))
            else
                transformed_values(i) = log10(tiny(1.0_wp))  ! Handle non-positive values
            end if
        end do
    end subroutine apply_log_scale

    subroutine apply_symlog_scale(values, threshold, transformed_values)
        !! Apply symmetric logarithmic scale transformation
        real(wp), intent(in) :: values(:), threshold
        real(wp), intent(out) :: transformed_values(:)
        
        integer :: i
        real(wp) :: abs_val, sign_val
        
        do i = 1, size(values)
            abs_val = abs(values(i))
            sign_val = sign(1.0_wp, values(i))
            
            if (abs_val <= threshold) then
                transformed_values(i) = values(i) / threshold
            else
                transformed_values(i) = sign_val * (log10(abs_val/threshold) + 1.0_wp)
            end if
        end do
    end subroutine apply_symlog_scale

    subroutine transform_axis_ranges(self)
        !! Transform axis ranges based on scale settings
        class(figure_t), intent(inout) :: self
        
        ! Transform X axis
        call transform_single_axis_range(self%x_min, self%x_max, self%xscale, &
                                        self%symlog_threshold, &
                                        self%x_min_transformed, self%x_max_transformed)
        
        ! Transform Y axis
        call transform_single_axis_range(self%y_min, self%y_max, self%yscale, &
                                        self%symlog_threshold, &
                                        self%y_min_transformed, self%y_max_transformed)
    end subroutine transform_axis_ranges

    subroutine transform_single_axis_range(min_val, max_val, scale_type, threshold, &
                                         min_transformed, max_transformed)
        !! Transform single axis range based on scale type
        real(wp), intent(in) :: min_val, max_val, threshold
        character(len=*), intent(in) :: scale_type
        real(wp), intent(out) :: min_transformed, max_transformed
        
        real(wp) :: values(2), transformed(2)
        
        values = [min_val, max_val]
        
        select case (trim(scale_type))
        case ('linear')
            transformed = values
        case ('log')
            call apply_log_scale(values, transformed)
        case ('symlog')
            call apply_symlog_scale(values, threshold, transformed)
        case default
            transformed = values
        end select
        
        min_transformed = transformed(1)
        max_transformed = transformed(2)
    end subroutine transform_single_axis_range

end module fortplot_coordinates