module fortplot_annotation_coordinates
    !! Coordinate transformation functions for text annotations
    !! 
    !! Provides:
    !! - Multiple coordinate system transformations
    !! - Data, figure, and axis coordinate support
    !! - Logarithmic scale transformation support
    !! - Visibility checking in coordinate space
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_annotation_types, only: text_annotation_t, COORD_DATA, COORD_FIGURE, COORD_AXIS
    implicit none
    
    private
    
    ! Public interface
    public :: transform_annotation_coordinates, transform_annotation_coordinates_log
    public :: is_annotation_visible
    
    ! Overloaded coordinate transformation interface
    interface transform_annotation_coordinates
        module procedure transform_annotation_coordinates_4arg
        module procedure transform_annotation_coordinates_5arg
    end interface transform_annotation_coordinates

contains

    subroutine transform_annotation_coordinates_4arg(annotation, area_or_size, pixel_x, pixel_y)
        !! 4-argument coordinate transformation (figure or axis coordinates)
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: area_or_size(:)
        real(wp), intent(out) :: pixel_x, pixel_y
        
        select case (annotation%coord_type)
        case (COORD_FIGURE)
            call transform_figure_coordinates(annotation, area_or_size, pixel_x, pixel_y)
        case (COORD_AXIS)
            call transform_axis_coordinates(annotation, area_or_size, pixel_x, pixel_y)
        case default
            ! Default to axis coordinates for 4-argument calls
            call transform_axis_coordinates(annotation, area_or_size, pixel_x, pixel_y)
        end select
    end subroutine transform_annotation_coordinates_4arg

    subroutine transform_annotation_coordinates_5arg(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        !! 5-argument coordinate transformation (data coordinates)
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: plot_area(:)
        real(wp), intent(in) :: data_bounds(:)
        real(wp), intent(out) :: pixel_x, pixel_y
        
        call transform_data_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
    end subroutine transform_annotation_coordinates_5arg

    subroutine transform_data_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        !! Transform data coordinates to pixel coordinates
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: plot_area(4)  ! x, y, width, height
        real(wp), intent(in) :: data_bounds(4)  ! xmin, xmax, ymin, ymax
        real(wp), intent(out) :: pixel_x, pixel_y
        
        real(wp) :: x_range, y_range, x_norm, y_norm
        
        ! Calculate normalized coordinates (0-1)
        x_range = data_bounds(2) - data_bounds(1)
        y_range = data_bounds(4) - data_bounds(3)
        
        if (x_range > 0.0_wp) then
            x_norm = (annotation%x - data_bounds(1)) / x_range
        else
            x_norm = 0.5_wp  ! Center if no range
        end if
        
        if (y_range > 0.0_wp) then
            y_norm = (annotation%y - data_bounds(3)) / y_range
        else
            y_norm = 0.5_wp  ! Center if no range
        end if
        
        ! Map to pixel coordinates
        pixel_x = plot_area(1) + x_norm * plot_area(3)
        pixel_y = plot_area(2) + y_norm * plot_area(4)
    end subroutine transform_data_coordinates

    subroutine transform_figure_coordinates(annotation, figure_size, pixel_x, pixel_y)
        !! Transform figure coordinates (0-1 normalized) to pixel coordinates
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: figure_size(2)  ! width, height
        real(wp), intent(out) :: pixel_x, pixel_y
        
        pixel_x = annotation%x * figure_size(1)
        pixel_y = annotation%y * figure_size(2)
    end subroutine transform_figure_coordinates

    subroutine transform_axis_coordinates(annotation, plot_area, pixel_x, pixel_y)
        !! Transform axis coordinates (0-1 normalized to plot area) to pixel coordinates
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: plot_area(4)  ! x, y, width, height
        real(wp), intent(out) :: pixel_x, pixel_y
        
        pixel_x = plot_area(1) + annotation%x * plot_area(3)
        pixel_y = plot_area(2) + annotation%y * plot_area(4)
    end subroutine transform_axis_coordinates

    subroutine transform_annotation_coordinates_log(annotation, plot_area, data_bounds, &
                                                   log_scale_x, log_scale_y, pixel_x, pixel_y)
        !! Transform annotation coordinates with logarithmic scaling support
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: plot_area(4)
        real(wp), intent(in) :: data_bounds(4)
        logical, intent(in) :: log_scale_x, log_scale_y
        real(wp), intent(out) :: pixel_x, pixel_y
        
        real(wp) :: x_norm, y_norm
        real(wp) :: log_xmin, log_xmax, log_ymin, log_ymax
        
        if (annotation%coord_type /= COORD_DATA) then
            ! For non-data coordinates, use regular transformation
            call transform_annotation_coordinates(annotation, plot_area, &
                                                data_bounds, pixel_x, pixel_y)
            return
        end if
        
        ! Handle logarithmic X transformation
        if (log_scale_x) then
            log_xmin = log10(max(data_bounds(1), 1.0e-10_wp))
            log_xmax = log10(max(data_bounds(2), 1.0e-10_wp))
            x_norm = (log10(max(annotation%x, 1.0e-10_wp)) - log_xmin) / (log_xmax - log_xmin)
        else
            x_norm = (annotation%x - data_bounds(1)) / (data_bounds(2) - data_bounds(1))
        end if
        
        ! Handle logarithmic Y transformation
        if (log_scale_y) then
            log_ymin = log10(max(data_bounds(3), 1.0e-10_wp))
            log_ymax = log10(max(data_bounds(4), 1.0e-10_wp))
            y_norm = (log10(max(annotation%y, 1.0e-10_wp)) - log_ymin) / (log_ymax - log_ymin)
        else
            y_norm = (annotation%y - data_bounds(3)) / (data_bounds(4) - data_bounds(3))
        end if
        
        ! Map to pixel coordinates
        pixel_x = plot_area(1) + x_norm * plot_area(3)
        pixel_y = plot_area(2) + y_norm * plot_area(4)
    end subroutine transform_annotation_coordinates_log

    function is_annotation_visible(annotation, plot_area) result(visible)
        !! Check if annotation is visible within plot area
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: plot_area(4)
        logical :: visible
        
        real(wp) :: pixel_x, pixel_y
        real(wp) :: data_bounds(4) = [0.0_wp, 10.0_wp, 0.0_wp, 10.0_wp]  ! Default bounds
        real(wp) :: figure_size(2) = [800.0_wp, 600.0_wp]  ! Default figure size
        
        ! For simplicity, check if center point is within reasonable bounds
        ! More sophisticated implementation would check full text bounds
        
        ! Transform coordinates to pixel space for all coordinate types
        if (annotation%coord_type == COORD_AXIS) then
            call transform_axis_coordinates(annotation, plot_area, pixel_x, pixel_y)
        else if (annotation%coord_type == COORD_FIGURE) then
            call transform_figure_coordinates(annotation, figure_size, pixel_x, pixel_y)
        else if (annotation%coord_type == COORD_DATA) then
            call transform_data_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        else
            ! Unknown coordinate type, assume not visible
            visible = .false.
            return
        end if
        
        ! Check if pixel coordinates are within extended plot area (with 50px margin)
        visible = (pixel_x >= plot_area(1) - 50.0_wp .and. &
                  pixel_x <= plot_area(1) + plot_area(3) + 50.0_wp .and. &
                  pixel_y >= plot_area(2) - 50.0_wp .and. &
                  pixel_y <= plot_area(2) + plot_area(4) + 50.0_wp)
    end function is_annotation_visible

end module fortplot_annotation_coordinates