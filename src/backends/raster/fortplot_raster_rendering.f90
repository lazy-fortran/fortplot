module fortplot_raster_rendering
    !! Specialized rendering functionality for raster backend
    !! Extracted from fortplot_raster.f90 for size reduction (SRP compliance)
    use fortplot_constants, only: EPSILON_COMPARE
    use fortplot_raster_core, only: raster_image_t
    use fortplot_margins, only: plot_area_t
    use fortplot_colormap, only: colormap_value_to_color
    use fortplot_interpolation, only: interpolate_z_bilinear
    use fortplot_raster_primitives, only: color_to_byte, draw_filled_quad_raster
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_fill_heatmap, raster_fill_quad, fill_triangle, fill_horizontal_line
    public :: raster_render_legend_specialized, raster_calculate_legend_dimensions
    public :: raster_set_legend_border_width, raster_calculate_legend_position
    public :: raster_extract_rgb_data, raster_get_png_data, raster_prepare_3d_data

contains

    subroutine raster_fill_heatmap(raster, width, height, plot_area, x_min, x_max, y_min, y_max, &
                                  x_grid, y_grid, z_grid, z_min, z_max)
        !! Fill contour plot using scanline method for pixel-by-pixel rendering
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        
        integer :: nx, ny
        real(wp) :: x_min_grid, x_max_grid, y_min_grid, y_max_grid
        
        nx = size(x_grid)
        ny = size(y_grid)
        
        ! Validate input dimensions and data bounds
        if (size(z_grid, 1) /= ny .or. size(z_grid, 2) /= nx) return
        if (abs(z_max - z_min) < EPSILON_COMPARE) return
        
        ! Get data bounds
        x_min_grid = minval(x_grid)
        x_max_grid = maxval(x_grid)
        y_min_grid = minval(y_grid)
        y_max_grid = maxval(y_grid)
        
        ! Render pixels using scanline method
        call raster_render_heatmap_pixels(raster, width, height, plot_area, &
                                         x_min, x_max, y_min, y_max, &
                                         x_grid, y_grid, z_grid, &
                                         x_min_grid, x_max_grid, y_min_grid, y_max_grid, z_min, z_max)
    end subroutine raster_fill_heatmap
    
    subroutine raster_render_heatmap_pixels(raster, width, height, plot_area, &
                                           x_min, x_max, y_min, y_max, &
                                           x_grid, y_grid, z_grid, &
                                           x_min_grid, x_max_grid, y_min_grid, y_max_grid, z_min, z_max)
        !! Render heatmap pixels using pixel-by-pixel scanline approach
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: x_min_grid, x_max_grid, y_min_grid, y_max_grid, z_min, z_max
        
        integer :: px, py
        real(wp) :: world_x, world_y, z_value
        real(wp) :: color_rgb(3)
        integer(1) :: r_byte, g_byte, b_byte
        integer :: offset
        associate(dxming=>x_min_grid, dxmaxg=>x_max_grid, dyming=>y_min_grid, dymaxg=>y_max_grid); end associate
        
        ! Scanline rendering: iterate over all pixels in plot area
        do py = plot_area%bottom, plot_area%bottom + plot_area%height - 1
            do px = plot_area%left, plot_area%left + plot_area%width - 1
                
                ! Map pixel to world coordinates
                world_x = x_min + (real(px - plot_area%left, wp) / &
                         real(plot_area%width - 1, wp)) * (x_max - x_min)
                         
                world_y = y_max - (real(py - plot_area%bottom, wp) / &
                         real(plot_area%height - 1, wp)) * (y_max - y_min)
                
                ! Interpolate Z value and convert to color
                call interpolate_z_bilinear(x_grid, y_grid, z_grid, world_x, world_y, z_value)
                call colormap_value_to_color(z_value, z_min, z_max, 'viridis', color_rgb)
                
                ! Convert to bytes and set pixel
                r_byte = color_to_byte(color_rgb(1))
                g_byte = color_to_byte(color_rgb(2))
                b_byte = color_to_byte(color_rgb(3))
                
                ! Set pixel directly in image data (RGB format)
                if (px >= 1 .and. px <= width .and. py >= 1 .and. py <= height) then
                    offset = 3 * ((py - 1) * width + (px - 1)) + 1
                    if (offset >= 1 .and. offset + 2 <= size(raster%image_data)) then
                        raster%image_data(offset) = r_byte
                        raster%image_data(offset + 1) = g_byte
                        raster%image_data(offset + 2) = b_byte
                    end if
                end if
            end do
        end do
    end subroutine raster_render_heatmap_pixels

    subroutine raster_fill_quad(raster, width, height, plot_area, x_min, x_max, y_min, y_max, &
                               x_quad, y_quad)
        !! Fill quadrilateral with current color
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        
        real(wp) :: px_quad(4), py_quad(4)
        integer :: i
        
        ! Transform data coordinates to pixel coordinates (same as line drawing)
        ! This ensures the quad respects plot area margins
        do i = 1, 4
            px_quad(i) = (x_quad(i) - x_min) / (x_max - x_min) * real(plot_area%width, wp) + &
                        real(plot_area%left, wp)
            py_quad(i) = real(plot_area%bottom + plot_area%height, wp) - &
                        (y_quad(i) - y_min) / (y_max - y_min) * real(plot_area%height, wp)
        end do
        
        call draw_filled_quad_raster(raster%image_data, width, height, &
                                    px_quad, py_quad, &
                                    raster%current_r, raster%current_g, raster%current_b)
    end subroutine raster_fill_quad

    subroutine fill_triangle(image_data, img_w, img_h, x1, y1, x2, y2, x3, y3, r, g, b)
        !! Fill triangle using barycentric coordinates
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x1, y1, x2, y2, x3, y3
        integer(1), intent(in) :: r, g, b
        
        integer :: x, y, x_min, x_max, y_min, y_max
        real(wp) :: denom, a, b_coord, c
        integer :: pixel_index
        
        ! Find bounding box
        x_min = max(1, int(min(min(x1, x2), x3)))
        x_max = min(img_w, int(max(max(x1, x2), x3)) + 1)
        y_min = max(1, int(min(min(y1, y2), y3)))
        y_max = min(img_h, int(max(max(y1, y2), y3)) + 1)
        
        ! Precompute denominator for barycentric coordinates
        denom = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
        
        if (abs(denom) < EPSILON_COMPARE) return  ! Degenerate triangle
        
        ! Check each pixel in bounding box
        do y = y_min, y_max
            do x = x_min, x_max
                ! Compute barycentric coordinates
                a = ((y2 - y3) * (real(x, wp) - x3) + (x3 - x2) * (real(y, wp) - y3)) / denom
                b_coord = ((y3 - y1) * (real(x, wp) - x3) + (x1 - x3) * (real(y, wp) - y3)) / denom
                c = 1.0_wp - a - b_coord
                
                ! Check if point is inside triangle
                if (a >= 0.0_wp .and. b_coord >= 0.0_wp .and. c >= 0.0_wp) then
                    pixel_index = 3 * ((y - 1) * img_w + (x - 1)) + 1
                    image_data(pixel_index) = r      ! Red
                    image_data(pixel_index + 1) = g  ! Green
                    image_data(pixel_index + 2) = b  ! Blue
                end if
            end do
        end do
    end subroutine fill_triangle

    subroutine fill_horizontal_line(image_data, img_w, img_h, x1, x2, y, r, g, b)
        !! Fill horizontal line segment
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h, x1, x2, y
        integer(1), intent(in) :: r, g, b
        
        integer :: x, x_start, x_end, pixel_index
        
        x_start = max(1, min(x1, x2))
        x_end = min(img_w, max(x1, x2))
        
        if (y >= 1 .and. y <= img_h) then
            do x = x_start, x_end
                pixel_index = 3 * ((y - 1) * img_w + (x - 1)) + 1
                image_data(pixel_index) = r      ! Red
                image_data(pixel_index + 1) = g  ! Green  
                image_data(pixel_index + 2) = b  ! Blue
            end do
        end if
    end subroutine fill_horizontal_line

    subroutine raster_render_legend_specialized(legend, legend_x, legend_y)
        !! Render legend using standard algorithm for PNG
        use fortplot_legend, only: legend_t
        type(legend_t), intent(in) :: legend
        real(wp), intent(in) :: legend_x, legend_y
        
        associate(dlg=>legend%num_entries, dx=>legend_x, dy=>legend_y); end associate
        ! No-op: legend rendering handled by fortplot_legend module
        ! This method exists only for polymorphic compatibility
    end subroutine raster_render_legend_specialized

    subroutine raster_calculate_legend_dimensions(legend, legend_width, legend_height)
        !! Calculate standard legend dimensions for PNG
        use fortplot_legend, only: legend_t
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: legend_width, legend_height
        
        ! Use standard dimension calculation for PNG backend
        legend_width = 80.0_wp   ! Standard legend width
        legend_height = real(legend%num_entries * 20 + 10, wp)  ! 20 pixels per entry + margins
    end subroutine raster_calculate_legend_dimensions

    subroutine raster_set_legend_border_width()
        !! Set thin border width for PNG legend
        ! No-op: border width handled by context
        ! This method exists only for polymorphic compatibility
    end subroutine raster_set_legend_border_width

    subroutine raster_calculate_legend_position(legend, x, y)
        !! Calculate standard legend position for PNG using plot coordinates
        use fortplot_legend, only: legend_t
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: x, y
        associate(dn=>legend%num_entries); end associate
        
        ! No-op: position calculation handled by fortplot_legend module
        ! This method exists only for polymorphic compatibility
        x = 0.0_wp
        y = 0.0_wp
    end subroutine raster_calculate_legend_position

    subroutine raster_extract_rgb_data(raster, width, height, rgb_data)
        !! Extract RGB data from PNG backend
        use, intrinsic :: iso_fortran_env, only: real64
        type(raster_image_t), intent(in) :: raster
        integer, intent(in) :: width, height
        real(real64), intent(out) :: rgb_data(width, height, 3)
        integer :: x, y, idx_base
        
        do y = 1, height
            do x = 1, width
                ! Calculate 1D index for packed RGB data (width * height * 3 array)
                ! Format: [R1, G1, B1, R2, G2, B2, ...]
                idx_base = ((y-1) * width + (x-1)) * 3
                
                ! Extract RGB values (normalized to 0-1)
                rgb_data(x, y, 1) = real(raster%image_data(idx_base + 1), real64) / 255.0_real64
                rgb_data(x, y, 2) = real(raster%image_data(idx_base + 2), real64) / 255.0_real64
                rgb_data(x, y, 3) = real(raster%image_data(idx_base + 3), real64) / 255.0_real64
            end do
        end do
    end subroutine raster_extract_rgb_data

    subroutine raster_get_png_data(width, height, png_data, status)
        !! Raster context doesn't generate PNG data - only PNG context does
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        associate(dw=>width, dh=>height); end associate
        
        ! Raster context doesn't generate PNG data
        ! This should be overridden by PNG context
        allocate(png_data(0))
        status = -1
    end subroutine raster_get_png_data

    subroutine raster_prepare_3d_data(plots)
        !! Prepare 3D data for PNG backend (no-op - PNG doesn't use 3D data)
        use fortplot_plot_data, only: plot_data_t
        type(plot_data_t), intent(in) :: plots(:)
        associate(dn=>size(plots)); end associate
        
        ! PNG backend doesn't need 3D data preparation - no-op
    end subroutine raster_prepare_3d_data

end module fortplot_raster_rendering
