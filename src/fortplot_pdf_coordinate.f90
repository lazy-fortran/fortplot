module fortplot_pdf_coordinate
    !! PDF coordinate transformation and polymorphic method support
    !! Handles coordinate normalization and backend-specific method implementations
    
    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core, PDF_WIDTH, PDF_HEIGHT
    use fortplot_pdf_text, only: draw_mixed_font_text, draw_rotated_mixed_font_text
    use fortplot_pdf_drawing, only: draw_pdf_arrow, draw_pdf_circle_with_outline, &
                                   draw_pdf_square_with_outline, draw_pdf_diamond_with_outline, &
                                   draw_pdf_x_marker
    use fortplot_plot_data, only: plot_data_t
    use fortplot_legend, only: legend_entry_t
    use fortplot_margins, only: plot_area_t
    implicit none
    
    private
    
    ! Forward declaration for PDF context type
    type :: pdf_context_handle
        real(wp) :: x_min, x_max, y_min, y_max
        integer :: width, height
        type(plot_area_t) :: plot_area
        type(pdf_context_core) :: core_ctx
    end type pdf_context_handle
    
    public :: pdf_context_handle
    public :: normalize_to_pdf_coords
    public :: pdf_get_width_scale, pdf_get_height_scale
    public :: pdf_fill_quad, pdf_fill_heatmap
    public :: pdf_render_legend_specialized, pdf_calculate_legend_dimensions
    public :: pdf_set_legend_border_width, pdf_calculate_legend_position
    public :: pdf_extract_rgb_data, pdf_get_png_data
    public :: pdf_prepare_3d_data, pdf_render_ylabel
    
contains

    subroutine normalize_to_pdf_coords(ctx, x, y, pdf_x, pdf_y)
        type(pdf_context_handle), intent(in) :: ctx
        real(wp), intent(in) :: x, y
        real(wp), intent(out) :: pdf_x, pdf_y
        real(wp) :: x_range, y_range
        real(wp), parameter :: EPSILON = 1.0e-10_wp
        
        ! Calculate data ranges with epsilon protection
        x_range = ctx%x_max - ctx%x_min
        y_range = ctx%y_max - ctx%y_min
        
        ! Transform X coordinate with zero-range protection
        if (abs(x_range) < EPSILON) then
            pdf_x = real(ctx%plot_area%left, wp) + real(ctx%plot_area%width, wp) * 0.5_wp
        else
            pdf_x = (x - ctx%x_min) / x_range * real(ctx%plot_area%width, wp) + &
                    real(ctx%plot_area%left, wp)
        end if
        
        ! Transform Y coordinate with zero-range protection
        if (abs(y_range) < EPSILON) then
            pdf_y = real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height, wp) + &
                    real(ctx%plot_area%height, wp) * 0.5_wp
        else
            pdf_y = (y - ctx%y_min) / y_range * real(ctx%plot_area%height, wp) + &
                    real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height, wp)
        end if
    end subroutine normalize_to_pdf_coords

    real(wp) function pdf_get_width_scale(ctx) result(scale)
        type(pdf_context_handle), intent(in) :: ctx
        scale = real(ctx%width, wp) / PDF_WIDTH
    end function pdf_get_width_scale
    
    real(wp) function pdf_get_height_scale(ctx) result(scale)
        type(pdf_context_handle), intent(in) :: ctx
        scale = real(ctx%height, wp) / PDF_HEIGHT
    end function pdf_get_height_scale
    
    subroutine pdf_fill_quad(ctx, stream_writer, x_quad, y_quad)
        type(pdf_context_handle), intent(in) :: ctx
        type(*), intent(inout) :: stream_writer  ! Avoid circular dependency
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        real(wp) :: px(4), py(4)
        character(len=512) :: cmd
        integer :: i
        
        ! Convert to PDF coordinates
        do i = 1, 4
            call normalize_to_pdf_coords(ctx, x_quad(i), y_quad(i), px(i), py(i))
        end do
        
        ! Draw filled quadrilateral
        write(cmd, '(8(F0.3, 1X), "m l l l h f")') px(1), py(1), px(2), py(2), px(3), py(3), px(4), py(4)
        ! stream_writer%add_to_stream(trim(cmd)) - will be handled by caller
    end subroutine pdf_fill_quad
    
    subroutine pdf_fill_heatmap(ctx, stream_writer, x_grid, y_grid, z_grid, z_min, z_max)
        type(pdf_context_handle), intent(in) :: ctx
        type(*), intent(inout) :: stream_writer  ! Avoid circular dependency
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        
        integer :: i, j
        real(wp) :: x_quad(4), y_quad(4)
        real(wp) :: value, norm_value
        real(wp), dimension(3) :: color
        character(len=32) :: cmd
        
        do i = 1, size(z_grid, 1) - 1
            do j = 1, size(z_grid, 2) - 1
                ! Get normalized value
                value = z_grid(i, j)
                if (z_max > z_min) then
                    norm_value = (value - z_min) / (z_max - z_min)
                else
                    norm_value = 0.5_wp
                end if
                
                ! Simple grayscale color
                color = [norm_value, norm_value, norm_value]
                
                ! Define quad corners
                x_quad = [x_grid(i), x_grid(i+1), x_grid(i+1), x_grid(i)]
                y_quad = [y_grid(j), y_grid(j), y_grid(j+1), y_grid(j+1)]
                
                ! Fill cell - caller handles actual drawing
                call pdf_fill_quad(ctx, stream_writer, x_quad, y_quad)
            end do
        end do
    end subroutine pdf_fill_heatmap
    
    subroutine pdf_render_legend_specialized(ctx, entries, x, y, width, height)
        type(pdf_context_handle), intent(inout) :: ctx
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(in) :: x, y, width, height
        
        ! Simplified legend rendering
        integer :: i
        real(wp) :: y_pos
        
        y_pos = y
        do i = 1, size(entries)
            call draw_mixed_font_text(ctx%core_ctx, x, y_pos, entries(i)%label)
            y_pos = y_pos - 20.0_wp
        end do
    end subroutine pdf_render_legend_specialized
    
    subroutine pdf_calculate_legend_dimensions(ctx, entries, width, height)
        type(pdf_context_handle), intent(in) :: ctx
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(out) :: width, height
        
        width = 100.0_wp
        height = real(size(entries), wp) * 20.0_wp
    end subroutine pdf_calculate_legend_dimensions
    
    subroutine pdf_set_legend_border_width(ctx, width)
        type(pdf_context_handle), intent(inout) :: ctx
        real(wp), intent(in) :: width
        
        call ctx%core_ctx%set_line_width(width)
    end subroutine pdf_set_legend_border_width
    
    subroutine pdf_calculate_legend_position(ctx, loc, x, y)
        type(pdf_context_handle), intent(in) :: ctx
        character(len=*), intent(in) :: loc
        real(wp), intent(out) :: x, y
        
        select case(trim(loc))
        case('upper right')
            x = real(ctx%plot_area%left + ctx%plot_area%width - 100, wp)
            y = real(ctx%plot_area%bottom + ctx%plot_area%height - 20, wp)
        case('upper left')
            x = real(ctx%plot_area%left + 20, wp)
            y = real(ctx%plot_area%bottom + ctx%plot_area%height - 20, wp)
        case('lower right')
            x = real(ctx%plot_area%left + ctx%plot_area%width - 100, wp)
            y = real(ctx%plot_area%bottom + 100, wp)
        case('lower left')
            x = real(ctx%plot_area%left + 20, wp)
            y = real(ctx%plot_area%bottom + 100, wp)
        case default
            x = real(ctx%plot_area%left + ctx%plot_area%width - 100, wp)
            y = real(ctx%plot_area%bottom + ctx%plot_area%height - 20, wp)
        end select
    end subroutine pdf_calculate_legend_position
    
    subroutine pdf_extract_rgb_data(ctx, width, height, rgb_data)
        type(pdf_context_handle), intent(in) :: ctx
        integer, intent(in) :: width, height
        real(wp), intent(out) :: rgb_data(width, height, 3)
        
        ! PDF doesn't have RGB pixel data - return white
        rgb_data = 1.0_wp
    end subroutine pdf_extract_rgb_data
    
    subroutine pdf_get_png_data(ctx, width, height, png_data, status)
        type(pdf_context_handle), intent(in) :: ctx
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        ! PDF doesn't generate PNG data
        allocate(png_data(0))
        status = -1
    end subroutine pdf_get_png_data
    
    subroutine pdf_prepare_3d_data(ctx, plots)
        type(pdf_context_handle), intent(inout) :: ctx
        type(plot_data_t), intent(in) :: plots(:)
        
        ! PDF doesn't support 3D - stub implementation
    end subroutine pdf_prepare_3d_data
    
    subroutine pdf_render_ylabel(ctx, ylabel)
        type(pdf_context_handle), intent(inout) :: ctx
        character(len=*), intent(in) :: ylabel
        
        real(wp) :: x, y
        
        x = real(ctx%plot_area%left - 40, wp)
        y = real(ctx%plot_area%bottom + ctx%plot_area%height / 2, wp)
        
        call draw_rotated_mixed_font_text(ctx%core_ctx, x, y, ylabel)
    end subroutine pdf_render_ylabel
    
end module fortplot_pdf_coordinate