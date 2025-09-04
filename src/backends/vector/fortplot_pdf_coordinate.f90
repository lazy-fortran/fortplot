module fortplot_pdf_coordinate
    !! PDF coordinate transformation and polymorphic method support
    !! Handles coordinate normalization and backend-specific method implementations
    
    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core
    use fortplot_pdf_text, only: draw_mixed_font_text, draw_rotated_mixed_font_text, draw_pdf_mathtext
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_pdf_drawing, only: draw_pdf_arrow, draw_pdf_circle_with_outline, &
                                   draw_pdf_square_with_outline, draw_pdf_diamond_with_outline, &
                                   draw_pdf_x_marker
    use fortplot_plot_data, only: plot_data_t
    use fortplot_legend, only: legend_entry_t
    use fortplot_margins, only: plot_area_t, plot_margins_t
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
    public :: normalize_to_pdf_coords, safe_coordinate_transform
    public :: pdf_get_width_scale, pdf_get_height_scale
    public :: pdf_render_legend_specialized, pdf_calculate_legend_dimensions
    public :: pdf_set_legend_border_width, pdf_calculate_legend_position
    public :: pdf_extract_rgb_data, pdf_get_png_data
    public :: pdf_prepare_3d_data, pdf_render_ylabel
    public :: calculate_pdf_plot_area
    
contains

    subroutine normalize_to_pdf_coords(ctx, x, y, pdf_x, pdf_y)
        type(pdf_context_handle), intent(in) :: ctx
        real(wp), intent(in) :: x, y
        real(wp), intent(out) :: pdf_x, pdf_y
        real(wp) :: x_range, y_range
        real(wp) :: x_scale, y_scale
        real(wp), parameter :: EPSILON = 1.0e-10_wp
        real(wp) :: left, right, bottom, top
        
        ! Calculate data ranges with epsilon protection
        x_range = ctx%x_max - ctx%x_min
        y_range = ctx%y_max - ctx%y_min
        
        ! Frame edges
        left   = real(ctx%plot_area%left,  wp)
        right  = real(ctx%plot_area%left + ctx%plot_area%width,  wp)
        bottom = real(ctx%plot_area%bottom, wp)
        top    = real(ctx%plot_area%bottom + ctx%plot_area%height, wp)

        ! Map X using independent scale (center if degenerate)
        if (abs(x_range) < EPSILON) then
            pdf_x = left + (right - left) * 0.5_wp
        else
            x_scale = (right - left) / x_range
            pdf_x = (x - ctx%x_min) * x_scale + left
        end if

        ! Map Y using independent scale (center if degenerate)
        if (abs(y_range) < EPSILON) then
            pdf_y = bottom + (top - bottom) * 0.5_wp
        else
            y_scale = (top - bottom) / y_range
            pdf_y = (y - ctx%y_min) * y_scale + bottom
        end if

        ! Clamp to frame bounds to avoid tiny floating errors leaking past frame
        if (pdf_x < left)  pdf_x = left
        if (pdf_x > right) pdf_x = right
        if (pdf_y < bottom) pdf_y = bottom
        if (pdf_y > top)    pdf_y = top
    end subroutine normalize_to_pdf_coords

    real(wp) function pdf_get_width_scale(ctx) result(scale)
        !! Get width scale - now returns 1.0 since page size matches figure size
        type(pdf_context_handle), intent(in) :: ctx
        associate(dummy_ctxw => ctx%plot_area%width); end associate
        scale = 1.0_wp  ! No scaling needed - PDF page size matches figure size
    end function pdf_get_width_scale
    
    real(wp) function pdf_get_height_scale(ctx) result(scale)
        !! Get height scale - now returns 1.0 since page size matches figure size
        type(pdf_context_handle), intent(in) :: ctx
        associate(dummy_h => ctx%plot_area%height); end associate
        scale = 1.0_wp  ! No scaling needed - PDF page size matches figure size
    end function pdf_get_height_scale
    
    ! Removed unused pdf_fill_quad/pdf_fill_heatmap helpers (deprecated, not referenced)
    
    subroutine pdf_render_legend_specialized(ctx, entries, x, y, width, height)
        type(pdf_context_handle), intent(inout) :: ctx
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(in) :: x, y, width, height
        
        ! Simplified legend rendering with LaTeX and mathtext support
        integer :: i, plen
        real(wp) :: y_pos
        character(len=512) :: processed
        associate(dummy_w => width, dummy_h => height); end associate
        
        y_pos = y
        do i = 1, size(entries)
            ! Process LaTeX commands first to convert to Unicode
            call process_latex_in_text(entries(i)%label, processed, plen)
            
            ! Always use mathtext rendering for legend entries to handle any mathematical notation
            ! This ensures superscripts, subscripts, and Unicode characters are rendered correctly
            call draw_pdf_mathtext(ctx%core_ctx, x, y_pos, processed(1:plen))
            
            y_pos = y_pos - 20.0_wp
        end do
    end subroutine pdf_render_legend_specialized
    
    subroutine pdf_calculate_legend_dimensions(ctx, entries, width, height)
        type(pdf_context_handle), intent(in) :: ctx
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(out) :: width, height
        associate(dummy_ctxw => ctx%plot_area%width); end associate
        
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
        associate(dummy_ctxw => ctx%plot_area%width, dummy_w => width, dummy_h => height); end associate
        
        ! PDF doesn't have RGB pixel data - return white
        rgb_data = 1.0_wp
    end subroutine pdf_extract_rgb_data
    
    subroutine pdf_get_png_data(ctx, width, height, png_data, status)
        type(pdf_context_handle), intent(in) :: ctx
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        associate(dummy_ctxw => ctx%plot_area%width, dummy_w => width, dummy_h => height); end associate
        
        ! PDF doesn't generate PNG data
        allocate(png_data(0))
        status = -1
    end subroutine pdf_get_png_data
    
    subroutine pdf_prepare_3d_data(ctx, plots)
        type(pdf_context_handle), intent(inout) :: ctx
        type(plot_data_t), intent(in) :: plots(:)
        associate(dummy_w => ctx%plot_area%width, dummy_n => size(plots)); end associate
        
        ! PDF doesn't support 3D - stub implementation
    end subroutine pdf_prepare_3d_data
    
    subroutine pdf_render_ylabel(ctx, ylabel)
        type(pdf_context_handle), intent(inout) :: ctx
        character(len=*), intent(in) :: ylabel
        
        real(wp) :: x, y
        
        x = real(ctx%plot_area%left - 30, wp)  ! Reduced spacing from 40 to 30
        y = real(ctx%plot_area%bottom + ctx%plot_area%height / 2, wp)
        
        call draw_rotated_mixed_font_text(ctx%core_ctx, x, y, ylabel)
    end subroutine pdf_render_ylabel
    
    subroutine safe_coordinate_transform(x, y, x_min, x_max, y_min, y_max, &
                                        plot_left, plot_width, plot_bottom, plot_height, &
                                        pdf_x, pdf_y)
        !! Safe coordinate transformation using independent x/y scales
        real(wp), intent(in) :: x, y
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(in) :: plot_left, plot_width, plot_bottom, plot_height
        real(wp), intent(out) :: pdf_x, pdf_y
        real(wp), parameter :: EPSILON = 1.0e-10_wp
        real(wp) :: x_range, y_range
        real(wp) :: x_scale, y_scale
        ! Calculate ranges with epsilon protection
        x_range = x_max - x_min
        y_range = y_max - y_min

        ! Map X using independent scale (center if degenerate)
        if (abs(x_range) < EPSILON) then
            pdf_x = plot_left + plot_width * 0.5_wp
        else
            x_scale = plot_width / x_range
            pdf_x = (x - x_min) * x_scale + plot_left
        end if

        ! Map Y using independent scale (center if degenerate)
        if (abs(y_range) < EPSILON) then
            pdf_y = plot_bottom + plot_height * 0.5_wp
        else
            y_scale = plot_height / y_range
            pdf_y = (y - y_min) * y_scale + plot_bottom
        end if
    end subroutine safe_coordinate_transform

    subroutine calculate_pdf_plot_area(canvas_width, canvas_height, margins, plot_area)
        !! Calculate plot area for PDF backend (mathematical coordinates: Y=0 at bottom)
        integer, intent(in) :: canvas_width, canvas_height
        type(plot_margins_t), intent(in) :: margins
        type(plot_area_t), intent(out) :: plot_area
        ! Calculate positions for mathematical coordinate system (Y=0 at bottom)
        plot_area%left = int(margins%left * real(canvas_width, wp))
        plot_area%width = int(margins%right * real(canvas_width, wp)) - plot_area%left
        ! For PDF mathematical coordinates (Y=0 at bottom), use direct calculation
        plot_area%bottom = int(margins%bottom * real(canvas_height, wp))
        plot_area%height = int(margins%top * real(canvas_height, wp)) - plot_area%bottom
    end subroutine calculate_pdf_plot_area

end module fortplot_pdf_coordinate
