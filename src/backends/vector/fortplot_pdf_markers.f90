module fortplot_pdf_markers
    !! PDF marker and graphics state operations
    !! Handles marker drawing, color management, and graphics state operations
    
    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core
    use fortplot_pdf_drawing, only: pdf_stream_writer, draw_pdf_arrow, &
                                   draw_pdf_circle_with_outline, draw_pdf_square_with_outline, &
                                   draw_pdf_diamond_with_outline, draw_pdf_x_marker
    use fortplot_pdf_coordinate, only: pdf_context_handle, normalize_to_pdf_coords
    implicit none
    
    private
    
    public :: draw_pdf_marker_at_coords
    public :: pdf_set_marker_colors, pdf_set_marker_colors_with_alpha
    public :: draw_pdf_arrow_at_coords
    
contains

    subroutine draw_pdf_marker_at_coords(ctx_handle, stream_writer, x, y, style)
        type(pdf_context_handle), intent(in) :: ctx_handle
        type(pdf_stream_writer), intent(inout) :: stream_writer
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp) :: pdf_x, pdf_y
        real(wp) :: size
        
        size = 5.0_wp
        call normalize_to_pdf_coords(ctx_handle, x, y, pdf_x, pdf_y)
        
        ! Save state for marker drawing
        call stream_writer%save_state()
        
        ! Draw marker based on style
        select case(trim(style))
        case('o', 'circle')
            call draw_pdf_circle_with_outline(stream_writer, pdf_x, pdf_y, size)
        case('s', 'square')
            call draw_pdf_square_with_outline(stream_writer, pdf_x, pdf_y, size)
        case('D', 'd', 'diamond')
            call draw_pdf_diamond_with_outline(stream_writer, pdf_x, pdf_y, size)
        case('x', 'cross')
            call draw_pdf_x_marker(stream_writer, pdf_x, pdf_y, size)
        end select
        
        ! Restore state
        call stream_writer%restore_state()
    end subroutine draw_pdf_marker_at_coords
    
    subroutine pdf_set_marker_colors(core_ctx, edge_r, edge_g, edge_b, face_r, face_g, face_b)
        type(pdf_context_core), intent(inout) :: core_ctx
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b
        
        ! Reference face color components to keep interface stable
        associate(df1=>face_r, df2=>face_g, df3=>face_b); end associate
        ! Set edge color for stroking
        call core_ctx%set_color(edge_r, edge_g, edge_b)
    end subroutine pdf_set_marker_colors
    
    subroutine pdf_set_marker_colors_with_alpha(core_ctx, edge_r, edge_g, edge_b, edge_alpha, &
                                               face_r, face_g, face_b, face_alpha)
        type(pdf_context_core), intent(inout) :: core_ctx
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        
        ! Reference alpha components to keep interface stable; PDF ignores alpha
        associate(da1=>edge_alpha, da2=>face_alpha); end associate
        ! PDF doesn't support alpha directly, just use the colors
        call pdf_set_marker_colors(core_ctx, edge_r, edge_g, edge_b, face_r, face_g, face_b)
    end subroutine pdf_set_marker_colors_with_alpha
    
    subroutine draw_pdf_arrow_at_coords(ctx_handle, stream_writer, x, y, dx, dy, size, style)
        type(pdf_context_handle), intent(in) :: ctx_handle
        type(pdf_stream_writer), intent(inout) :: stream_writer
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        real(wp) :: pdf_x, pdf_y
        
        call normalize_to_pdf_coords(ctx_handle, x, y, pdf_x, pdf_y)
        call draw_pdf_arrow(stream_writer, pdf_x, pdf_y, dx, dy, size, style)
    end subroutine draw_pdf_arrow_at_coords
    
end module fortplot_pdf_markers
