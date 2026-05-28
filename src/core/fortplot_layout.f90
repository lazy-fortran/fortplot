module fortplot_layout
    !! Layout and margin calculations for plotting backends
    !! 
    !! This module handles plot area geometry and margin calculations,
    !! following Single Responsibility Principle by focusing solely
    !! on layout-related functionality.
    !! 
    !! Author: fortplot contributors
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: plot_margins_t, plot_area_t, calculate_plot_area
    public :: twiny_top_offset_px
    
    ! Matplotlib default margins (exact values from matplotlibrc)
    type :: plot_margins_t
        real(wp) :: left = 0.125_wp    ! 12.5% left margin 
        real(wp) :: right = 0.9_wp     ! 90% right edge (not margin!)
        real(wp) :: bottom = 0.11_wp   ! 11% bottom margin
        real(wp) :: top = 0.88_wp      ! 88% top edge (not margin!)
    end type plot_margins_t
    
    ! Plot area geometry
    type :: plot_area_t
        integer :: left, bottom, width, height
    end type plot_area_t
    
contains

    subroutine calculate_plot_area(canvas_width, canvas_height, margins, plot_area, &
                                   top_offset_px)
        !! Calculate plot area based on canvas size and margins (matplotlib-compatible)
        !! Note: left/bottom are margins, right/top are edge positions
        !!
        !! top_offset_px (optional) pushes the plot-area top edge downward by that
        !! many image pixels. A twiny (top x-axis) needs this so its tick labels,
        !! axis label, and the figure title fit above the axes without clipping.
        !! When absent or non-positive the geometry is unchanged, preserving the
        !! matplotlib-exact default edges.
        integer, intent(in) :: canvas_width, canvas_height
        type(plot_margins_t), intent(in) :: margins
        type(plot_area_t), intent(out) :: plot_area
        integer, intent(in), optional :: top_offset_px

        integer :: right_edge, top_edge, offset

        offset = 0
        if (present(top_offset_px)) offset = max(0, top_offset_px)

        ! Round each edge to the nearest pixel, matching matplotlib's axes bbox
        ! rounding. ceiling()/floor() biased the top edge up by one pixel
        ! (427.2 -> 428 instead of 427) for the default 640x480 canvas.
        plot_area%left = nint(margins%left * real(canvas_width, wp))
        right_edge = nint(margins%right * real(canvas_width, wp))
        plot_area%width = right_edge - plot_area%left

        ! Image coordinates have Y=0 at the top, so the edges are flipped.
        plot_area%bottom = nint((1.0_wp - margins%top) * real(canvas_height, wp)) &
                           + offset
        top_edge = nint((1.0_wp - margins%bottom) * real(canvas_height, wp))
        plot_area%height = top_edge - plot_area%bottom
    end subroutine calculate_plot_area

    pure integer function twiny_top_offset_px(dpi, has_top_xlabel) result(offset)
        !! Extra image pixels to push the plot-area top edge down for a twiny
        !! (top x-axis), beyond the default top margin.
        !!
        !! The raster/PDF backends stack the top band upward from the plot-area
        !! top edge as: top tick labels, then the top axis label, then the title.
        !! Without a twiny only the title sits above the axes, and the default
        !! top margin already holds it inside the canvas. A twiny inserts the
        !! tick labels and the top axis label below the title, so the stack grows
        !! and the upper elements clip at y=0. This returns that added height.
        !!
        !! Tick and axis label heights match DEFAULT_FONT_SIZE points scaled to
        !! device pixels by DPI; pads match the constants the label placement
        !! applies (X_TICK_LABEL_TOP_PAD, CANVAS_EDGE_PADDING_PX). The added
        !! TITLE_VERTICAL_OFFSET is the gap the title now needs above the
        !! top-axis block. The title's own height is not added: the default top
        !! margin already reserves it, and with a twiny the title simply moves
        !! from just above the axes to the top of this taller stack.
        use fortplot_constants, only: REFERENCE_DPI, X_TICK_LABEL_TOP_PAD, &
                                      TITLE_VERTICAL_OFFSET, CANVAS_EDGE_PADDING_PX
        real(wp), intent(in) :: dpi
        logical, intent(in) :: has_top_xlabel

        integer, parameter :: LABEL_FONT_PT = 16          ! DEFAULT_FONT_SIZE
        real(wp) :: dpi_val, scale
        integer :: label_h

        dpi_val = dpi
        if (dpi_val <= 0.0_wp) dpi_val = REFERENCE_DPI
        scale = dpi_val / REFERENCE_DPI
        label_h = nint(real(LABEL_FONT_PT, wp) * scale)

        ! Top tick labels, the pad above the axis frame, and the title gap that
        ! now separates the title from the top-axis block are always added.
        offset = nint(real(CANVAS_EDGE_PADDING_PX, wp) * scale) + &
                 nint(real(X_TICK_LABEL_TOP_PAD, wp) * scale) + label_h + &
                 TITLE_VERTICAL_OFFSET

        ! Reserve room for the top axis label only when one is set.
        if (has_top_xlabel) offset = offset + label_h
    end function twiny_top_offset_px

end module fortplot_layout
