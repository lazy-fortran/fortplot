module fortplot_tt_metrics
    !! TrueType font metrics: scale factors, vertical/horizontal metrics,
    !! glyph offsets, and bounding boxes. Pure Fortran implementations
    !! operating directly on the font byte array via fortplot_tt_binary.
    use fortplot_tt_binary, only: tt_short, tt_ushort, tt_ulong
    use, intrinsic :: iso_fortran_env, only: dp => real64, int8 => int8
    implicit none

    private
    public :: tt_scale_for_pixel_height
    public :: tt_get_font_vmetrics, tt_get_glyph_hmetrics
    public :: tt_get_glyf_offset, tt_get_glyph_box, tt_get_glyph_bitmap_box

contains

    pure function tt_scale_for_pixel_height(data, hhea, height) result(scale)
        !! Compute scale factor to achieve a desired pixel height.
        !! Uses ascent - descent from the hhea table.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: hhea
        real(dp), intent(in) :: height
        real(dp) :: scale
        integer :: fheight

        fheight = tt_short(data, hhea + 4) - tt_short(data, hhea + 6)
        scale = height / real(fheight, dp)
    end function tt_scale_for_pixel_height

    pure subroutine tt_get_font_vmetrics(data, hhea, ascent, descent, line_gap)
        !! Read vertical font metrics (unscaled) from the hhea table.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: hhea
        integer, intent(out) :: ascent, descent, line_gap

        ascent = tt_short(data, hhea + 4)
        descent = tt_short(data, hhea + 6)
        line_gap = tt_short(data, hhea + 8)
    end subroutine tt_get_font_vmetrics

    pure subroutine tt_get_glyph_hmetrics(data, hhea, hmtx, glyph_index, &
            advance_width, left_side_bearing)
        !! Read horizontal metrics for a glyph (unscaled) from the hmtx table.
        !! Glyphs beyond numOfLongHorMetrics share the last advance width.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: hhea, hmtx, glyph_index
        integer, intent(out) :: advance_width, left_side_bearing
        integer :: num_long

        num_long = tt_ushort(data, hhea + 34)
        if (glyph_index < num_long) then
            advance_width = tt_short(data, hmtx + 4*glyph_index)
            left_side_bearing = tt_short(data, hmtx + 4*glyph_index + 2)
        else
            advance_width = tt_short(data, hmtx + 4*(num_long - 1))
            left_side_bearing = tt_short(data, hmtx + 4*num_long &
                + 2*(glyph_index - num_long))
        end if
    end subroutine tt_get_glyph_hmetrics

    pure function tt_get_glyf_offset(data, loca, glyf, index_to_loc_format, &
            num_glyphs, glyph_index) result(offset)
        !! Get byte offset to a glyph in the glyf table.
        !! Returns -1 for empty glyphs or invalid indices.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: loca, glyf, index_to_loc_format
        integer, intent(in) :: num_glyphs, glyph_index
        integer :: offset
        integer :: g1, g2

        offset = -1
        if (glyph_index >= num_glyphs) return
        if (index_to_loc_format >= 2) return

        if (index_to_loc_format == 0) then
            g1 = glyf + tt_ushort(data, loca + glyph_index*2) * 2
            g2 = glyf + tt_ushort(data, loca + glyph_index*2 + 2) * 2
        else
            g1 = glyf + tt_ulong(data, loca + glyph_index*4)
            g2 = glyf + tt_ulong(data, loca + glyph_index*4 + 4)
        end if

        if (g1 == g2) return
        offset = g1
    end function tt_get_glyf_offset

    pure subroutine tt_get_glyph_box(data, loca, glyf, index_to_loc_format, &
            num_glyphs, glyph_index, x0, y0, x1, y1, found)
        !! Get bounding box of a glyph in unscaled font coordinates.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: loca, glyf, index_to_loc_format
        integer, intent(in) :: num_glyphs, glyph_index
        integer, intent(out) :: x0, y0, x1, y1
        logical, intent(out) :: found
        integer :: g

        g = tt_get_glyf_offset(data, loca, glyf, index_to_loc_format, &
            num_glyphs, glyph_index)
        if (g < 0) then
            x0 = 0; y0 = 0; x1 = 0; y1 = 0
            found = .false.
            return
        end if
        x0 = tt_short(data, g + 2)
        y0 = tt_short(data, g + 4)
        x1 = tt_short(data, g + 6)
        y1 = tt_short(data, g + 8)
        found = .true.
    end subroutine tt_get_glyph_box

    pure subroutine tt_get_glyph_bitmap_box(data, loca, glyf, &
            index_to_loc_format, num_glyphs, glyph_index, &
            scale_x, scale_y, ix0, iy0, ix1, iy1)
        !! Get bitmap bounding box in pixel coordinates (y-flipped).
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: loca, glyf, index_to_loc_format
        integer, intent(in) :: num_glyphs, glyph_index
        real(dp), intent(in) :: scale_x, scale_y
        integer, intent(out) :: ix0, iy0, ix1, iy1
        integer :: x0, y0, x1, y1
        logical :: found

        call tt_get_glyph_box(data, loca, glyf, index_to_loc_format, &
            num_glyphs, glyph_index, x0, y0, x1, y1, found)
        if (.not. found) then
            ix0 = 0; iy0 = 0; ix1 = 0; iy1 = 0
            return
        end if
        ix0 = floor(real(x0, dp) * scale_x)
        iy0 = floor(-real(y1, dp) * scale_y)
        ix1 = ceiling(real(x1, dp) * scale_x)
        iy1 = ceiling(-real(y0, dp) * scale_y)
    end subroutine tt_get_glyph_bitmap_box

end module fortplot_tt_metrics
