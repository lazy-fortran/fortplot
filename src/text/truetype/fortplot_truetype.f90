module fortplot_truetype
    !! Pure Fortran TrueType font parser and rasterizer.
    !! Based on stb_truetype.h v1.26 by Sean Barrett.
    !! Original C implementation rewritten in Fortran for the fortplot project.
    use fortplot_tt_binary, only: tt_load_file
    use fortplot_tt_tables, only: tt_is_font, tt_get_font_offset_for_index, &
        tt_init_font_tables
    use fortplot_tt_cmap, only: tt_find_glyph_index
    use fortplot_tt_metrics, only: tt_scale_for_pixel_height, &
        tt_get_font_vmetrics, tt_get_glyph_hmetrics, tt_get_glyf_offset, &
        tt_get_glyph_bitmap_box
    use fortplot_tt_outlines, only: tt_vertex_t, tt_get_glyph_shape
    use fortplot_tt_curves, only: tt_point_t, tt_flatten_curves
    use, intrinsic :: iso_fortran_env, only: dp => real64, int8
    implicit none

    private
    public :: truetype_font_t

    type :: truetype_font_t
        integer(int8), allocatable :: data(:)
        integer :: fontstart = 0
        integer :: num_glyphs = 0
        integer :: loca = 0, head = 0, glyf = 0, hhea = 0, hmtx = 0
        integer :: kern = 0, gpos = 0
        integer :: index_map = 0
        integer :: index_to_loc_format = 0
        logical :: initialized = .false.
    contains
        procedure :: init => font_init
        procedure :: cleanup => font_cleanup
        procedure :: scale_for_pixel_height => font_scale
        procedure :: get_vmetrics => font_vmetrics
        procedure :: get_hmetrics => font_hmetrics
        procedure :: find_glyph_index => font_find_glyph
        procedure :: get_bitmap_box => font_bitmap_box
        procedure :: get_codepoint_bitmap => font_get_bitmap
        procedure :: make_codepoint_bitmap => font_make_bitmap
    end type truetype_font_t

contains

    function font_init(self, filepath) result(success)
        !! Load a TrueType font from file.
        class(truetype_font_t), intent(inout) :: self
        character(len=*), intent(in) :: filepath
        logical :: success
        integer :: file_size, offset

        success = .false.
        call self%cleanup()

        if (.not. tt_load_file(filepath, self%data, file_size)) return

        offset = tt_get_font_offset_for_index(self%data, 0)
        if (offset < 0) then
            deallocate(self%data)
            return
        end if
        self%fontstart = offset

        call tt_init_font_tables(self%data, self%fontstart, &
            self%loca, self%head, self%glyf, self%hhea, self%hmtx, &
            self%kern, self%gpos, self%index_map, &
            self%index_to_loc_format, self%num_glyphs, success)

        if (.not. success) then
            deallocate(self%data)
            return
        end if

        self%initialized = .true.
    end function font_init

    subroutine font_cleanup(self)
        !! Release all font resources.
        class(truetype_font_t), intent(inout) :: self

        if (allocated(self%data)) deallocate(self%data)
        self%fontstart = 0
        self%num_glyphs = 0
        self%loca = 0; self%head = 0; self%glyf = 0
        self%hhea = 0; self%hmtx = 0
        self%kern = 0; self%gpos = 0
        self%index_map = 0; self%index_to_loc_format = 0
        self%initialized = .false.
    end subroutine font_cleanup

    function font_scale(self, pixel_height) result(scale)
        !! Get scale factor for desired pixel height.
        class(truetype_font_t), intent(in) :: self
        real(dp), intent(in) :: pixel_height
        real(dp) :: scale

        if (.not. self%initialized) then
            scale = 0.0_dp
            return
        end if
        scale = tt_scale_for_pixel_height(self%data, self%hhea, pixel_height)
    end function font_scale

    subroutine font_vmetrics(self, ascent, descent, line_gap)
        !! Get vertical font metrics (unscaled).
        class(truetype_font_t), intent(in) :: self
        integer, intent(out) :: ascent, descent, line_gap

        if (.not. self%initialized) then
            ascent = 0; descent = 0; line_gap = 0
            return
        end if
        call tt_get_font_vmetrics(self%data, self%hhea, &
            ascent, descent, line_gap)
    end subroutine font_vmetrics

    subroutine font_hmetrics(self, codepoint, advance_width, left_side_bearing)
        !! Get horizontal metrics for a codepoint (unscaled).
        class(truetype_font_t), intent(in) :: self
        integer, intent(in) :: codepoint
        integer, intent(out) :: advance_width, left_side_bearing
        integer :: glyph_index

        if (.not. self%initialized) then
            advance_width = 0; left_side_bearing = 0
            return
        end if
        glyph_index = tt_find_glyph_index(self%data, self%index_map, codepoint)
        call tt_get_glyph_hmetrics(self%data, self%hhea, self%hmtx, &
            glyph_index, advance_width, left_side_bearing)
    end subroutine font_hmetrics

    function font_find_glyph(self, codepoint) result(glyph_index)
        !! Map a Unicode codepoint to a glyph index.
        class(truetype_font_t), intent(in) :: self
        integer, intent(in) :: codepoint
        integer :: glyph_index

        if (.not. self%initialized) then
            glyph_index = 0
            return
        end if
        glyph_index = tt_find_glyph_index(self%data, self%index_map, codepoint)
    end function font_find_glyph

    subroutine font_bitmap_box(self, codepoint, scale_x, scale_y, &
            ix0, iy0, ix1, iy1)
        !! Get bitmap bounding box for a codepoint.
        class(truetype_font_t), intent(in) :: self
        integer, intent(in) :: codepoint
        real(dp), intent(in) :: scale_x, scale_y
        integer, intent(out) :: ix0, iy0, ix1, iy1
        integer :: glyph_index

        ix0 = 0; iy0 = 0; ix1 = 0; iy1 = 0
        if (.not. self%initialized) return
        glyph_index = tt_find_glyph_index(self%data, self%index_map, codepoint)
        call tt_get_glyph_bitmap_box(self%data, self%loca, self%glyf, &
            self%index_to_loc_format, self%num_glyphs, glyph_index, &
            scale_x, scale_y, ix0, iy0, ix1, iy1)
    end subroutine font_bitmap_box

    subroutine font_get_bitmap(self, scale_x, scale_y, codepoint, &
            bitmap, width, height, xoff, yoff)
        !! Render a codepoint to an 8-bit grayscale bitmap.
        !! The bitmap is returned as an allocatable int8 array.
        use fortplot_tt_rasterizer, only: tt_rasterize
        class(truetype_font_t), intent(in) :: self
        real(dp), intent(in) :: scale_x, scale_y
        integer, intent(in) :: codepoint
        integer(int8), allocatable, intent(out) :: bitmap(:)
        integer, intent(out) :: width, height, xoff, yoff

        type(tt_vertex_t), allocatable :: vertices(:)
        type(tt_point_t), allocatable :: points(:)
        integer, allocatable :: contour_lengths(:)
        integer :: nvertices, num_contours, glyph_index
        integer :: ix0, iy0, ix1, iy1
        real(dp) :: scale, flatness

        width = 0; height = 0; xoff = 0; yoff = 0
        if (.not. self%initialized) return
        if (scale_x == 0.0_dp .and. scale_y == 0.0_dp) return

        glyph_index = tt_find_glyph_index(self%data, self%index_map, codepoint)

        call tt_get_glyph_shape(self%data, self%loca, self%glyf, &
            self%index_to_loc_format, self%num_glyphs, glyph_index, &
            vertices, nvertices)
        if (nvertices == 0) return

        call tt_get_glyph_bitmap_box(self%data, self%loca, self%glyf, &
            self%index_to_loc_format, self%num_glyphs, glyph_index, &
            scale_x, scale_y, ix0, iy0, ix1, iy1)

        width = ix1 - ix0
        height = iy1 - iy0
        xoff = ix0
        yoff = iy0
        if (width <= 0 .or. height <= 0) then
            width = 0; height = 0
            return
        end if

        allocate(bitmap(width * height))
        bitmap = 0_int8

        scale = min(abs(scale_x), abs(scale_y))
        if (scale == 0.0_dp) scale = max(abs(scale_x), abs(scale_y))
        flatness = 0.35_dp / scale

        call tt_flatten_curves(vertices, nvertices, flatness, &
            points, contour_lengths, num_contours)

        if (num_contours > 0 .and. allocated(points)) then
            call tt_rasterize(bitmap, width, height, width, &
                points, contour_lengths, num_contours, &
                scale_x, scale_y, 0.0_dp, 0.0_dp, &
                ix0, iy0, .true.)
        end if
    end subroutine font_get_bitmap

    subroutine font_make_bitmap(self, output, out_w, out_h, out_stride, &
            scale_x, scale_y, codepoint)
        !! Render a codepoint into a user-provided buffer.
        use fortplot_tt_rasterizer, only: tt_rasterize
        class(truetype_font_t), intent(in) :: self
        integer(int8), intent(inout) :: output(:)
        integer, intent(in) :: out_w, out_h, out_stride
        real(dp), intent(in) :: scale_x, scale_y
        integer, intent(in) :: codepoint

        type(tt_vertex_t), allocatable :: vertices(:)
        type(tt_point_t), allocatable :: points(:)
        integer, allocatable :: contour_lengths(:)
        integer :: nvertices, num_contours, glyph_index
        integer :: ix0, iy0
        real(dp) :: scale, flatness

        if (.not. self%initialized) return
        if (out_w <= 0 .or. out_h <= 0) return

        glyph_index = tt_find_glyph_index(self%data, self%index_map, codepoint)

        call tt_get_glyph_shape(self%data, self%loca, self%glyf, &
            self%index_to_loc_format, self%num_glyphs, glyph_index, &
            vertices, nvertices)
        if (nvertices == 0) return

        call tt_get_glyph_bitmap_box(self%data, self%loca, self%glyf, &
            self%index_to_loc_format, self%num_glyphs, glyph_index, &
            scale_x, scale_y, ix0, iy0, ix0, iy0)

        scale = min(abs(scale_x), abs(scale_y))
        if (scale == 0.0_dp) scale = max(abs(scale_x), abs(scale_y))
        flatness = 0.35_dp / scale

        call tt_flatten_curves(vertices, nvertices, flatness, &
            points, contour_lengths, num_contours)

        if (num_contours > 0 .and. allocated(points)) then
            call tt_rasterize(output, out_w, out_h, out_stride, &
                points, contour_lengths, num_contours, &
                scale_x, scale_y, 0.0_dp, 0.0_dp, &
                ix0, iy0, .true.)
        end if
    end subroutine font_make_bitmap

end module fortplot_truetype
