module fortplot_tt_outlines
    !! TrueType glyph outline extraction.
    !! Parses the glyf table to extract vertex data (moves, lines, quadratic
    !! bezier curves) for simple and composite glyphs. Fortran port of
    !! stbtt__GetGlyphShapeTT from stb_truetype.h lines 1658-1895.
    use fortplot_tt_binary, only: tt_byte, tt_ushort, tt_short, tt_ulong
    use fortplot_tt_metrics, only: tt_get_glyf_offset
    use, intrinsic :: iso_fortran_env, only: int8, dp => real64
    implicit none

    private
    public :: tt_vertex_t, TT_VMOVE, TT_VLINE, TT_VCURVE
    public :: tt_set_vertex, tt_get_glyph_shape

    integer, parameter :: TT_VMOVE = 1
    integer, parameter :: TT_VLINE = 2
    integer, parameter :: TT_VCURVE = 3

    type :: tt_vertex_t
        integer :: x, y
        integer :: cx, cy
        integer :: vtype
    end type tt_vertex_t

contains

    pure function tt_set_vertex(vtype, x, y, cx, cy) result(v)
        !! Construct a vertex with the given type and coordinates.
        integer, intent(in) :: vtype, x, y, cx, cy
        type(tt_vertex_t) :: v

        v%vtype = vtype
        v%x = x
        v%y = y
        v%cx = cx
        v%cy = cy
    end function tt_set_vertex

    pure subroutine tt_close_shape(vertices, nv, was_off, start_off, &
            sx, sy, scx, scy, cx, cy)
        !! Close a contour by emitting final vertex(es) back to the start point.
        !! Handles all four combinations of was_off and start_off flags.
        type(tt_vertex_t), intent(inout) :: vertices(:)
        integer, intent(inout) :: nv
        logical, intent(in) :: was_off, start_off
        integer, intent(in) :: sx, sy, scx, scy, cx, cy

        if (start_off) then
            if (was_off) then
                nv = nv + 1
                vertices(nv) = tt_set_vertex(TT_VCURVE, &
                    shifta(cx + scx, 1), shifta(cy + scy, 1), cx, cy)
            end if
            nv = nv + 1
            vertices(nv) = tt_set_vertex(TT_VCURVE, sx, sy, scx, scy)
        else
            if (was_off) then
                nv = nv + 1
                vertices(nv) = tt_set_vertex(TT_VCURVE, sx, sy, cx, cy)
            else
                nv = nv + 1
                vertices(nv) = tt_set_vertex(TT_VLINE, sx, sy, 0, 0)
            end if
        end if
    end subroutine tt_close_shape

    subroutine tt_get_glyph_shape(data, loca, glyf, index_to_loc_format, &
            num_glyphs, glyph_index, vertices, nvertices)
        !! Extract glyph outline as an array of move/line/curve vertices.
        !! Handles simple glyphs (numberOfContours > 0) and composite glyphs
        !! (numberOfContours < 0) with recursive component resolution.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: loca, glyf, index_to_loc_format
        integer, intent(in) :: num_glyphs, glyph_index
        type(tt_vertex_t), allocatable, intent(out) :: vertices(:)
        integer, intent(out) :: nvertices

        integer :: g, number_of_contours

        nvertices = 0
        g = tt_get_glyf_offset(data, loca, glyf, index_to_loc_format, &
            num_glyphs, glyph_index)
        if (g < 0) return

        number_of_contours = tt_short(data, g)

        if (number_of_contours > 0) then
            call parse_simple_glyph(data, g, number_of_contours, &
                vertices, nvertices)
        else if (number_of_contours < 0) then
            call parse_composite_glyph(data, g, loca, glyf, &
                index_to_loc_format, num_glyphs, vertices, nvertices)
        end if
    end subroutine tt_get_glyph_shape

    subroutine parse_simple_glyph(data, g, number_of_contours, &
            vertices, nvertices)
        !! Parse a simple (non-composite) glyph from the glyf table.
        !! Decodes flags, delta-encoded coordinates, and converts raw points
        !! into move/line/curve vertices with proper contour closing.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: g, number_of_contours
        type(tt_vertex_t), allocatable, intent(out) :: vertices(:)
        integer, intent(out) :: nvertices

        integer :: endpts_off, ins, points_off, n, m, off, ptr

        endpts_off = g + 10
        ins = tt_ushort(data, g + 10 + number_of_contours * 2)
        points_off = g + 10 + number_of_contours * 2 + 2 + ins

        n = 1 + tt_ushort(data, endpts_off + number_of_contours * 2 - 2)
        m = n + 2 * number_of_contours
        allocate(vertices(m))

        off = m - n
        ptr = points_off

        call load_flags(data, vertices, off, n, ptr)
        call load_x_coords(data, vertices, off, n, ptr)
        call load_y_coords(data, vertices, off, n, ptr)
        call build_contours(data, vertices, endpts_off, off, n, nvertices)
    end subroutine parse_simple_glyph

    subroutine load_flags(data, vertices, off, n, ptr)
        !! Load per-point flags from the glyf table into vertices(off+1:off+n)%vtype.
        !! Handles run-length encoding via the repeat flag (bit 3).
        integer(int8), intent(in) :: data(:)
        type(tt_vertex_t), intent(inout) :: vertices(:)
        integer, intent(in) :: off, n
        integer, intent(inout) :: ptr
        integer :: i, flags, flagcount

        flags = 0
        flagcount = 0
        do i = 1, n
            if (flagcount == 0) then
                flags = tt_byte(data, ptr)
                ptr = ptr + 1
                if (iand(flags, 8) /= 0) then
                    flagcount = tt_byte(data, ptr)
                    ptr = ptr + 1
                end if
            else
                flagcount = flagcount - 1
            end if
            vertices(off + i)%vtype = flags
        end do
    end subroutine load_flags

    subroutine load_x_coords(data, vertices, off, n, ptr)
        !! Decode delta-encoded x coordinates from the glyf table.
        !! Flag bit 1 = short (1 byte), bit 4 = sign/same.
        integer(int8), intent(in) :: data(:)
        type(tt_vertex_t), intent(inout) :: vertices(:)
        integer, intent(in) :: off, n
        integer, intent(inout) :: ptr
        integer :: i, flags, dx, x

        x = 0
        do i = 1, n
            flags = vertices(off + i)%vtype
            if (iand(flags, 2) /= 0) then
                dx = tt_byte(data, ptr)
                ptr = ptr + 1
                if (iand(flags, 16) /= 0) then
                    x = x + dx
                else
                    x = x - dx
                end if
            else
                if (iand(flags, 16) == 0) then
                    x = x + tt_short(data, ptr)
                    ptr = ptr + 2
                end if
            end if
            vertices(off + i)%x = x
        end do
    end subroutine load_x_coords

    subroutine load_y_coords(data, vertices, off, n, ptr)
        !! Decode delta-encoded y coordinates from the glyf table.
        !! Flag bit 2 = short (1 byte), bit 5 = sign/same.
        integer(int8), intent(in) :: data(:)
        type(tt_vertex_t), intent(inout) :: vertices(:)
        integer, intent(in) :: off, n
        integer, intent(inout) :: ptr
        integer :: i, flags, dy, y

        y = 0
        do i = 1, n
            flags = vertices(off + i)%vtype
            if (iand(flags, 4) /= 0) then
                dy = tt_byte(data, ptr)
                ptr = ptr + 1
                if (iand(flags, 32) /= 0) then
                    y = y + dy
                else
                    y = y - dy
                end if
            else
                if (iand(flags, 32) == 0) then
                    y = y + tt_short(data, ptr)
                    ptr = ptr + 2
                end if
            end if
            vertices(off + i)%y = y
        end do
    end subroutine load_y_coords

    subroutine build_contours(data, vertices, endpts_off, off, n, nvertices)
        !! Convert raw decoded points into move/line/curve vertex sequences.
        !! Each contour starts with TT_VMOVE; off-curve points generate
        !! TT_VCURVE with implicit on-curve midpoints between consecutive
        !! off-curve points.
        integer(int8), intent(in) :: data(:)
        type(tt_vertex_t), intent(inout) :: vertices(:)
        integer, intent(in) :: endpts_off, off, n
        integer, intent(out) :: nvertices

        integer :: i, j, flags, x, y
        integer :: sx, sy, cx, cy, scx, scy
        integer :: next_move, next_flags
        logical :: was_off, start_off, skip_next

        nvertices = 0
        sx = 0; sy = 0; cx = 0; cy = 0; scx = 0; scy = 0
        was_off = .false.; start_off = .false.
        skip_next = .false.
        next_move = 0
        j = 0

        do i = 1, n
            if (skip_next) then
                skip_next = .false.
                cycle
            end if

            flags = vertices(off + i)%vtype
            x = vertices(off + i)%x
            y = vertices(off + i)%y

            if (i - 1 == next_move) then
                if (i /= 1) then
                    call tt_close_shape(vertices, nvertices, was_off, &
                        start_off, sx, sy, scx, scy, cx, cy)
                end if

                start_off = (iand(flags, 1) == 0)
                if (start_off) then
                    scx = x; scy = y
                    if (i < n) then
                        next_flags = vertices(off + i + 1)%vtype
                        if (iand(next_flags, 1) == 0) then
                            sx = shifta(x + vertices(off + i + 1)%x, 1)
                            sy = shifta(y + vertices(off + i + 1)%y, 1)
                        else
                            sx = vertices(off + i + 1)%x
                            sy = vertices(off + i + 1)%y
                            skip_next = .true.
                        end if
                    else
                        sx = x; sy = y
                    end if
                else
                    sx = x; sy = y
                end if

                nvertices = nvertices + 1
                vertices(nvertices) = tt_set_vertex(TT_VMOVE, sx, sy, 0, 0)
                was_off = .false.
                next_move = 1 + tt_ushort(data, endpts_off + j * 2)
                j = j + 1
            else
                if (iand(flags, 1) == 0) then
                    if (was_off) then
                        nvertices = nvertices + 1
                        vertices(nvertices) = tt_set_vertex(TT_VCURVE, &
                            shifta(cx + x, 1), shifta(cy + y, 1), cx, cy)
                    end if
                    cx = x; cy = y
                    was_off = .true.
                else
                    if (was_off) then
                        nvertices = nvertices + 1
                        vertices(nvertices) = tt_set_vertex(TT_VCURVE, &
                            x, y, cx, cy)
                    else
                        nvertices = nvertices + 1
                        vertices(nvertices) = tt_set_vertex(TT_VLINE, &
                            x, y, 0, 0)
                    end if
                    was_off = .false.
                end if
            end if
        end do

        call tt_close_shape(vertices, nvertices, was_off, start_off, &
            sx, sy, scx, scy, cx, cy)
    end subroutine build_contours

    subroutine parse_composite_glyph(data, g, loca, glyf, &
            index_to_loc_format, num_glyphs, vertices, nvertices)
        !! Parse a composite glyph by recursively resolving component glyphs,
        !! applying affine transforms, and concatenating vertex arrays.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: g, loca, glyf, index_to_loc_format, num_glyphs
        type(tt_vertex_t), allocatable, intent(out) :: vertices(:)
        integer, intent(out) :: nvertices

        type(tt_vertex_t), allocatable :: comp_verts(:), tmp(:)
        integer :: comp_off, comp_flags, gidx, comp_nv
        integer :: x, y, k
        real(dp) :: mtx(6), m_scale, n_scale

        comp_off = g + 10
        nvertices = 0

        do
            comp_flags = tt_ushort(data, comp_off); comp_off = comp_off + 2
            gidx = tt_ushort(data, comp_off); comp_off = comp_off + 2

            call read_component_transform(data, comp_flags, comp_off, mtx)

            m_scale = sqrt(mtx(1) * mtx(1) + mtx(2) * mtx(2))
            n_scale = sqrt(mtx(3) * mtx(3) + mtx(4) * mtx(4))

            call tt_get_glyph_shape(data, loca, glyf, index_to_loc_format, &
                num_glyphs, gidx, comp_verts, comp_nv)

            if (comp_nv > 0) then
                do k = 1, comp_nv
                    x = comp_verts(k)%x; y = comp_verts(k)%y
                    comp_verts(k)%x = nint(m_scale * (mtx(1)*x + mtx(3)*y + mtx(5)))
                    comp_verts(k)%y = nint(n_scale * (mtx(2)*x + mtx(4)*y + mtx(6)))
                    x = comp_verts(k)%cx; y = comp_verts(k)%cy
                    comp_verts(k)%cx = nint(m_scale * (mtx(1)*x + mtx(3)*y + mtx(5)))
                    comp_verts(k)%cy = nint(n_scale * (mtx(2)*x + mtx(4)*y + mtx(6)))
                end do

                if (allocated(vertices)) then
                    allocate(tmp(nvertices + comp_nv))
                    tmp(1:nvertices) = vertices(1:nvertices)
                    tmp(nvertices + 1:nvertices + comp_nv) = comp_verts(1:comp_nv)
                    call move_alloc(tmp, vertices)
                else
                    allocate(vertices(comp_nv))
                    vertices(1:comp_nv) = comp_verts(1:comp_nv)
                end if
                nvertices = nvertices + comp_nv
                deallocate(comp_verts)
            end if

            if (iand(comp_flags, 32) == 0) exit
        end do
    end subroutine parse_composite_glyph

    subroutine read_component_transform(data, comp_flags, comp_off, mtx)
        !! Read the affine transform for a composite glyph component.
        !! Decodes translation (8-bit or 16-bit, xy or point-number) and
        !! optional scale/rotation matrix entries from the glyf table.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: comp_flags
        integer, intent(inout) :: comp_off
        real(dp), intent(out) :: mtx(6)
        integer :: byte_val

        mtx = [1.0_dp, 0.0_dp, 0.0_dp, 1.0_dp, 0.0_dp, 0.0_dp]

        if (iand(comp_flags, 2) /= 0) then
            ! ARGS_ARE_XY_VALUES
            if (iand(comp_flags, 1) /= 0) then
                mtx(5) = real(tt_short(data, comp_off), dp)
                comp_off = comp_off + 2
                mtx(6) = real(tt_short(data, comp_off), dp)
                comp_off = comp_off + 2
            else
                byte_val = tt_byte(data, comp_off)
                if (byte_val >= 128) byte_val = byte_val - 256
                mtx(5) = real(byte_val, dp)
                comp_off = comp_off + 1
                byte_val = tt_byte(data, comp_off)
                if (byte_val >= 128) byte_val = byte_val - 256
                mtx(6) = real(byte_val, dp)
                comp_off = comp_off + 1
            end if
        else
            if (iand(comp_flags, 1) /= 0) then
                comp_off = comp_off + 4
            else
                comp_off = comp_off + 2
            end if
        end if

        if (iand(comp_flags, 8) /= 0) then
            mtx(1) = real(tt_short(data, comp_off), dp) / 16384.0_dp
            mtx(4) = mtx(1)
            mtx(2) = 0.0_dp; mtx(3) = 0.0_dp
            comp_off = comp_off + 2
        else if (iand(comp_flags, 64) /= 0) then
            mtx(1) = real(tt_short(data, comp_off), dp) / 16384.0_dp
            comp_off = comp_off + 2
            mtx(2) = 0.0_dp; mtx(3) = 0.0_dp
            mtx(4) = real(tt_short(data, comp_off), dp) / 16384.0_dp
            comp_off = comp_off + 2
        else if (iand(comp_flags, 128) /= 0) then
            mtx(1) = real(tt_short(data, comp_off), dp) / 16384.0_dp
            comp_off = comp_off + 2
            mtx(2) = real(tt_short(data, comp_off), dp) / 16384.0_dp
            comp_off = comp_off + 2
            mtx(3) = real(tt_short(data, comp_off), dp) / 16384.0_dp
            comp_off = comp_off + 2
            mtx(4) = real(tt_short(data, comp_off), dp) / 16384.0_dp
            comp_off = comp_off + 2
        end if
    end subroutine read_component_transform

end module fortplot_tt_outlines
