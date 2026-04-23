module fortplot_tt_tables
    !! TrueType table lookup and font initialization.
    !! Validates font signatures, finds tables in the table directory,
    !! and initializes a font by locating all required tables and the cmap encoding.
    use fortplot_tt_binary, only: tt_byte, tt_ushort, tt_ulong, tt_tag_match
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none

    private
    public :: tt_is_font, tt_find_table
    public :: tt_get_font_offset_for_index, tt_init_font_tables

contains

    pure function tt_is_font(data, offset) result(is_valid)
        !! Check if data at offset contains a valid TrueType/OpenType signature.
        !! Recognizes: 00-01-00-00 (standard), "true", "typ1", "OTTO".
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: offset
        logical :: is_valid

        is_valid = .false.
        if (offset + 4 > size(data)) return

        ! Standard TrueType: 00 01 00 00
        if (tt_byte(data, offset) == 0 .and. tt_byte(data, offset + 1) == 1 .and. &
            tt_byte(data, offset + 2) == 0 .and. tt_byte(data, offset + 3) == 0) then
            is_valid = .true.
            return
        end if

        ! Apple TrueType
        if (tt_tag_match(data, offset, 'true')) then
            is_valid = .true.
            return
        end if

        ! TrueType with Type1 tables
        if (tt_tag_match(data, offset, 'typ1')) then
            is_valid = .true.
            return
        end if

        ! OpenType with CFF data
        if (tt_tag_match(data, offset, 'OTTO')) then
            is_valid = .true.
            return
        end if
    end function tt_is_font

    pure function tt_find_table(data, fontstart, tag) result(table_offset)
        !! Find a table by its 4-byte tag in the table directory.
        !! Returns the table byte offset, or 0 if not found.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: fontstart
        character(len=4), intent(in) :: tag
        integer :: table_offset
        integer :: num_tables, i, record_offset

        table_offset = 0
        num_tables = tt_ushort(data, fontstart + 4)

        do i = 0, num_tables - 1
            record_offset = fontstart + 12 + 16*i
            if (tt_tag_match(data, record_offset, tag)) then
                table_offset = tt_ulong(data, record_offset + 8)
                return
            end if
        end do
    end function tt_find_table

    pure function tt_get_font_offset_for_index(data, index) result(offset)
        !! Get font byte offset for a given index in a TTC collection.
        !! For a standalone font, index 0 returns 0.
        !! Returns -1 if the index is out of range or data is invalid.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: index
        integer :: offset
        integer :: version, num_fonts

        offset = -1

        ! Standalone font: only index 0 is valid
        if (tt_is_font(data, 0)) then
            if (index == 0) offset = 0
            return
        end if

        ! TrueType Collection
        if (.not. tt_tag_match(data, 0, 'ttcf')) return

        ! TTC header version is a 16.16 fixed-point number:
        ! 1.0 = 0x00010000 (65536), 2.0 = 0x00020000 (131072).
        version = tt_ulong(data, 4)
        if (version /= 65536 .and. version /= 131072) return

        num_fonts = tt_ulong(data, 8)
        if (index < 0 .or. index >= num_fonts) return

        offset = tt_ulong(data, 12 + index*4)
    end function tt_get_font_offset_for_index

    pure subroutine tt_init_font_tables(data, fontstart, loca, head, glyf, hhea, &
            hmtx, kern, gpos, index_map, index_to_loc_format, &
            num_glyphs, success)
        !! Initialize a TrueType font by locating all required tables and the
        !! cmap encoding subtable. Sets success=.false. if required tables are
        !! missing, no glyf table is present (CFF not supported), or no usable
        !! cmap encoding is found.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: fontstart
        integer, intent(out) :: loca, head, glyf, hhea, hmtx
        integer, intent(out) :: kern, gpos
        integer, intent(out) :: index_map, index_to_loc_format, num_glyphs
        logical, intent(out) :: success
        integer :: cmap, maxp_table
        integer :: num_cmap_tables, i, encoding_record, platform_id, encoding_id

        success = .false.

        ! Locate required and optional tables
        cmap = tt_find_table(data, fontstart, 'cmap')
        loca = tt_find_table(data, fontstart, 'loca')
        head = tt_find_table(data, fontstart, 'head')
        glyf = tt_find_table(data, fontstart, 'glyf')
        hhea = tt_find_table(data, fontstart, 'hhea')
        hmtx = tt_find_table(data, fontstart, 'hmtx')
        kern = tt_find_table(data, fontstart, 'kern')
        gpos = tt_find_table(data, fontstart, 'GPOS')

        ! Validate required tables
        if (cmap == 0 .or. head == 0 .or. hhea == 0 .or. hmtx == 0) return
        if (glyf == 0) return
        if (loca == 0) return

        ! Number of glyphs from maxp table
        maxp_table = tt_find_table(data, fontstart, 'maxp')
        if (maxp_table /= 0) then
            num_glyphs = tt_ushort(data, maxp_table + 4)
        else
            num_glyphs = 65535
        end if

        ! Find a usable cmap encoding subtable
        num_cmap_tables = tt_ushort(data, cmap + 2)
        index_map = 0
        do i = 0, num_cmap_tables - 1
            encoding_record = cmap + 4 + 8*i
            platform_id = tt_ushort(data, encoding_record)
            select case (platform_id)
            case (3)  ! Microsoft
                encoding_id = tt_ushort(data, encoding_record + 2)
                if (encoding_id == 1 .or. encoding_id == 10) then
                    index_map = cmap + tt_ulong(data, encoding_record + 4)
                end if
            case (0)  ! Unicode
                index_map = cmap + tt_ulong(data, encoding_record + 4)
            end select
        end do
        if (index_map == 0) return

        ! Loca table entry format from head table offset 50
        index_to_loc_format = tt_ushort(data, head + 50)

        success = .true.
    end subroutine tt_init_font_tables

end module fortplot_tt_tables
