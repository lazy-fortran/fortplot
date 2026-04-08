module fortplot_tt_cmap
    !! TrueType cmap (character map) table parser.
    !! Maps Unicode codepoints to glyph indices, supporting formats 0, 4, 6, 12, and 13.
    use, intrinsic :: iso_fortran_env, only: int8
    use fortplot_tt_binary, only: tt_byte, tt_ushort, tt_short, tt_ulong
    implicit none

    private
    public :: tt_find_glyph_index

contains

    pure function tt_find_glyph_index(data, index_map, &
                                      unicode_codepoint) &
                                      result(glyph_index)
        !! Look up the glyph index for a Unicode codepoint in a cmap subtable.
        !! Returns 0 when the codepoint is not covered by the subtable.
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: index_map
        integer, intent(in) :: unicode_codepoint
        integer :: glyph_index

        integer :: fmt, bytes, first, count
        integer :: segcount, search_range, entry_selector, range_shift
        integer :: end_count, search, item, start_val, last_val, offset_val, delta
        integer :: ngroups, low, high, mid, start_char, end_char, start_glyph

        glyph_index = 0
        fmt = tt_ushort(data, index_map)

        select case (fmt)
        case (0)
            bytes = tt_ushort(data, index_map + 2)
            if (unicode_codepoint < bytes - 6) then
                glyph_index = tt_byte(data, index_map + 6 + unicode_codepoint)
            end if

        case (6)
            first = tt_ushort(data, index_map + 6)
            count = tt_ushort(data, index_map + 8)
            if (unicode_codepoint >= first .and. unicode_codepoint < first + count) then
                glyph_index = tt_ushort(data, index_map + 10 &
                    + (unicode_codepoint - first) * 2)
            end if

        case (4)
            if (unicode_codepoint > 65535) return

            segcount = ishft(tt_ushort(data, index_map + 6), -1)
            search_range = ishft(tt_ushort(data, index_map + 8), -1)
            entry_selector = tt_ushort(data, index_map + 10)
            range_shift = ishft(tt_ushort(data, index_map + 12), -1)

            end_count = index_map + 14
            search = end_count

            if (unicode_codepoint >= tt_ushort(data, search + range_shift * 2)) then
                search = search + range_shift * 2
            end if

            search = search - 2
            do while (entry_selector > 0)
                search_range = ishft(search_range, -1)
                if (unicode_codepoint > tt_ushort(data, search + search_range * 2)) then
                    search = search + search_range * 2
                end if
                entry_selector = entry_selector - 1
            end do
            search = search + 2

            item = ishft(search - end_count, -1)
            start_val = tt_ushort(data, index_map + 14 + segcount * 2 + 2 + 2 * item)
            last_val = tt_ushort(data, end_count + 2 * item)
            if (unicode_codepoint < start_val .or. unicode_codepoint > last_val) return

            offset_val = tt_ushort(data, index_map + 14 + segcount * 6 + 2 + 2 * item)
            if (offset_val == 0) then
                delta = tt_short(data, index_map + 14 + segcount * 4 + 2 + 2 * item)
                glyph_index = iand(unicode_codepoint + delta, 65535)
            else
                glyph_index = tt_ushort(data, offset_val &
                    + (unicode_codepoint - start_val) * 2 &
                    + index_map + 14 + segcount * 6 &
                    + 2 + 2 * item)
            end if

        case (12, 13)
            ngroups = tt_ulong(data, index_map + 12)
            low = 0
            high = ngroups
            do while (low < high)
                mid = low + ishft(high - low, -1)
                start_char = tt_ulong(data, index_map + 16 + mid * 12)
                end_char = tt_ulong(data, index_map + 16 + mid * 12 + 4)
                if (unicode_codepoint < start_char) then
                    high = mid
                else if (unicode_codepoint > end_char) then
                    low = mid + 1
                else
                    start_glyph = tt_ulong(data, index_map + 16 + mid * 12 + 8)
                    if (fmt == 12) then
                        glyph_index = start_glyph + unicode_codepoint - start_char
                    else
                        glyph_index = start_glyph
                    end if
                    return
                end if
            end do

        end select
    end function tt_find_glyph_index

end module fortplot_tt_cmap
