module fortplot_braille
    !! Braille subpixel canvas for the text backend.
    !!
    !! A braille canvas maps each terminal character cell to a 2-by-4 grid of
    !! dots and encodes the set dots as a Unicode braille codepoint
    !! (U+2800 + bitmask). This gives line and scatter data glyphs four times
    !! the vertical and twice the horizontal resolution of plain ASCII cells,
    !! while axes, ticks, and labels stay ordinary text.
    !!
    !! Dot-bit layout matches Drawille's pixel map:
    !!     column 0 (left):  dot1=0x01 dot2=0x02 dot3=0x04 dot7=0x40
    !!     column 1 (right): dot4=0x08 dot5=0x10 dot6=0x20 dot8=0x80

    implicit none

    private
    public :: braille_canvas_t, create_braille_canvas
    public :: braille_dot_bit, set_braille_pixel, braille_draw_line
    public :: braille_char, braille_codepoint, braille_mask_valid

    integer, parameter :: BRAILLE_BASE = int(z'2800')
    integer, parameter :: MASK_MIN = 0
    integer, parameter :: MASK_MAX = 255

    type :: braille_canvas_t
        integer :: n_cols = 0
        integer :: n_rows = 0
        integer :: sub_w = 0
        integer :: sub_h = 0
        integer, allocatable :: mask(:, :)
    end type braille_canvas_t

contains

    function create_braille_canvas(n_cols, n_rows) result(canvas)
        !! Build a blank braille canvas sized to a text grid of n_cols by n_rows
        !! character cells. Subpixel resolution is 2*n_cols by 4*n_rows dots.
        integer, intent(in) :: n_cols, n_rows
        type(braille_canvas_t) :: canvas

        canvas%n_cols = max(1, n_cols)
        canvas%n_rows = max(1, n_rows)
        canvas%sub_w = 2 * canvas%n_cols
        canvas%sub_h = 4 * canvas%n_rows
        allocate (canvas%mask(canvas%n_rows, canvas%n_cols))
        canvas%mask = 0
    end function create_braille_canvas

    pure integer function braille_dot_bit(local_x, local_y) result(bit)
        !! Bit value for one dot within a cell. local_x in {0,1} (left/right),
        !! local_y in {0,1,2,3} (top to bottom). Out-of-range positions have no
        !! dot and return 0 so an invalid position never sets a spurious bit.
        integer, intent(in) :: local_x, local_y

        integer, parameter :: dot_bits(0:1, 0:3) = reshape( &
            [int(z'01'), int(z'08'), &
            int(z'02'), int(z'10'), &
            int(z'04'), int(z'20'), &
            int(z'40'), int(z'80')], [2, 4])

        if (local_x < 0 .or. local_x > 1) then
            bit = 0
            return
        end if
        if (local_y < 0 .or. local_y > 3) then
            bit = 0
            return
        end if
        bit = dot_bits(local_x, local_y)
    end function braille_dot_bit

    subroutine set_braille_pixel(canvas, x_sub, y_sub)
        !! Turn on the subpixel dot at absolute subpixel coordinates
        !! (x_sub in [0, sub_w), y_sub in [0, sub_h)). Coordinates outside the
        !! canvas are ignored, so callers never corrupt an out-of-range cell.
        type(braille_canvas_t), intent(inout) :: canvas
        integer, intent(in) :: x_sub, y_sub
        integer :: cell_x, cell_y, local_x, local_y, bit

        if (x_sub < 0 .or. x_sub >= canvas%sub_w) return
        if (y_sub < 0 .or. y_sub >= canvas%sub_h) return

        cell_x = x_sub / 2 + 1
        cell_y = y_sub / 4 + 1
        local_x = mod(x_sub, 2)
        local_y = mod(y_sub, 4)
        bit = braille_dot_bit(local_x, local_y)
        canvas%mask(cell_y, cell_x) = ior(canvas%mask(cell_y, cell_x), bit)
    end subroutine set_braille_pixel

    subroutine braille_draw_line(canvas, x0, y0, x1, y1)
        !! Rasterize a line between two subpixel coordinates with Bresenham's
        !! algorithm, setting every visited dot.
        type(braille_canvas_t), intent(inout) :: canvas
        integer, intent(in) :: x0, y0, x1, y1
        integer :: x, y, dx, dy, sx, sy, err, e2

        x = x0
        y = y0
        dx = abs(x1 - x0)
        dy = -abs(y1 - y0)
        sx = merge(1, -1, x0 < x1)
        sy = merge(1, -1, y0 < y1)
        err = dx + dy

        do
            call set_braille_pixel(canvas, x, y)
            if (x == x1 .and. y == y1) exit
            e2 = 2 * err
            if (e2 >= dy) then
                if (x == x1) exit
                err = err + dy
                x = x + sx
            end if
            if (e2 <= dx) then
                if (y == y1) exit
                err = err + dx
                y = y + sy
            end if
        end do
    end subroutine braille_draw_line

    pure logical function braille_mask_valid(mask) result(valid)
        !! A mask is a valid braille bitmask only within [0, 255]; any other
        !! value carries dot bits that do not exist in the U+2800 block.
        integer, intent(in) :: mask

        valid = (mask >= MASK_MIN) .and. (mask <= MASK_MAX)
    end function braille_mask_valid

    pure integer function braille_codepoint(mask) result(cp)
        !! Codepoint for a braille mask, clamped into the valid block so an
        !! invalid mask can never encode outside U+2800..U+28FF.
        integer, intent(in) :: mask

        cp = BRAILLE_BASE + iand(max(mask, 0), MASK_MAX)
    end function braille_codepoint

    pure function braille_char(mask) result(utf8)
        !! UTF-8 encoding of the braille codepoint for a mask. Invalid masks are
        !! clamped before encoding (braille_mask_valid reports the raw value).
        integer, intent(in) :: mask
        character(len=3) :: utf8
        integer :: cp

        cp = braille_codepoint(mask)
        utf8(1:1) = achar(ior(int(z'E0'), ishft(cp, -12)))
        utf8(2:2) = achar(ior(int(z'80'), iand(ishft(cp, -6), int(z'3F'))))
        utf8(3:3) = achar(ior(int(z'80'), iand(cp, int(z'3F'))))
    end function braille_char

end module fortplot_braille
