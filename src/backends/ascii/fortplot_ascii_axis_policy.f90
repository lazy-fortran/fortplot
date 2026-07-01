module fortplot_ascii_axis_policy
    !! Text-backend axis rendering policy (issue #2069).
    !!
    !! Defines the single source of truth for how the text/ASCII backend draws
    !! axis spines, tick marks, and how cells are resolved when several plot
    !! layers land on the same character cell.
    !!
    !! Adopted policy (compared against gnuplot ``set terminal dumb``, plotext,
    !! and UnicodePlots.jl):
    !!
    !! - Axis spines are SOLID: a continuous left ``|`` spine spans the data
    !!   rows and a continuous bottom ``-`` spine spans the data columns. Spines
    !!   are never drawn by sampling in data space, so they never degrade into a
    !!   dashed data-like line.
    !! - Tick marks (``+``) are drawn only at labeled (major) tick positions on
    !!   the spine. No minor/unlabeled tick marks and no grid lines are drawn by
    !!   default in text output.
    !! - Cell priority is ordered by layer: grid < data < axis < tick < label.
    !!   A higher layer overwrites a lower one; equal layers overwrite so the
    !!   most recently drawn glyph wins. Tick/axis labels (highest layer) are
    !!   rendered last and are therefore never overwritten by plot data.
    !!
    !! Author: fortplot contributors

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: text_cell_t
    public :: LAYER_EMPTY, LAYER_GRID, LAYER_DATA, LAYER_AXIS, LAYER_TICK, LAYER_LABEL
    public :: put_cell, glyph_layer, map_ticks_to_cells

    integer, parameter :: LAYER_EMPTY = 0
    integer, parameter :: LAYER_GRID = 10
    integer, parameter :: LAYER_DATA = 20
    integer, parameter :: LAYER_AXIS = 30
    integer, parameter :: LAYER_TICK = 40
    integer, parameter :: LAYER_LABEL = 50

    type :: text_cell_t
        integer :: layer = LAYER_EMPTY
        character(len=1) :: glyph = ' '
    end type text_cell_t

contains

    pure integer function glyph_layer(glyph) result(layer)
        !! Classify a character already present on the canvas into a layer so
        !! put_cell can resolve collisions without a parallel layer buffer.
        character(len=1), intent(in) :: glyph

        select case (glyph)
        case (' ')
            layer = LAYER_EMPTY
        case ('|', '-')
            layer = LAYER_AXIS
        case ('+')
            layer = LAYER_TICK
        case ('.', ':', '=', '*', '#', '%', '@', 'o')
            layer = LAYER_DATA
        case default
            ! Digits, letters and punctuation belong to tick/axis labels.
            layer = LAYER_LABEL
        end select
    end function glyph_layer

    subroutine put_cell(canvas, row, col, glyph, layer)
        !! Write glyph into the canvas only when its layer is at least the layer
        !! of the glyph already occupying the cell (higher or equal layer wins).
        character(len=1), intent(inout) :: canvas(:, :)
        integer, intent(in) :: row, col
        character(len=1), intent(in) :: glyph
        integer, intent(in) :: layer

        if (row < 1 .or. row > size(canvas, 1)) return
        if (col < 1 .or. col > size(canvas, 2)) return
        if (layer >= glyph_layer(canvas(row, col))) then
            canvas(row, col) = glyph
        end if
    end subroutine put_cell

    pure subroutine map_ticks_to_cells(tick_values, num_ticks, v_min, v_max, &
                                       cell_lo, cell_hi, cells, in_range)
        !! Map axis tick data values to integer screen cells along one axis.
        !! ``in_range`` flags the ticks that fall inside [cell_lo, cell_hi] and
        !! are therefore drawable; callers decide which are labeled.
        real(wp), intent(in) :: tick_values(:)
        integer, intent(in) :: num_ticks
        real(wp), intent(in) :: v_min, v_max
        integer, intent(in) :: cell_lo, cell_hi
        integer, intent(out) :: cells(:)
        logical, intent(out) :: in_range(:)

        integer :: i, n, span
        real(wp) :: frac

        n = min(num_ticks, size(cells))
        n = min(n, size(in_range))
        cells = cell_lo
        in_range = .false.
        span = cell_hi - cell_lo
        if (span <= 0 .or. v_max <= v_min) return

        do i = 1, n
            frac = (tick_values(i) - v_min)/(v_max - v_min)
            cells(i) = cell_lo + nint(frac*real(span, wp))
            in_range(i) = cells(i) >= cell_lo .and. cells(i) <= cell_hi
        end do
    end subroutine map_ticks_to_cells

end module fortplot_ascii_axis_policy
