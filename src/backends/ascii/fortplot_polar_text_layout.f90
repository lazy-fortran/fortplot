module fortplot_polar_text_layout
    !! Text-backend polar layout policy (issue #2072).
    !!
    !! Character terminals cannot overlay labels, spokes, and dense curve data
    !! the way a raster backend can, so the text backend needs a deterministic
    !! cell layout for polar plots. This module owns the geometry that keeps
    !! angular/radial labels and legend text off the plotted glyphs:
    !!
    !! - ``polar_to_text_cell`` maps a (theta, r) sample to an integer canvas
    !!   cell, keeping Matplotlib's orientation (0 rad at east, angle increasing
    !!   counter-clockwise) as the geometry oracle.
    !! - ``inside_polar_frame`` clips samples to the drawn circular frame so the
    !!   rose never spills into the label ring outside the boundary.
    !! - ``reserve_label_cells``/``can_place_data`` mark and query cells that
    !!   labels own, so curve glyphs are suppressed there rather than crowding
    !!   the labels (gnuplot ``dumb`` and plotext do the same for terminals).

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_polar, only: polar_to_cartesian
    implicit none

    private
    public :: polar_frame_t
    public :: polar_to_text_cell, inside_polar_frame
    public :: reserve_label_cells, can_place_data

    type :: polar_frame_t
        !! Circular frame expressed in canvas cells. ``radius_cols`` and
        !! ``radius_rows`` differ because a character cell is taller than wide.
        integer :: center_row = 0
        integer :: center_col = 0
        integer :: radius_rows = 0
        integer :: radius_cols = 0
    end type polar_frame_t

contains

    pure subroutine polar_to_text_cell(frame, theta, r, r_max, theta_offset, &
                                       clockwise, row, col)
        !! Map a polar sample to a canvas cell. ``r_max`` is the shared radial
        !! axis maximum so ``r == r_max`` lands on the circular boundary.
        type(polar_frame_t), intent(in) :: frame
        real(wp), intent(in) :: theta, r, r_max, theta_offset
        logical, intent(in) :: clockwise
        integer, intent(out) :: row, col

        real(wp) :: rr, x, y

        rr = 0.0_wp
        if (r_max > 0.0_wp) rr = r/r_max
        call polar_to_cartesian(theta, rr, x, y, theta_offset, clockwise)
        col = frame%center_col + nint(x*real(frame%radius_cols, wp))
        row = frame%center_row - nint(y*real(frame%radius_rows, wp))
    end subroutine polar_to_text_cell

    pure logical function inside_polar_frame(frame, row, col) result(inside)
        !! True when the cell lies within the circular frame (elliptical in
        !! cell space because of the character aspect ratio).
        type(polar_frame_t), intent(in) :: frame
        integer, intent(in) :: row, col

        real(wp) :: nx, ny

        inside = .false.
        if (frame%radius_cols <= 0) return
        if (frame%radius_rows <= 0) return
        nx = real(col - frame%center_col, wp)/real(frame%radius_cols, wp)
        ny = real(row - frame%center_row, wp)/real(frame%radius_rows, wp)
        inside = (nx*nx + ny*ny) <= 1.0_wp + 1.0e-9_wp
    end function inside_polar_frame

    pure subroutine reserve_label_cells(reserved, row, col, span, halo)
        !! Reserve a label footprint of ``span`` cells starting at (row, col)
        !! plus a ``halo`` border, so plotted data is kept clear of the label.
        logical, intent(inout) :: reserved(:, :)
        integer, intent(in) :: row, col, span, halo
        integer :: r, c

        do r = row - halo, row + halo
            if (r < 1) cycle
            if (r > size(reserved, 1)) cycle
            do c = col - halo, col + span - 1 + halo
                if (c < 1) cycle
                if (c > size(reserved, 2)) cycle
                reserved(r, c) = .true.
            end do
        end do
    end subroutine reserve_label_cells

    pure logical function can_place_data(reserved, row, col) result(ok)
        !! True when a data glyph may occupy the cell (in bounds and not
        !! reserved for a label).
        logical, intent(in) :: reserved(:, :)
        integer, intent(in) :: row, col

        ok = .false.
        if (row < 1) return
        if (row > size(reserved, 1)) return
        if (col < 1) return
        if (col > size(reserved, 2)) return
        ok = .not. reserved(row, col)
    end function can_place_data

end module fortplot_polar_text_layout
