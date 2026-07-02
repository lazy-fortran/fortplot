module fortplot_ascii_polar
    !! Text-backend polar data compositing (issue #2072).
    !!
    !! Draws polar curve samples directly onto the ASCII canvas as single-glyph
    !! marks, clipped to the circular frame and kept clear of the radial-label
    !! corridor. Using the layer policy (#2069) means angular and radial labels,
    !! stamped last as text elements, always win the cells they need while the
    !! curve stays a readable outline instead of a dense character mass.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_ascii, only: ascii_context
    use fortplot_margins, only: plot_area_t
    use fortplot_polar, only: PI
    use fortplot_ascii_axis_policy, only: put_cell, LAYER_DATA
    use fortplot_polar_text_layout, only: polar_frame_t, polar_to_text_cell, &
                                          inside_polar_frame, reserve_label_cells, &
                                          can_place_data
    use fortplot_tick_calculation, only: calculate_tick_labels
    implicit none

    private
    public :: render_polar_data_text

    type :: cell_map_t
        !! Data-to-cell mapping matching the ASCII line/marker primitives so the
        !! curve stays aligned with the boundary and labels.
        type(plot_area_t) :: plot_area
        integer :: plot_width = 0
        integer :: plot_height = 0
        real(wp) :: x_min = 0.0_wp, x_max = 1.0_wp
        real(wp) :: y_min = 0.0_wp, y_max = 1.0_wp
    end type cell_map_t

contains

    subroutine render_polar_data_text(ctx, theta, r, n, center_x, center_y, &
                                      radius, r_max, theta_offset, clockwise, &
                                      x_min, x_max, y_min, y_max, glyph)
        !! Composite one polar series onto the text canvas.
        type(ascii_context), intent(inout) :: ctx
        real(wp), contiguous, intent(in) :: theta(:), r(:)
        integer, intent(in) :: n
        real(wp), intent(in) :: center_x, center_y, radius, r_max
        real(wp), intent(in) :: theta_offset
        logical, intent(in) :: clockwise
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=1), intent(in) :: glyph

        type(cell_map_t) :: cmap
        type(polar_frame_t) :: frame
        logical, allocatable :: reserved(:, :)
        integer :: i, row, col

        if (n < 1) return
        if (.not. allocated(ctx%canvas)) return
        if (r_max <= 0.0_wp) return

        cmap%plot_area = ctx%plot_area
        cmap%plot_width = ctx%plot_width
        cmap%plot_height = ctx%plot_height
        cmap%x_min = x_min
        cmap%x_max = x_max
        cmap%y_min = y_min
        cmap%y_max = y_max

        call build_frame(cmap, center_x, center_y, radius, frame)
        if (frame%radius_cols <= 0) return
        if (frame%radius_rows <= 0) return

        allocate (reserved(size(ctx%canvas, 1), size(ctx%canvas, 2)))
        reserved = .false.
        call reserve_radial_corridor(cmap, reserved, center_x, center_y, radius, &
                                     r_max)

        do i = 1, n
            call polar_to_text_cell(frame, theta(i), r(i), r_max, theta_offset, &
                                    clockwise, row, col)
            if (.not. inside_polar_frame(frame, row, col)) cycle
            if (.not. can_place_data(reserved, row, col)) cycle
            ! Keep an earlier series' glyph so overlapping curves stay
            ! distinguishable instead of the last one masking the rest.
            if (is_series_glyph(cell_glyph(ctx%canvas, row, col))) cycle
            call put_cell(ctx%canvas, row, col, glyph, LAYER_DATA)
        end do
    end subroutine render_polar_data_text

    pure character(len=1) function cell_glyph(canvas, row, col) result(glyph)
        character(len=1), intent(in) :: canvas(:, :)
        integer, intent(in) :: row, col

        glyph = ' '
        if (row < 1) return
        if (row > size(canvas, 1)) return
        if (col < 1) return
        if (col > size(canvas, 2)) return
        glyph = canvas(row, col)
    end function cell_glyph

    pure logical function is_series_glyph(glyph) result(is_glyph)
        character(len=1), intent(in) :: glyph

        is_glyph = glyph == 'o' .or. glyph == '#' .or. glyph == '*' &
                   .or. glyph == '%'
    end function is_series_glyph

    subroutine build_frame(cmap, center_x, center_y, radius, frame)
        !! Derive the circular frame in canvas cells from the data-space center
        !! and radius using the same mapping as the ASCII line primitive.
        type(cell_map_t), intent(in) :: cmap
        real(wp), intent(in) :: center_x, center_y, radius
        type(polar_frame_t), intent(out) :: frame

        integer :: col_east, row_north

        call data_to_cell(cmap, center_x, center_y, frame%center_row, &
                         frame%center_col)
        call data_to_cell(cmap, center_x + radius, center_y, row_north, col_east)
        frame%radius_cols = col_east - frame%center_col
        call data_to_cell(cmap, center_x, center_y + radius, row_north, col_east)
        frame%radius_rows = frame%center_row - row_north
    end subroutine build_frame

    subroutine reserve_radial_corridor(cmap, reserved, center_x, center_y, radius, &
                                       r_max)
        !! Reserve the 22.5-degree ray that carries the radial tick labels so
        !! curve glyphs leave room for them (matplotlib's rlabel position).
        type(cell_map_t), intent(in) :: cmap
        logical, intent(inout) :: reserved(:, :)
        real(wp), intent(in) :: center_x, center_y, radius, r_max

        real(wp), parameter :: label_angle = PI/8.0_wp
        real(wp), parameter :: r_max_pad = 1.1_wp
        real(wp), parameter :: label_x_shift = 0.04_wp
        character(len=20) :: labels(12)
        integer :: i, ios, row, col
        real(wp) :: r_value, r_geom, r_data, rx, ry

        if (r_max <= 0.0_wp) return

        r_data = r_max/r_max_pad
        labels = ''
        call calculate_tick_labels(0.0_wp, r_data, size(labels), labels)

        do i = 1, size(labels)
            if (len_trim(labels(i)) == 0) cycle
            read (labels(i), *, iostat=ios) r_value
            if (ios /= 0) cycle
            if (r_value <= 0.0_wp) cycle
            if (r_value > r_data + 1.0e-9_wp) cycle

            r_geom = radius*(r_value/r_max)
            rx = center_x + r_geom*cos(label_angle) - radius*label_x_shift
            ry = center_y + r_geom*sin(label_angle)
            call data_to_text_cell(cmap, rx, ry, row, col)
            call reserve_label_cells(reserved, row, col, len_trim(labels(i)) + 3, 1)
        end do
    end subroutine reserve_radial_corridor

    pure subroutine data_to_text_cell(cmap, x, y, row, col)
        type(cell_map_t), intent(in) :: cmap
        real(wp), intent(in) :: x, y
        integer, intent(out) :: row, col

        real(wp) :: fx, fy

        fx = (x - cmap%x_min)/(cmap%x_max - cmap%x_min)
        fy = (y - cmap%y_min)/(cmap%y_max - cmap%y_min)

        if (cmap%plot_area%width > 0 .and. cmap%plot_area%height > 0) then
            col = cmap%plot_area%left + nint(fx*real(max(1, cmap%plot_area%width), wp))
            row = cmap%plot_area%bottom + cmap%plot_area%height - &
                  nint(fy*real(max(1, cmap%plot_area%height), wp))
            col = max(cmap%plot_area%left + 1, &
                      min(col, cmap%plot_area%left + max(1, cmap%plot_area%width) - 1))
            row = max(cmap%plot_area%bottom + 1, &
                      min(row, cmap%plot_area%bottom + max(1, cmap%plot_area%height) - 1))
        else
            col = nint(fx*real(cmap%plot_width, wp))
            row = nint((1.0_wp - fy)*real(cmap%plot_height, wp))
            col = max(2, min(col, max(2, cmap%plot_width - 1)))
            row = max(1, min(row, cmap%plot_height))
        end if
    end subroutine data_to_text_cell

    pure subroutine data_to_cell(cmap, x, y, row, col)
        !! Map a data coordinate to a canvas cell. Uses the plot-area mapping
        !! when a plot area is configured, otherwise the width/height fallback,
        !! matching ascii_draw_line_primitive so glyphs align with the frame.
        type(cell_map_t), intent(in) :: cmap
        real(wp), intent(in) :: x, y
        integer, intent(out) :: row, col

        integer :: inner_width, inner_height
        real(wp) :: fx, fy

        fx = (x - cmap%x_min)/(cmap%x_max - cmap%x_min)
        fy = (y - cmap%y_min)/(cmap%y_max - cmap%y_min)

        if (cmap%plot_area%width > 0 .and. cmap%plot_area%height > 0) then
            inner_width = max(1, cmap%plot_area%width - 2)
            inner_height = max(1, cmap%plot_area%height - 2)
            col = cmap%plot_area%left + 1 + nint(fx*real(inner_width, wp))
            row = cmap%plot_area%bottom + cmap%plot_area%height - 1 - &
                  nint(fy*real(inner_height, wp))
        else
            col = int(fx*real(cmap%plot_width - 3, wp)) + 2
            row = (cmap%plot_height - 1) - int(fy*real(cmap%plot_height - 3, wp))
        end if
    end subroutine data_to_cell

end module fortplot_ascii_polar
