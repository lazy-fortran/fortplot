module fortplot_figure_colorbar
    !! Stateful colorbar rendering (matplotlib-style).
    !!
    !! Implements:
    !! - Plot-area splitting for right/left/top/bottom colorbar placement
    !! - Scalar-mappable detection (pcolormesh/scatter/contour)
    !! - Gradient rendering + ticks/labels using existing primitives

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_PCOLORMESH, &
        PLOT_TYPE_SCATTER, &
        PLOT_TYPE_CONTOUR
    use fortplot_margins, only: plot_area_t
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_colormap, only: get_colormap_color
    use fortplot_figure_contour_colorbar, only: render_contour_colorbar_lines, &
        contour_colorbar_tick_positions, contour_colorbar_default_ticks, &
        get_contour_colorbar_levels, format_contour_colorbar_tick
    use fortplot_ticks, only: find_nice_tick_locations, format_tick_value_smart
    use fortplot_tick_calculation, only: determine_decimals_from_ticks, &
        format_tick_value_consistent
    use fortplot_string_utils, only: to_lowercase
    use fortplot_raster_core, only: pt2px
    implicit none

    private
    public :: prepare_colorbar_layout
    public :: resolve_colorbar_mappable
    public :: render_colorbar
    public :: colorbar_auto_tick_labels, colorbar_tick_target

contains

    subroutine prepare_colorbar_layout(backend, location, fraction, pad, shrink, &
            saved_area, main_area, colorbar_area, supported)
        class(plot_context), intent(inout) :: backend
        character(len=*), intent(in) :: location
        real(wp), intent(in) :: fraction, pad, shrink
        type(plot_area_t), intent(out) :: saved_area, main_area, colorbar_area
        logical, intent(out) :: supported
        character(len=32) :: loc

        supported = .false.

        call get_backend_plot_area(backend, saved_area, supported)
        if (.not. supported) then
            main_area = saved_area
            colorbar_area = saved_area
            return
        end if

        call compute_colorbar_plot_areas(saved_area, location, fraction, pad, shrink, &
            main_area, colorbar_area)
        select type (backend)
            type is (png_context)
            loc = to_lowercase(trim(location))
            if (loc == 'top' .or. loc == 'bottom') then
                ! Raster areas start at the top; the shared layout uses upward Y.
                main_area%bottom = saved_area%bottom + saved_area%height - &
                    (main_area%bottom - saved_area%bottom) - main_area%height
                colorbar_area%bottom = saved_area%bottom + saved_area%height - &
                    (colorbar_area%bottom - saved_area%bottom) - colorbar_area%height
            end if
        end select
        call set_backend_plot_area(backend, main_area)
    end subroutine prepare_colorbar_layout

    subroutine resolve_colorbar_mappable(plots, plot_count, preferred_index, &
            plot_index, vmin, vmax, colormap, ok)
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        integer, intent(in) :: preferred_index
        integer, intent(out) :: plot_index
        real(wp), intent(out) :: vmin, vmax
        character(len=20), intent(out) :: colormap
        logical, intent(out) :: ok

        integer :: i, start_idx
        logical :: found
        real(wp), allocatable :: line_levels(:)

        ok = .false.
        plot_index = 0
        vmin = 0.0_wp
        vmax = 1.0_wp
        colormap = 'viridis'

        if (plot_count <= 0) return

        start_idx = preferred_index
        if (start_idx < 1 .or. start_idx > plot_count) start_idx = plot_count

        found = .false.
        do i = start_idx, 1, -1
            if (plots(i)%plot_type == PLOT_TYPE_PCOLORMESH) then
                if (allocated(plots(i)%pcolormesh_data%c_values)) then
                    vmin = plots(i)%pcolormesh_data%vmin
                    vmax = plots(i)%pcolormesh_data%vmax
                    if (.not. plots(i)%pcolormesh_data%vmin_set) vmin = &
                        minval(plots(i)%pcolormesh_data%c_values)
                    if (.not. plots(i)%pcolormesh_data%vmax_set) vmax = &
                        maxval(plots(i)%pcolormesh_data%c_values)
                    colormap = plots(i)%pcolormesh_data%colormap_name
                    plot_index = i
                    found = .true.
                    exit
                end if
            end if

            if (plots(i)%plot_type == PLOT_TYPE_SCATTER) then
                if (allocated(plots(i)%scatter_colors)) then
                    if (size(plots(i)%scatter_colors) > 0) then
                        if (plots(i)%scatter_vrange_set) then
                            vmin = plots(i)%scatter_vmin
                            vmax = plots(i)%scatter_vmax
                        else
                            vmin = minval(plots(i)%scatter_colors)
                            vmax = maxval(plots(i)%scatter_colors)
                        end if
                        colormap = plots(i)%scatter_colormap
                        plot_index = i
                        found = .true.
                        exit
                    end if
                end if
            end if

            if (plots(i)%plot_type == PLOT_TYPE_CONTOUR) then
                if (allocated(plots(i)%z_grid)) then
                    if (size(plots(i)%z_grid) > 0) then
                        ! Contour colors normalize over the level range, rather
                        ! than the raw data range. Fall back
                        ! to the data range only when no levels are stored.
                        if (.not. plots(i)%fill_contours) then
                            call get_contour_colorbar_levels(plots(i), line_levels)
                            if (size(line_levels) == 0) cycle
                            vmin = minval(line_levels)
                            vmax = maxval(line_levels)
                        else if (allocated(plots(i)%contour_levels)) then
                            if (size(plots(i)%contour_levels) == 0) cycle
                            vmin = minval(plots(i)%contour_levels)
                            vmax = maxval(plots(i)%contour_levels)
                        else
                            vmin = minval(plots(i)%z_grid)
                            vmax = maxval(plots(i)%z_grid)
                        end if
                        colormap = plots(i)%colormap
                        plot_index = i
                        found = .true.
                        exit
                    end if
                end if
            end if
        end do

        if (.not. found) return
        if (vmax <= vmin) vmax = vmin + 1.0_wp
        ok = .true.
    end subroutine resolve_colorbar_mappable

    subroutine render_colorbar(backend, plot_area, vmin, vmax, colormap, &
            location, label, custom_ticks, custom_ticklabels, &
            label_fontsize, line_levels, line_width)
        class(plot_context), intent(inout) :: backend
        type(plot_area_t), intent(in) :: plot_area
        real(wp), intent(in) :: vmin, vmax
        character(len=*), intent(in) :: colormap
        character(len=*), intent(in) :: location
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: custom_ticks(:)
        character(len=*), intent(in), optional :: custom_ticklabels(:)
        real(wp), intent(in), optional :: label_fontsize
        real(wp), intent(in), optional :: line_levels(:), line_width

        type(plot_area_t) :: saved_area
        logical :: supported
        character(len=32) :: loc
        logical :: vertical
        real(wp) :: range_val
        logical :: use_custom_ticks, use_custom_labels

        supported = .false.
        call get_backend_plot_area(backend, saved_area, supported)
        if (.not. supported) return

        loc = to_lowercase(trim(location))
        vertical = .true.
        if (loc == 'top' .or. loc == 'bottom') vertical = .false.

        range_val = max(1.0e-12_wp, vmax - vmin)

        use_custom_ticks = .false.
        if (present(custom_ticks)) use_custom_ticks = size(custom_ticks) > 0
        use_custom_labels = .false.
        if (present(custom_ticklabels)) then
            use_custom_labels = size(custom_ticklabels) > 0
        end if

        call render_colorbar_with_context(backend, plot_area, vertical, vmin, &
            vmax, range_val, colormap, &
            use_custom_ticks, use_custom_labels, &
            custom_ticks, custom_ticklabels, &
            label, label_fontsize, saved_area, line_levels, line_width)
    end subroutine render_colorbar

    subroutine compute_colorbar_plot_areas(orig, location, fraction, pad, &
            shrink, main, cb)
        type(plot_area_t), intent(in) :: orig
        character(len=*), intent(in) :: location
        real(wp), intent(in) :: fraction, pad, shrink
        type(plot_area_t), intent(out) :: main, cb

        character(len=32) :: loc
        logical :: vertical
        integer :: bar_px, region_px, pad_px
        integer :: long_px, shrink_px, delta_px

        main = orig
        cb = orig

        loc = to_lowercase(trim(location))
        vertical = .true.
        if (loc == 'top' .or. loc == 'bottom') vertical = .false.

        if (vertical) then
            ! fraction is the share of the original axes reserved for the
            ! colorbar region (bar + pad), not the bar width itself.
            region_px = max(1, int(max(0.01_wp, fraction)*real(orig%width, wp)))
            pad_px = max(0, int(max(0.0_wp, pad)*real(orig%width, wp)))

            main%width = max(1, orig%width - region_px - pad_px)

            long_px = max(1, orig%height)
            shrink_px = max(1, int(max(0.05_wp, min(1.0_wp, shrink))*real(long_px, wp)))
            delta_px = (long_px - shrink_px)/2
            cb%height = shrink_px
            cb%bottom = orig%bottom + delta_px

            ! matplotlib aspect=20: the slender bar width is the bar length
            ! divided by the aspect ratio, not the full reserved region.
            bar_px = bar_width_from_aspect(shrink_px, region_px)
            cb%width = bar_px

            if (loc == 'left') then
                cb%left = orig%left
                main%left = orig%left + region_px + pad_px
            else
                main%left = orig%left
                cb%left = orig%left + main%width + pad_px
            end if
        else
            region_px = max(1, int(max(0.01_wp, fraction)*real(orig%height, wp)))
            pad_px = max(0, int(max(0.0_wp, pad)*real(orig%height, wp)))

            main%height = max(1, orig%height - region_px - pad_px)

            long_px = max(1, orig%width)
            shrink_px = max(1, int(max(0.05_wp, min(1.0_wp, shrink))*real(long_px, wp)))
            delta_px = (long_px - shrink_px)/2
            cb%width = shrink_px
            cb%left = orig%left + delta_px

            bar_px = bar_width_from_aspect(shrink_px, region_px)
            cb%height = bar_px

            if (loc == 'bottom') then
                cb%bottom = orig%bottom
                main%bottom = orig%bottom + region_px + pad_px
            else
                main%bottom = orig%bottom
                cb%bottom = orig%bottom + main%height + pad_px
            end if
        end if
    end subroutine compute_colorbar_plot_areas

    pure integer function bar_width_from_aspect(bar_length_px, region_px) &
            result(bar_px)
        !! Slender colorbar bar width from matplotlib aspect=20: width is the
        !! bar length over the aspect ratio, clamped to the reserved region.
        integer, intent(in) :: bar_length_px, region_px
        integer, parameter :: aspect = 20

        bar_px = max(1, nint(real(bar_length_px, wp)/real(aspect, wp)))
        bar_px = min(bar_px, max(1, region_px))
    end function bar_width_from_aspect

    subroutine get_backend_plot_area(backend, plot_area, supported)
        class(plot_context), intent(in) :: backend
        type(plot_area_t), intent(out) :: plot_area
        logical, intent(out) :: supported

        supported = .false.
        plot_area%left = 0
        plot_area%bottom = 0
        plot_area%width = 0
        plot_area%height = 0

        select type (bk => backend)
            type is (png_context)
            plot_area = bk%plot_area
            supported = .true.
            type is (pdf_context)
            plot_area = bk%plot_area
            supported = .true.
        class default
            supported = .false.
        end select
    end subroutine get_backend_plot_area

    subroutine set_backend_plot_area(backend, plot_area)
        class(plot_context), intent(inout) :: backend
        type(plot_area_t), intent(in) :: plot_area

        select type (bk => backend)
            type is (png_context)
            bk%plot_area = plot_area
            type is (pdf_context)
            bk%plot_area = plot_area
        class default
            continue
        end select
    end subroutine set_backend_plot_area

    subroutine render_colorbar_gradient(backend, vertical, vmin, vmax, range_val, &
            colormap, plot_area)
        class(plot_context), intent(inout) :: backend
        logical, intent(in) :: vertical
        real(wp), intent(in) :: vmin, vmax, range_val
        character(len=*), intent(in) :: colormap
        type(plot_area_t), intent(in) :: plot_area

        integer :: n_slices, i
        real(wp) :: t, c(3)
        real(wp) :: x0, x1, y0, y1
        real(wp) :: quad_x(4), quad_y(4)

        if (vertical) then
            call backend%set_coordinates(0.0_wp, 1.0_wp, vmin, vmax)
            n_slices = min(128, max(32, plot_area%height/4))
        else
            call backend%set_coordinates(vmin, vmax, 0.0_wp, 1.0_wp)
            n_slices = min(128, max(32, plot_area%width/4))
        end if

        do i = 1, n_slices
            if (n_slices == 1) then
                t = 0.5_wp
            else
                t = real(i - 1, wp)/real(n_slices - 1, wp)
            end if
            call get_colormap_color(t, colormap, c)
            call backend%color(c(1), c(2), c(3))

            if (vertical) then
                x0 = 0.0_wp
                x1 = 1.0_wp
                y0 = vmin + (real(i - 1, wp)/real(n_slices, wp))*range_val
                y1 = vmin + (real(i, wp)/real(n_slices, wp))*range_val
                quad_x = [x0, x1, x1, x0]
                quad_y = [y0, y0, y1, y1]
            else
                y0 = 0.0_wp
                y1 = 1.0_wp
                x0 = vmin + (real(i - 1, wp)/real(n_slices, wp))*range_val
                x1 = vmin + (real(i, wp)/real(n_slices, wp))*range_val
                quad_x = [x0, x1, x1, x0]
                quad_y = [y0, y0, y1, y1]
            end if

            call backend%fill_quad(quad_x, quad_y)
        end do
    end subroutine render_colorbar_gradient

    subroutine render_colorbar_border(backend, vertical, vmin, vmax)
        class(plot_context), intent(inout) :: backend
        logical, intent(in) :: vertical
        real(wp), intent(in) :: vmin, vmax

        ! 0.8pt matches the axes frame width (matplotlib axes.linewidth
        ! default) so the colorbar outline keeps the same stroke weight as
        ! the surrounding axes box instead of the backend default.
        call backend%set_line_width(0.8_wp)
        call backend%color(0.0_wp, 0.0_wp, 0.0_wp)
        if (vertical) then
            call backend%line(0.0_wp, vmin, 1.0_wp, vmin)
            call backend%line(0.0_wp, vmax, 1.0_wp, vmax)
            call backend%line(0.0_wp, vmin, 0.0_wp, vmax)
            call backend%line(1.0_wp, vmin, 1.0_wp, vmax)
        else
            call backend%line(vmin, 0.0_wp, vmax, 0.0_wp)
            call backend%line(vmin, 1.0_wp, vmax, 1.0_wp)
            call backend%line(vmin, 0.0_wp, vmin, 1.0_wp)
            call backend%line(vmax, 0.0_wp, vmax, 1.0_wp)
        end if
    end subroutine render_colorbar_border

    subroutine render_colorbar_custom_ticks(backend, vertical, vmin, vmax, &
            custom_ticks, custom_ticklabels, &
            use_custom_labels, tick_positions)
        class(plot_context), intent(inout) :: backend
        logical, intent(in) :: vertical
        real(wp), intent(in) :: vmin, vmax
        real(wp), contiguous, intent(in) :: custom_ticks(:)
        character(len=*), intent(in), optional :: custom_ticklabels(:)
        logical, intent(in) :: use_custom_labels
        real(wp), intent(in), optional :: tick_positions(:)

        real(wp) :: tick_len
        integer :: n_custom_ticks, i
        real(wp) :: tick, position
        character(len=50) :: tick_label

        tick_len = 0.08_wp
        n_custom_ticks = size(custom_ticks)
        do i = 1, n_custom_ticks
            tick = custom_ticks(i)
            if (tick < vmin .or. tick > vmax) cycle

            tick_label = format_tick_value_smart(tick, 10)
            if (present(tick_positions)) &
                tick_label = format_contour_colorbar_tick(tick, custom_ticks)
            if (use_custom_labels) then
                if (i <= size(custom_ticklabels)) &
                    tick_label = trim(custom_ticklabels(i))
            end if
            position = tick
            if (present(tick_positions)) position = tick_positions(i)

            if (vertical) then
                call backend%line(1.0_wp, position, 1.0_wp + tick_len, position)
                call backend%text(1.0_wp + 0.12_wp, position, trim(tick_label))
            else
                call backend%line(position, 0.0_wp, position, -tick_len)
                call backend%text(position, -0.18_wp, trim(tick_label))
            end if
        end do
    end subroutine render_colorbar_custom_ticks

    subroutine render_colorbar_auto_ticks(backend, vertical, vmin, vmax, plot_area)
        class(plot_context), intent(inout) :: backend
        logical, intent(in) :: vertical
        real(wp), intent(in) :: vmin, vmax
        type(plot_area_t), intent(in) :: plot_area

        real(wp) :: tick_len, visible_ticks(40)
        character(len=50) :: tick_labels(40)
        integer :: n_visible, i

        tick_len = 0.08_wp
        call colorbar_auto_tick_labels(vmin, vmax, colorbar_tick_target(vertical, &
            plot_area), visible_ticks, tick_labels, n_visible)

        do i = 1, n_visible
            if (vertical) then
                call backend%line(1.0_wp, visible_ticks(i), 1.0_wp + tick_len, &
                    visible_ticks(i))
                call backend%text(1.0_wp + 0.12_wp, visible_ticks(i), &
                    trim(tick_labels(i)))
            else
                call backend%line(visible_ticks(i), 0.0_wp, visible_ticks(i), -tick_len)
                call backend%text(visible_ticks(i), -0.18_wp, trim(tick_labels(i)))
            end if
        end do
    end subroutine render_colorbar_auto_ticks

    subroutine colorbar_auto_tick_labels(vmin, vmax, target_ticks, ticks, labels, n)
        !! Automatic colorbar ticks inside [vmin, vmax] and their labels.
        real(wp), intent(in) :: vmin, vmax
        integer, intent(in) :: target_ticks
        real(wp), intent(out) :: ticks(40)
        character(len=50), intent(out) :: labels(40)
        integer, intent(out) :: n

        real(wp) :: tick_locations(40), nice_min, nice_max, nice_step, tol
        integer :: n_ticks, i, decimals

        call find_nice_tick_locations(vmin, vmax, target_ticks, nice_min, &
            nice_max, nice_step, tick_locations, n_ticks)

        ! Nice tick boundaries can fall just outside the data range; matplotlib
        ! does not draw colorbar ticks beyond [vmin, vmax].
        tol = 1.0e-6_wp * max(1.0_wp, abs(vmax - vmin))

        ! Format all in-range ticks with the same number of decimals
        ! (matplotlib shows "0.5, 1.0, 1.5", not "0.5, 1, 1.5").
        n = 0
        do i = 1, n_ticks
            if (tick_locations(i) < vmin - tol .or. tick_locations(i) > vmax + tol) cycle
            n = n + 1
            ticks(n) = tick_locations(i)
        end do
        decimals = determine_decimals_from_ticks(ticks, n)
        do i = 1, n
            labels(i) = format_tick_value_consistent(ticks(i), decimals)
        end do
    end subroutine colorbar_auto_tick_labels

    pure integer function colorbar_tick_target(vertical, plot_area) result(target)
        !! Target tick count for the colorbar long axis, sized to the bar's
        !! pixel length like matplotlib's length-aware MaxNLocator. matplotlib
        !! allows roughly one tick per ~40 px (its default ~0.4 in spacing at
        !! 100 dpi), then snaps to a nice step. A fixed count (e.g. 5) yields
        !! coarse steps (0.5) on tall bars where matplotlib renders 0.2.
        logical, intent(in) :: vertical
        type(plot_area_t), intent(in) :: plot_area

        integer :: length_px
        integer, parameter :: px_per_tick = 40

        if (vertical) then
            length_px = plot_area%height
        else
            length_px = plot_area%width
        end if

        target = max(3, 1 + nint(real(max(1, length_px), wp)/real(px_per_tick, wp)))
        target = min(target, 21)
    end function colorbar_tick_target

    subroutine render_colorbar_label(backend, vertical, vmin, vmax, mid_val, &
            plot_area, label, label_fontsize)
        class(plot_context), intent(inout) :: backend
        logical, intent(in) :: vertical
        real(wp), intent(in) :: vmin, vmax, mid_val
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: label
        real(wp), intent(in), optional :: label_fontsize

        real(wp) :: label_x_px, label_y_px
        real(wp) :: actual_fontsize
        real(wp) :: rotation, label_px
        real(wp) :: black_color(3)

        actual_fontsize = 10.0_wp
        if (present(label_fontsize)) actual_fontsize = label_fontsize
        black_color = [0.0_wp, 0.0_wp, 0.0_wp]

        if (vertical) then
            label_x_px = real(plot_area%left, wp) + &
                real(plot_area%width, wp) + 4.0_wp*actual_fontsize
            label_y_px = real(plot_area%bottom, wp) + &
                ((mid_val - vmin)/(vmax - vmin))* &
                real(plot_area%height, wp)
            rotation = 90.0_wp
        else
            label_x_px = real(plot_area%left, wp) + &
                ((mid_val - vmin)/(vmax - vmin))* &
                real(plot_area%width, wp)
            label_y_px = real(plot_area%bottom, wp) - &
                0.40_wp*real(plot_area%height, wp)
            rotation = 0.0_wp
        end if

        select type (bk => backend)
            type is (png_context)
            ! label_fontsize is in points like every other font size; the
            ! raster draws in pixels at the canvas dpi.
            label_px = pt2px(actual_fontsize, bk%raster%dpi)
            if (vertical) then
                ! Raster plot areas are measured from the top edge.
                label_x_px = real(plot_area%left + plot_area%width, wp) + &
                    4.0_wp*label_px
                label_y_px = real(plot_area%bottom, wp) + &
                    (1.0_wp - (mid_val - vmin)/(vmax - vmin))* &
                    real(plot_area%height, wp)
            else
                label_y_px = real(plot_area%bottom + plot_area%height, wp) + &
                    0.40_wp*real(plot_area%height, wp)
            end if
            call bk%draw_text_styled(label_x_px, label_y_px, trim(label), &
                label_px, rotation, 'center', &
                'center', .false., black_color)
            type is (pdf_context)
            call bk%draw_text_styled(label_x_px, label_y_px, trim(label), &
                actual_fontsize, rotation, 'center', &
                'center', .false., black_color)
        class default
            call backend%text(1.35_wp, mid_val, trim(label))
        end select
    end subroutine render_colorbar_label

    subroutine render_colorbar_with_context(backend, plot_area, vertical, vmin, &
            vmax, range_val, colormap, &
            use_custom_ticks, use_custom_labels, &
            custom_ticks, custom_ticklabels, &
            label, label_fontsize, saved_area, line_levels, line_width)
        class(plot_context), intent(inout) :: backend
        type(plot_area_t), intent(in) :: plot_area
        logical, intent(in) :: vertical
        real(wp), intent(in) :: vmin, vmax, range_val
        character(len=*), intent(in) :: colormap
        logical, intent(in) :: use_custom_ticks, use_custom_labels
        real(wp), intent(in), optional :: custom_ticks(:)
        character(len=*), intent(in), optional :: custom_ticklabels(:)
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: label_fontsize
        type(plot_area_t), intent(in) :: saved_area
        real(wp), intent(in), optional :: line_levels(:), line_width

        real(wp) :: x_min_saved, x_max_saved, y_min_saved, y_max_saved
        real(wp) :: bar_min, bar_max, contour_width
        real(wp), allocatable :: ticks(:), positions(:)

        call backend%save_coordinates(x_min_saved, x_max_saved, y_min_saved, &
            y_max_saved)
        call set_backend_plot_area(backend, plot_area)

        bar_min = vmin
        bar_max = vmax
        if (present(line_levels)) then
            contour_width = 1.5_wp
            if (present(line_width)) contour_width = line_width
            call render_contour_colorbar_lines(backend, vertical, line_levels, &
                                               colormap, contour_width)
            bar_min = 0.0_wp
            bar_max = real(max(1, size(line_levels) - 1), wp)
        else
            call render_colorbar_gradient(backend, vertical, vmin, vmax, range_val, &
                                          colormap, plot_area)
        end if
        call render_colorbar_border(backend, vertical, bar_min, bar_max)

        if (present(line_levels)) then
            call contour_colorbar_default_ticks(line_levels, ticks)
            if (use_custom_ticks) ticks = custom_ticks
            call contour_colorbar_tick_positions(line_levels, ticks, positions)
            call render_colorbar_custom_ticks(backend, vertical, &
                minval(line_levels), maxval(line_levels), &
                ticks, custom_ticklabels, use_custom_labels .and. use_custom_ticks, &
                tick_positions=positions)
        else if (use_custom_ticks) then
            call render_colorbar_custom_ticks(backend, vertical, vmin, vmax, &
                custom_ticks, custom_ticklabels, &
                use_custom_labels)
        else
            call render_colorbar_auto_ticks(backend, vertical, vmin, vmax, plot_area)
        end if

        if (present(label)) then
            if (len_trim(label) > 0) then
                call render_colorbar_label(backend, vertical, bar_min, bar_max, &
                    0.5_wp*(bar_min + bar_max), &
                    plot_area, trim(label), label_fontsize)
            end if
        end if

        call backend%set_coordinates(x_min_saved, x_max_saved, y_min_saved, y_max_saved)
        call set_backend_plot_area(backend, saved_area)
    end subroutine render_colorbar_with_context

end module fortplot_figure_colorbar
