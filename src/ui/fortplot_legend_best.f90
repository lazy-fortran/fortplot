module fortplot_legend_best
    !! Resolve matplotlib 'best' legend placement against the plotted artists.
    !!
    !! Single Responsibility: collect artist sample points in data coordinates
    !! and delegate corner selection to fortplot_legend_layout. Kept separate
    !! from rendering so the layout module stays free of plot_data_t.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, &
                                  PLOT_TYPE_BOXPLOT, PLOT_TYPE_PIE
    use fortplot_legend_layout, only: choose_best_legend_position
    use fortplot_legend_state, only: legend_t, LEGEND_BEST
    implicit none

    private
    public :: resolve_best_legend_position

    real(wp), parameter :: DEFAULT_BAR_WIDTH = 0.8_wp

contains

    subroutine resolve_best_legend_position(legend, plots, plot_count, &
                                            x_min, x_max, y_min, y_max, &
                                            pixel_plot_width, pixel_plot_height)
        !! If the legend is in 'best' mode, pick the lowest-overlap corner and
        !! pin the legend to it. Coordinates passed to the scorer are relative to
        !! the data window origin (x_min, y_min), matching the layout module.
        type(legend_t), intent(inout) :: legend
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: pixel_plot_width, pixel_plot_height

        character(len=256), allocatable :: labels(:)
        real(wp), allocatable :: ax(:), ay(:)
        real(wp) :: data_width, data_height
        integer :: i

        if (legend%position /= LEGEND_BEST) return
        if (legend%num_entries == 0) return

        data_width = x_max - x_min
        data_height = y_max - y_min
        if (data_width <= 0.0_wp .or. data_height <= 0.0_wp) then
            legend%position = 2  ! LEGEND_UPPER_RIGHT fallback
            return
        end if

        allocate(labels(legend%num_entries))
        do i = 1, legend%num_entries
            labels(i) = legend%entries(i)%label
        end do

        call collect_artist_points(plots, plot_count, x_min, y_min, ax, ay)

        legend%position = choose_best_legend_position(labels, data_width, &
            data_height, legend%num_entries, ax, ay, &
            pixel_plot_width, pixel_plot_height)
    end subroutine resolve_best_legend_position

    subroutine collect_artist_points(plots, plot_count, x_min, y_min, ax, ay)
        !! Build artist sample points in data-window-relative coordinates.
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        real(wp), intent(in) :: x_min, y_min
        real(wp), allocatable, intent(out) :: ax(:), ay(:)

        integer :: i, n
        real(wp), allocatable :: px(:), py(:)

        allocate(ax(0), ay(0))
        n = min(plot_count, size(plots))
        do i = 1, n
            select case (plots(i)%plot_type)
            case (PLOT_TYPE_PIE)
                cycle
            case (PLOT_TYPE_BAR)
                call bar_points(plots(i), px, py)
            case (PLOT_TYPE_BOXPLOT)
                call boxplot_points(plots(i), px, py)
            case (PLOT_TYPE_HISTOGRAM)
                call histogram_points(plots(i), px, py)
            case default
                call line_points(plots(i), px, py)
            end select
            call append_relative(ax, ay, px, py, x_min, y_min)
        end do
    end subroutine collect_artist_points

    subroutine append_relative(ax, ay, px, py, x_min, y_min)
        !! Append (px, py) to (ax, ay), shifted to data-window-relative coords.
        real(wp), allocatable, intent(inout) :: ax(:), ay(:)
        real(wp), allocatable, intent(in) :: px(:), py(:)
        real(wp), intent(in) :: x_min, y_min
        real(wp), allocatable :: tx(:), ty(:)
        integer :: old, add

        if (.not. allocated(px) .or. .not. allocated(py)) return
        add = min(size(px), size(py))
        if (add == 0) return
        old = size(ax)
        allocate(tx(old + add), ty(old + add))
        if (old > 0) then
            tx(1:old) = ax
            ty(1:old) = ay
        end if
        tx(old + 1:old + add) = px(1:add) - x_min
        ty(old + 1:old + add) = py(1:add) - y_min
        call move_alloc(tx, ax)
        call move_alloc(ty, ay)
    end subroutine append_relative

    subroutine line_points(plot, px, py)
        !! Vertices of line/scatter-style plots.
        type(plot_data_t), intent(in) :: plot
        real(wp), allocatable, intent(out) :: px(:), py(:)
        integer :: n

        if (.not. allocated(plot%x) .or. .not. allocated(plot%y)) then
            allocate(px(0), py(0))
            return
        end if
        n = min(size(plot%x), size(plot%y))
        px = plot%x(1:n)
        py = plot%y(1:n)
    end subroutine line_points

    subroutine bar_points(plot, px, py)
        !! Four corners of each bar rectangle.
        type(plot_data_t), intent(in) :: plot
        real(wp), allocatable, intent(out) :: px(:), py(:)
        integer :: i, n, k
        real(wp) :: hw, base, top, lo_x, hi_x, lo_y, hi_y, w

        if (.not. allocated(plot%bar_x) .or. .not. allocated(plot%bar_heights)) then
            allocate(px(0), py(0))
            return
        end if
        n = min(size(plot%bar_x), size(plot%bar_heights))
        if (n <= 0) then
            allocate(px(0), py(0))
            return
        end if
        w = abs(plot%bar_width)
        if (w <= 0.0_wp) w = DEFAULT_BAR_WIDTH
        hw = 0.5_wp * w
        allocate(px(4 * n), py(4 * n))
        do i = 1, n
            base = 0.0_wp
            if (allocated(plot%bar_bottom)) then
                if (i <= size(plot%bar_bottom)) base = plot%bar_bottom(i)
            end if
            top = base + plot%bar_heights(i)
            if (plot%bar_horizontal) then
                lo_x = min(base, top); hi_x = max(base, top)
                lo_y = plot%bar_x(i) - hw; hi_y = plot%bar_x(i) + hw
            else
                lo_x = plot%bar_x(i) - hw; hi_x = plot%bar_x(i) + hw
                lo_y = min(base, top); hi_y = max(base, top)
            end if
            k = 4 * (i - 1)
            px(k + 1:k + 4) = [lo_x, hi_x, hi_x, lo_x]
            py(k + 1:k + 4) = [lo_y, lo_y, hi_y, hi_y]
        end do
    end subroutine bar_points

    subroutine boxplot_points(plot, px, py)
        !! Box rectangle corners plus whisker extents.
        type(plot_data_t), intent(in) :: plot
        real(wp), allocatable, intent(out) :: px(:), py(:)
        real(wp) :: pos, hw, lo, hi, wlo, whi

        if (.not. allocated(plot%box_data)) then
            allocate(px(0), py(0))
            return
        end if
        if (size(plot%box_data) == 0) then
            allocate(px(0), py(0))
            return
        end if
        pos = plot%position
        hw = 0.5_wp * plot%width
        lo = plot%q1; hi = plot%q3
        wlo = plot%whisker_low; whi = plot%whisker_high
        if (plot%horizontal) then
            px = [lo, hi, hi, lo, wlo, whi]
            py = [pos - hw, pos - hw, pos + hw, pos + hw, pos, pos]
        else
            px = [pos - hw, pos + hw, pos + hw, pos - hw, pos, pos]
            py = [lo, lo, hi, hi, wlo, whi]
        end if
    end subroutine boxplot_points

    subroutine histogram_points(plot, px, py)
        !! Bar corners reconstructed from histogram bin edges and counts.
        type(plot_data_t), intent(in) :: plot
        real(wp), allocatable, intent(out) :: px(:), py(:)
        integer :: i, n, k

        if (.not. allocated(plot%hist_bin_edges) .or. &
            .not. allocated(plot%hist_counts)) then
            allocate(px(0), py(0))
            return
        end if
        n = min(size(plot%hist_bin_edges) - 1, size(plot%hist_counts))
        if (n <= 0) then
            allocate(px(0), py(0))
            return
        end if
        allocate(px(4 * n), py(4 * n))
        do i = 1, n
            k = 4 * (i - 1)
            px(k + 1:k + 4) = [plot%hist_bin_edges(i), plot%hist_bin_edges(i + 1), &
                               plot%hist_bin_edges(i + 1), plot%hist_bin_edges(i)]
            py(k + 1:k + 4) = [0.0_wp, 0.0_wp, plot%hist_counts(i), plot%hist_counts(i)]
        end do
    end subroutine histogram_points

end module fortplot_legend_best
