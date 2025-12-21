module fortplot_subplot_layout
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: subplot_data_t
    use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
    use fortplot_tick_calculation, only: determine_decimals_from_ticks, &
                                         format_tick_value_consistent
    use fortplot_constants, only: TICK_MARK_LENGTH, TITLE_VERTICAL_OFFSET, &
                                  XLABEL_VERTICAL_OFFSET, X_TICK_LABEL_PAD, &
                                  Y_TICK_LABEL_RIGHT_PAD, YLABEL_EXTRA_GAP
    use fortplot_text_layout, only: calculate_text_width, calculate_text_height, &
                                    calculate_text_height_with_size_internal, &
                                    TITLE_FONT_SIZE
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_text_helpers, only: prepare_mathtext_if_needed
    use fortplot_unicode, only: escape_unicode_for_raster
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_pdf_text, only: estimate_pdf_text_width
    use fortplot_pdf_core, only: PDF_TICK_LABEL_SIZE, PDF_LABEL_SIZE, PDF_TITLE_SIZE
    implicit none

    private
    public :: compute_tight_subplot_margins

contains

    subroutine compute_tight_subplot_margins(backend, subplots_array, nr, nc, &
                                             xscale, yscale, symlog_threshold, left_f, &
                                             right_f, bottom_f, top_f, &
                                             ok)
        class(*), intent(in) :: backend
        type(subplot_data_t), intent(in) :: subplots_array(:, :)
        integer, intent(in) :: nr, nc
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), allocatable, intent(out) :: left_f(:, :), right_f(:, :)
        real(wp), allocatable, intent(out) :: bottom_f(:, :), top_f(:, :)
        logical, intent(out) :: ok

        real(wp), allocatable :: dec_left(:, :), dec_right(:, :)
        real(wp), allocatable :: dec_bottom(:, :), dec_top(:, :)

        integer :: i, j
        real(wp) :: fig_w, fig_h

        ok = .false.

        if (nr <= 0 .or. nc <= 0) return
        if (size(subplots_array, 1) /= nr) return
        if (size(subplots_array, 2) /= nc) return

        allocate (left_f(nr, nc), right_f(nr, nc))
        allocate (bottom_f(nr, nc), top_f(nr, nc))
        allocate (dec_left(nr, nc), dec_right(nr, nc))
        allocate (dec_bottom(nr, nc), dec_top(nr, nc))

        select type (bk => backend)
        class is (png_context)
            fig_w = real(max(1, bk%width), wp)
            fig_h = real(max(1, bk%height), wp)
            do i = 1, nr
                do j = 1, nc
                    call estimate_subplot_decorations_raster(subplots_array(i, j), &
                                                             xscale, yscale, &
                                                             symlog_threshold, &
                                                             dec_left(i, j), &
                                                             dec_right(i, j), &
                                                             dec_bottom(i, j), &
                                                             dec_top(i, j))
                end do
            end do
        class is (pdf_context)
            fig_w = real(max(1, bk%width), wp)
            fig_h = real(max(1, bk%height), wp)
            do i = 1, nr
                do j = 1, nc
                    call estimate_subplot_decorations_pdf(subplots_array(i, j), &
                                                          xscale, yscale, &
                                                          symlog_threshold, &
                                                          dec_left(i, j), &
                                                          dec_right(i, j), &
                                                          dec_bottom(i, j), &
                                                          dec_top(i, j))
                end do
            end do
        class default
            return
        end select

        call solve_tight_grid(fig_w, fig_h, dec_left, dec_right, dec_bottom, &
                              dec_top, left_f, right_f, bottom_f, top_f, ok)
    end subroutine compute_tight_subplot_margins

    subroutine solve_tight_grid(fig_w, fig_h, dec_left, dec_right, dec_bottom, &
                                dec_top, left_f, right_f, bottom_f, top_f, ok)
        real(wp), intent(in) :: fig_w, fig_h
        real(wp), intent(in) :: dec_left(:, :), dec_right(:, :)
        real(wp), intent(in) :: dec_bottom(:, :), dec_top(:, :)
        real(wp), intent(out) :: left_f(:, :), right_f(:, :)
        real(wp), intent(out) :: bottom_f(:, :), top_f(:, :)
        logical, intent(out) :: ok

        integer :: nr, nc, i, j
        real(wp), allocatable :: gap_x(:), gap_y(:)
        real(wp) :: margin_l, margin_r, margin_b, margin_t
        real(wp) :: axes_w, axes_h
        real(wp) :: avail_w, avail_h
        real(wp) :: pad
        real(wp) :: x0, x1, y0, y1

        nr = size(dec_left, 1)
        nc = size(dec_left, 2)
        ok = .false.

        pad = 6.0_wp

        allocate (gap_x(max(0, nc - 1)))
        allocate (gap_y(max(0, nr - 1)))
        gap_x = 0.0_wp
        gap_y = 0.0_wp

        margin_l = 0.0_wp
        margin_r = 0.0_wp
        margin_b = 0.0_wp
        margin_t = 0.0_wp

        do i = 1, nr
            margin_l = max(margin_l, dec_left(i, 1))
            margin_r = max(margin_r, dec_right(i, nc))
        end do
        do j = 1, nc
            margin_b = max(margin_b, dec_bottom(nr, j))
            margin_t = max(margin_t, dec_top(1, j))
        end do

        margin_l = margin_l + pad
        margin_r = margin_r + pad
        margin_b = margin_b + pad
        margin_t = margin_t + pad

        do j = 1, nc - 1
            gap_x(j) = 0.0_wp
            do i = 1, nr
                gap_x(j) = max(gap_x(j), dec_right(i, j) + dec_left(i, j + 1))
            end do
            gap_x(j) = gap_x(j) + pad
        end do

        do i = 1, nr - 1
            gap_y(i) = 0.0_wp
            do j = 1, nc
                gap_y(i) = max(gap_y(i), dec_bottom(i, j) + dec_top(i + 1, j))
            end do
            gap_y(i) = gap_y(i) + pad
        end do

        avail_w = fig_w - margin_l - margin_r
        if (nc > 1) avail_w = avail_w - sum(gap_x)
        avail_h = fig_h - margin_t - margin_b
        if (nr > 1) avail_h = avail_h - sum(gap_y)

        if (avail_w <= real(nc, wp) .or. avail_h <= real(nr, wp)) return

        axes_w = avail_w/real(nc, wp)
        axes_h = avail_h/real(nr, wp)

        do i = 1, nr
            y1 = fig_h - margin_t
            if (i > 1) then
                y1 = y1 - real(i - 1, wp)*axes_h - sum(gap_y(1:i - 1))
            end if
            y0 = y1 - axes_h
            do j = 1, nc
                x0 = margin_l
                if (j > 1) then
                    x0 = x0 + real(j - 1, wp)*axes_w + sum(gap_x(1:j - 1))
                end if
                x1 = x0 + axes_w

                left_f(i, j) = x0/fig_w
                right_f(i, j) = x1/fig_w
                bottom_f(i, j) = y0/fig_h
                top_f(i, j) = y1/fig_h
            end do
        end do

        ok = .true.
    end subroutine solve_tight_grid

    subroutine estimate_subplot_decorations_raster(subplot, xscale, yscale, &
                                                   symlog_threshold, dec_left, &
                                                   dec_right, dec_bottom, dec_top)
        type(subplot_data_t), intent(in) :: subplot
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(out) :: dec_left, dec_right, dec_bottom, dec_top

        real(wp) :: x_tick_positions(MAX_TICKS), y_tick_positions(MAX_TICKS)
        integer :: n_x, n_y, i, decimals
        character(len=50) :: x_labels(MAX_TICKS), y_labels(MAX_TICKS)
        integer :: max_y_w, max_x_h
        integer :: xlabel_h, ylabel_h, title_h
        character(len=:), allocatable :: title, xlabel, ylabel

        max_y_w = 0
        max_x_h = 0

        call compute_scale_ticks(xscale, subplot%x_min, subplot%x_max, &
                                 symlog_threshold, x_tick_positions, n_x)
        call compute_scale_ticks(yscale, subplot%y_min, subplot%y_max, &
                                 symlog_threshold, y_tick_positions, n_y)

        if (n_x > 0) then
            decimals = 0
            if (trim(xscale) == 'linear' .and. n_x >= 2) then
                decimals = determine_decimals_from_ticks(x_tick_positions, n_x)
            end if
            do i = 1, n_x
                if (trim(xscale) == 'linear') then
                    x_labels(i) = format_tick_value_consistent(x_tick_positions(i), &
                                                               decimals)
                else
                    x_labels(i) = format_tick_label(x_tick_positions(i), xscale)
                end if
                max_x_h = max(max_x_h, measure_raster_height(trim(x_labels(i))))
            end do
        end if

        if (n_y > 0) then
            decimals = 0
            if (trim(yscale) == 'linear' .and. n_y >= 2) then
                decimals = determine_decimals_from_ticks(y_tick_positions, n_y)
            end if
            do i = 1, n_y
                if (trim(yscale) == 'linear') then
                    y_labels(i) = format_tick_value_consistent(y_tick_positions(i), &
                                                               decimals)
                else
                    y_labels(i) = format_tick_label(y_tick_positions(i), yscale)
                end if
                max_y_w = max(max_y_w, measure_raster_width(trim(y_labels(i))))
            end do
        end if

        title = ''
        xlabel = ''
        ylabel = ''
        if (allocated(subplot%title)) title = subplot%title
        if (allocated(subplot%xlabel)) xlabel = subplot%xlabel
        if (allocated(subplot%ylabel)) ylabel = subplot%ylabel

        xlabel_h = 0
        if (len_trim(xlabel) > 0) xlabel_h = measure_raster_height(xlabel)

        ylabel_h = 0
        if (len_trim(ylabel) > 0) ylabel_h = measure_raster_height(ylabel)

        title_h = 0
        if (len_trim(title) > 0) then
            title_h = &
                calculate_text_height_with_size_internal(real(TITLE_FONT_SIZE, wp))
        end if

        dec_left = real(Y_TICK_LABEL_RIGHT_PAD + max_y_w, wp)
        dec_left = max(dec_left, real(TICK_MARK_LENGTH, wp))
        if (len_trim(ylabel) > 0) then
            dec_left = real(TICK_MARK_LENGTH + Y_TICK_LABEL_RIGHT_PAD + max_y_w + &
                            YLABEL_EXTRA_GAP + ylabel_h, wp)
        end if
        dec_right = 0.0_wp

        dec_bottom = real(X_TICK_LABEL_PAD + max_x_h, wp)
        if (len_trim(xlabel) > 0) then
            dec_bottom = max(dec_bottom, real(XLABEL_VERTICAL_OFFSET + 5 + &
                                              xlabel_h, wp))
        end if

        dec_top = 0.0_wp
        if (len_trim(title) > 0) then
            dec_top = real(TITLE_VERTICAL_OFFSET + title_h, wp)
        end if
    end subroutine estimate_subplot_decorations_raster

    subroutine estimate_subplot_decorations_pdf(subplot, xscale, yscale, &
                                                symlog_threshold, dec_left, dec_right, &
                                                dec_bottom, dec_top)
        type(subplot_data_t), intent(in) :: subplot
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(out) :: dec_left, dec_right, dec_bottom, dec_top

        real(wp) :: x_tick_positions(MAX_TICKS), y_tick_positions(MAX_TICKS)
        integer :: n_x, n_y, i, decimals
        character(len=50) :: x_labels(MAX_TICKS), y_labels(MAX_TICKS)
        real(wp) :: max_y_w, max_x_h
        real(wp) :: xlabel_h, title_h
        character(len=:), allocatable :: title, xlabel, ylabel

        real(wp), parameter :: X_TICK_GAP = 15.0_wp
        real(wp), parameter :: Y_TICK_GAP = 1.0_wp
        real(wp), parameter :: TITLE_GAP = 20.0_wp
        real(wp), parameter :: YLABEL_PAD = 1.0_wp
        real(wp), parameter :: LABEL_THICKNESS = 1.2_wp*PDF_LABEL_SIZE

        max_y_w = 0.0_wp
        max_x_h = 0.0_wp

        call compute_scale_ticks(xscale, subplot%x_min, subplot%x_max, &
                                 symlog_threshold, x_tick_positions, n_x)
        call compute_scale_ticks(yscale, subplot%y_min, subplot%y_max, &
                                 symlog_threshold, y_tick_positions, n_y)

        if (n_x > 0) then
            decimals = 0
            if (trim(xscale) == 'linear' .and. n_x >= 2) then
                decimals = determine_decimals_from_ticks(x_tick_positions, n_x)
            end if
            do i = 1, n_x
                if (trim(xscale) == 'linear') then
                    x_labels(i) = format_tick_value_consistent(x_tick_positions(i), &
                                                               decimals)
                else
                    x_labels(i) = format_tick_label(x_tick_positions(i), xscale)
                end if
                max_x_h = max(max_x_h, PDF_TICK_LABEL_SIZE*1.2_wp)
            end do
        end if

        if (n_y > 0) then
            decimals = 0
            if (trim(yscale) == 'linear' .and. n_y >= 2) then
                decimals = determine_decimals_from_ticks(y_tick_positions, n_y)
            end if
            do i = 1, n_y
                if (trim(yscale) == 'linear') then
                    y_labels(i) = format_tick_value_consistent(y_tick_positions(i), &
                                                               decimals)
                else
                    y_labels(i) = format_tick_label(y_tick_positions(i), yscale)
                end if
                max_y_w = max(max_y_w, estimate_pdf_text_width(trim(y_labels(i)), &
                                                               PDF_TICK_LABEL_SIZE))
            end do
        end if

        title = ''
        xlabel = ''
        ylabel = ''
        if (allocated(subplot%title)) title = subplot%title
        if (allocated(subplot%xlabel)) xlabel = subplot%xlabel
        if (allocated(subplot%ylabel)) ylabel = subplot%ylabel

        xlabel_h = 0.0_wp
        if (len_trim(xlabel) > 0) xlabel_h = PDF_LABEL_SIZE*1.2_wp

        title_h = 0.0_wp
        if (len_trim(title) > 0) title_h = PDF_TITLE_SIZE*1.2_wp

        dec_left = Y_TICK_GAP + max_y_w
        if (len_trim(ylabel) > 0) then
            dec_left = dec_left + YLABEL_PAD + LABEL_THICKNESS
        end if
        dec_right = 0.0_wp

        dec_bottom = X_TICK_GAP + max_x_h
        if (len_trim(xlabel) > 0) then
            dec_bottom = max(dec_bottom, real(XLABEL_VERTICAL_OFFSET, wp) + xlabel_h)
        end if

        dec_top = 0.0_wp
        if (len_trim(title) > 0) then
            dec_top = TITLE_GAP + title_h
        end if
    end subroutine estimate_subplot_decorations_pdf

    integer function measure_raster_width(text) result(w)
        character(len=*), intent(in) :: text
        character(len=500) :: processed
        integer :: plen, mlen
        character(len=600) :: math_ready
        character(len=600) :: escaped

        if (len_trim(text) == 0) then
            w = 0
            return
        end if

        call process_latex_in_text(trim(text), processed, plen)
        call prepare_mathtext_if_needed(processed(1:plen), math_ready, mlen)
        call escape_unicode_for_raster(math_ready(1:mlen), escaped)
        w = calculate_text_width(trim(escaped))
    end function measure_raster_width

    integer function measure_raster_height(text) result(h)
        character(len=*), intent(in) :: text
        character(len=500) :: processed
        integer :: plen, mlen
        character(len=600) :: math_ready
        character(len=600) :: escaped

        if (len_trim(text) == 0) then
            h = 0
            return
        end if

        call process_latex_in_text(trim(text), processed, plen)
        call prepare_mathtext_if_needed(processed(1:plen), math_ready, mlen)
        call escape_unicode_for_raster(math_ready(1:mlen), escaped)
        h = calculate_text_height(trim(escaped))
        if (h <= 0) h = 12
    end function measure_raster_height

end module fortplot_subplot_layout
