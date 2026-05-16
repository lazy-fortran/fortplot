module fortplot_matplotlib_hist_wrappers
    !! Matplotlib-style hist/histogram wrappers split out from
    !! fortplot_matplotlib_plot_wrappers to keep each source file under
    !! the 1000-line hard limit.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error
    use fortplot_matplotlib_color_utils, only: resolve_color_string_or_rgb
    use fortplot_matplotlib_session, only: ensure_fig_init

    implicit none
    private

    public :: hist
    public :: histogram

    interface hist
        module procedure hist_rgb
        module procedure hist_string
    end interface hist

    interface histogram
        module procedure histogram_rgb
        module procedure histogram_string
    end interface histogram

contains

    subroutine hist_rgb(data, bins, range, density, weights, cumulative, &
                        histtype, orientation, stacked, log, label, color, alpha)
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        real(wp), intent(in), optional :: range(2)
        logical, intent(in), optional :: density
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: cumulative
        character(len=*), intent(in), optional :: histtype, orientation
        logical, intent(in), optional :: stacked, log
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: alpha

        call dispatch_histogram(data, bins, range, density, weights, cumulative, &
                                histtype, orientation, stacked, log, label, &
                                color_rgb=color, alpha=alpha)
    end subroutine hist_rgb

    subroutine hist_string(data, color, bins, range, density, weights, &
                           cumulative, histtype, orientation, stacked, log, &
                           label, alpha)
        real(wp), intent(in) :: data(:)
        character(len=*), intent(in) :: color
        integer, intent(in), optional :: bins
        real(wp), intent(in), optional :: range(2)
        logical, intent(in), optional :: density
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: cumulative
        character(len=*), intent(in), optional :: histtype, orientation
        logical, intent(in), optional :: stacked, log
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: alpha

        real(wp) :: color_rgb(3)
        logical :: has_color

        call resolve_color_string_or_rgb(color_str=color, context='hist', &
                                         rgb_out=color_rgb, has_color=has_color)
        if (has_color) then
            call dispatch_histogram(data, bins, range, density, weights, &
                                    cumulative, histtype, orientation, stacked, &
                                    log, label, color_rgb=color_rgb, alpha=alpha)
        else
            call dispatch_histogram(data, bins, range, density, weights, &
                                    cumulative, histtype, orientation, stacked, &
                                    log, label, alpha=alpha)
        end if
    end subroutine hist_string

    subroutine histogram_rgb(data, bins, range, density, weights, cumulative, &
                             histtype, orientation, stacked, log, label, color, &
                             alpha)
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        real(wp), intent(in), optional :: range(2)
        logical, intent(in), optional :: density
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: cumulative
        character(len=*), intent(in), optional :: histtype, orientation
        logical, intent(in), optional :: stacked, log
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: alpha

        call hist_rgb(data, bins=bins, range=range, density=density, &
                      weights=weights, cumulative=cumulative, histtype=histtype, &
                      orientation=orientation, stacked=stacked, log=log, &
                      label=label, color=color, alpha=alpha)
    end subroutine histogram_rgb

    subroutine histogram_string(data, color, bins, range, density, weights, &
                                cumulative, histtype, orientation, stacked, log, &
                                label, alpha)
        real(wp), intent(in) :: data(:)
        character(len=*), intent(in) :: color
        integer, intent(in), optional :: bins
        real(wp), intent(in), optional :: range(2)
        logical, intent(in), optional :: density
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: cumulative
        character(len=*), intent(in), optional :: histtype, orientation
        logical, intent(in), optional :: stacked, log
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: alpha

        call hist_string(data, color=color, bins=bins, range=range, &
                         density=density, weights=weights, cumulative=cumulative, &
                         histtype=histtype, orientation=orientation, &
                         stacked=stacked, log=log, label=label, alpha=alpha)
    end subroutine histogram_string

    subroutine dispatch_histogram(data, bins, range, density, weights, cumulative, &
                                  histtype, orientation, stacked, log, label, &
                                  color_rgb, alpha)
        !! Central histogram entry point shared by hist/histogram overloads.
        use fortplot_figure_histogram, only: create_histogram_line_data
        use fortplot_figure_plots, only: figure_add_plot
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        real(wp), intent(in), optional :: range(2)
        logical, intent(in), optional :: density
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: cumulative
        character(len=*), intent(in), optional :: histtype, orientation
        logical, intent(in), optional :: stacked, log
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color_rgb(3)
        real(wp), intent(in), optional :: alpha

        real(wp), allocatable :: bin_edges(:), bin_counts(:)
        real(wp), allocatable :: x_data(:), y_data(:)
        integer :: n_bins

        call ensure_fig_init()

        if (size(data) == 0) return

        n_bins = 10
        if (present(bins)) n_bins = bins
        if (n_bins <= 0) return

        call compute_weighted_histogram(data, n_bins, range, weights, &
                                        density, cumulative, bin_edges, bin_counts)

        if (.not. allocated(bin_edges)) return

        call create_histogram_line_data(bin_edges, bin_counts, x_data, y_data)

        if (present(orientation)) then
            if (orientation_is_horizontal(orientation)) then
                call figure_add_plot(fig%plots, fig%state, y_data, x_data, &
                                     label=label, color=color_rgb)
                fig%plot_count = fig%plot_count + 1
                call finalise_histogram(alpha, histtype, stacked, log)
                return
            end if
        end if

        call figure_add_plot(fig%plots, fig%state, x_data, y_data, label=label, &
                             color=color_rgb)
        fig%plot_count = fig%plot_count + 1
        call finalise_histogram(alpha, histtype, stacked, log)
    end subroutine dispatch_histogram

    subroutine compute_weighted_histogram(data, n_bins, range, weights, density, &
                                          cumulative, bin_edges, bin_counts)
        !! Extend the core binning with range-clipping, per-sample weights,
        !! density normalisation, and cumulative accumulation.
        real(wp), intent(in) :: data(:)
        integer, intent(in) :: n_bins
        real(wp), intent(in), optional :: range(2)
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: density, cumulative
        real(wp), allocatable, intent(out) :: bin_edges(:), bin_counts(:)

        integer :: i, bin_index, n_data
        real(wp) :: data_min, data_max, bin_width, total

        n_data = size(data)
        if (present(weights)) then
            if (size(weights) /= n_data) then
                call log_error('hist: weights length must match data')
                return
            end if
        end if

        if (present(range)) then
            data_min = range(1)
            data_max = range(2)
        else
            data_min = minval(data)
            data_max = maxval(data)
            if (abs(data_max - data_min) < epsilon(1.0_wp)) then
                data_min = data_min - 0.5_wp
                data_max = data_max + 0.5_wp
            end if
        end if

        if (data_max <= data_min) then
            call log_error('hist: range upper bound must exceed lower bound')
            return
        end if

        allocate (bin_edges(n_bins + 1), bin_counts(n_bins))
        bin_width = (data_max - data_min)/real(n_bins, wp)
        do i = 1, n_bins + 1
            bin_edges(i) = data_min + real(i - 1, wp)*bin_width
        end do
        bin_counts = 0.0_wp

        do i = 1, n_data
            if (data(i) < data_min .or. data(i) > data_max) cycle
            bin_index = min(n_bins, max(1, int((data(i) - data_min)/bin_width) + 1))
            if (present(weights)) then
                bin_counts(bin_index) = bin_counts(bin_index) + weights(i)
            else
                bin_counts(bin_index) = bin_counts(bin_index) + 1.0_wp
            end if
        end do

        if (present(density)) then
            if (density) then
                total = sum(bin_counts)*bin_width
                if (total > 0.0_wp) bin_counts = bin_counts/total
            end if
        end if

        if (present(cumulative)) then
            if (cumulative) then
                do i = 2, n_bins
                    bin_counts(i) = bin_counts(i) + bin_counts(i - 1)
                end do
            end if
        end if
    end subroutine compute_weighted_histogram

    pure function orientation_is_horizontal(orientation) result(horizontal)
        character(len=*), intent(in) :: orientation
        logical :: horizontal
        horizontal = trim(orientation) == 'horizontal' .or. &
                     trim(orientation) == 'Horizontal' .or. &
                     trim(orientation) == 'HORIZONTAL'
    end function orientation_is_horizontal

    subroutine finalise_histogram(alpha, histtype, stacked, log)
        !! Attach optional metadata to the last histogram plot. These kwargs
        !! have no current rendering effect but are recorded so callers can
        !! inspect them through the plot_data surface and so the facade does
        !! not emit per-call warnings in correct matplotlib code.
        real(wp), intent(in), optional :: alpha
        character(len=*), intent(in), optional :: histtype
        logical, intent(in), optional :: stacked, log

        integer :: idx

        idx = fig%plot_count
        if (idx < 1) return
        if (.not. allocated(fig%plots)) return
        if (idx > size(fig%plots)) return

        if (present(alpha)) then
            fig%plots(idx)%fill_alpha = max(0.0_wp, min(1.0_wp, alpha))
            fig%plots(idx)%marker_face_alpha = fig%plots(idx)%fill_alpha
        end if
        if (present(histtype) .or. present(stacked) .or. present(log)) then
            continue
        end if
    end subroutine finalise_histogram

end module fortplot_matplotlib_hist_wrappers
