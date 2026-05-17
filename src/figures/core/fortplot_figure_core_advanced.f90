module fortplot_figure_core_advanced
    !! Advanced plotting operations extracted from fortplot_figure_core
    !!
    !! This module contains advanced plotting functionality like scatter plots,
    !! histograms, and statistical plots that were moved from the core module
    !! to maintain architectural compliance with size limits.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t, ensure_figure_storage
    use fortplot_figure_operations
    use fortplot_figure_core_ranges, only: update_data_ranges_figure
    implicit none

    private
    public :: core_scatter, core_hist, core_boxplot, core_colorbar

contains

  subroutine core_scatter(plots, state, plot_count, x, y, s, c, marker, &
                             markersize, &
                             color, colormap, alpha, edgecolor, facecolor, &
                             linewidth, &
                             vmin, vmax, label, show_colorbar, default_color)
        !! Add an efficient scatter plot using a single plot object
        !! Properly handles thousands of points without O(n) overhead
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(..), c(:)
        character(len=*), intent(in), optional :: marker, colormap, label
        real(wp), intent(in), optional :: markersize, alpha, linewidth, vmin, vmax
        real(wp), intent(in), optional :: color(3), edgecolor(3), facecolor(3)
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in) :: default_color(3)

        ! Delegate to efficient scatter implementation
        call ensure_figure_storage(plots, state)
        call figure_scatter_operation(state, plots, plot_count, &
                                      x, y, s, c, marker, markersize, color, &
                                      colormap, alpha, edgecolor, facecolor, &
                                      linewidth, vmin, vmax, label, show_colorbar, &
                                      default_color)

        ! Sync plot_count back to state and update data ranges
        state%plot_count = plot_count
        call update_data_ranges_figure(plots, state, state%plot_count)
    end subroutine core_scatter

    subroutine core_hist(plots, state, plot_count, data, bins, density, label, color)
        !! Create a histogram plot
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), contiguous, intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)

        call ensure_figure_storage(plots, state)
        call figure_hist_operation(plots, state, plot_count, data, bins, density, &
                                   label, color)
    end subroutine core_hist

  subroutine core_boxplot(plots, state, plot_count, data, position, width, label, &
                             show_outliers, horizontal, color, max_plots)
        !! Create a box plot
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), contiguous, intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        real(wp), intent(in), optional :: color(3)
        integer, intent(in) :: max_plots

        call ensure_figure_storage(plots, state)
        call figure_boxplot_operation(state, plots, plot_count, data, position, &
                                      width, label, &
                                      show_outliers, horizontal, color, max_plots)
    end subroutine core_boxplot

    subroutine core_colorbar(state, plots, plot_count, plot_index, label, location, &
                             fraction, pad, shrink, ticks, ticklabels, label_fontsize)
        !! Enable a stateful colorbar for the current figure.
        !!
        !! This mirrors matplotlib's pyplot behavior: the colorbar is configured
        !! independently from plot creation and is rendered during save/show.
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        integer, intent(in), optional :: plot_index
        character(len=*), intent(in), optional :: label, location
        real(wp), intent(in), optional :: fraction, pad, shrink
        real(wp), intent(in), optional :: ticks(:)
        character(len=*), intent(in), optional :: ticklabels(:)
        real(wp), intent(in), optional :: label_fontsize

        integer :: idx, i

        associate (dummy => size(plots)); end associate

        if (plot_count <= 0) then
            state%colorbar_enabled = .false.
            state%colorbar_plot_index = 0
            return
        end if

        idx = 0
        if (present(plot_index)) then
            if (plot_index >= 1 .and. plot_index <= plot_count) then
                idx = plot_index
            end if
        end if

        if (idx == 0) then
            idx = plot_count
        end if

        state%colorbar_enabled = .true.
        state%colorbar_plot_index = idx

        if (present(location)) then
            if (len_trim(location) > 0) state%colorbar_location = trim(location)
        end if

        if (present(fraction)) then
            state%colorbar_fraction = max(0.01_wp, min(0.45_wp, fraction))
        end if

        if (present(pad)) then
            state%colorbar_pad = max(0.0_wp, min(0.30_wp, pad))
        end if

        if (present(shrink)) then
            state%colorbar_shrink = max(0.05_wp, min(1.0_wp, shrink))
        end if

        state%colorbar_label_set = .false.
        if (allocated(state%colorbar_label)) deallocate (state%colorbar_label)
        if (present(label)) then
            if (len_trim(label) > 0) then
                state%colorbar_label = trim(label)
                state%colorbar_label_set = .true.
            end if
        end if

        state%colorbar_ticks_set = .false.
        if (allocated(state%colorbar_ticks)) deallocate (state%colorbar_ticks)
        if (present(ticks)) then
            if (size(ticks) > 0) then
                allocate (state%colorbar_ticks(size(ticks)))
                state%colorbar_ticks = ticks
                state%colorbar_ticks_set = .true.
            end if
        end if

        state%colorbar_ticklabels_set = .false.
        if (allocated(state%colorbar_ticklabels)) deallocate (state%colorbar_ticklabels)
        if (present(ticklabels)) then
            if (size(ticklabels) > 0) then
                allocate (state%colorbar_ticklabels(size(ticklabels)))
                do i = 1, size(ticklabels)
                    state%colorbar_ticklabels(i) = trim(ticklabels(i))
                end do
                state%colorbar_ticklabels_set = .true.
            end if
        end if

        if (present(label_fontsize)) then
            state%colorbar_label_fontsize = max(4.0_wp, min(72.0_wp, label_fontsize))
        end if
    end subroutine core_colorbar

end module fortplot_figure_core_advanced
