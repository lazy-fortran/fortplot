module fortplot_figure_core_advanced
    !! Advanced plotting operations extracted from fortplot_figure_core
    !!
    !! This module contains advanced plotting functionality like scatter plots,
    !! histograms, and statistical plots that were moved from the core module
    !! to maintain architectural compliance with size limits.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_BOXPLOT
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

    subroutine core_hist(plots, state, plot_count, data, bins, density, label, color, &
                         range, weights, cumulative, orientation, alpha)
        !! Create a histogram plot (matplotlib-compatible).
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), contiguous, intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: range(2)
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: cumulative
        character(len=*), intent(in), optional :: orientation
        real(wp), intent(in), optional :: alpha

        call ensure_figure_storage(plots, state)
        call figure_hist_operation(plots, state, plot_count, data, bins, density, &
                                   label, color, range=range, weights=weights, &
                                   cumulative=cumulative, orientation=orientation, &
                                   alpha=alpha)
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
        call apply_boxplot_category_ticks(plots, state, plot_count)
    end subroutine core_boxplot

    subroutine apply_boxplot_category_ticks(plots, state, plot_count)
        !! Pin the category axis to the box positions, as matplotlib's bxp does.
        !!
        !! A box plot's category axis is discrete, but it was left to the linear
        !! locator, which produced ticks between the boxes: three boxes at 1, 2, 3
        !! were labelled 1.0, 1.5, 2.0, 2.5, 3.0, and half of those mark nothing.
        !! matplotlib places a tick per box and nowhere else.
        !!
        !! Auto-applied ticks are flagged so an explicit set_xticks by the caller
        !! wins and is never overwritten by a later boxplot call.
        type(plot_data_t), intent(in) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(in) :: plot_count

        real(wp), allocatable :: positions(:)
        character(len=50), allocatable :: labels(:)
        integer :: i, n
        logical :: horizontal

        n = 0
        horizontal = .false.
        allocate (positions(max(plot_count, 1)), labels(max(plot_count, 1)))
        do i = 1, min(plot_count, size(plots))
            if (plots(i)%plot_type /= PLOT_TYPE_BOXPLOT) cycle
            n = n + 1
            positions(n) = plots(i)%position
            labels(n) = position_label(plots(i)%position)
            if (plots(i)%horizontal) horizontal = .true.
        end do
        if (n == 0) return

        ! A horizontal box plot puts the categories on y instead. Only the axis
        ! actually being written is checked for a caller-supplied override, so
        ! that setting y ticks by hand does not suppress the x categories.
        if (horizontal) then
            if (state%custom_yticks_set .and. .not. state%auto_category_ticks) return
            state%custom_ytick_positions = positions(1:n)
            state%custom_ytick_labels = labels(1:n)
            state%custom_yticks_set = .true.
        else
            if (state%custom_xticks_set .and. .not. state%auto_category_ticks) return
            state%custom_xtick_positions = positions(1:n)
            state%custom_xtick_labels = labels(1:n)
            state%custom_xticks_set = .true.
        end if
        state%auto_category_ticks = .true.
    end subroutine apply_boxplot_category_ticks

    function position_label(position) result(text)
        !! Format a box position the way matplotlib labels it: an integral
        !! position reads '1', not '1.0', and a fractional one keeps only the
        !! digits it needs, so 0.5 reads '0.5' rather than '0.5000'.
        real(wp), intent(in) :: position
        character(len=50) :: text
        integer :: last

        if (abs(position - anint(position)) < 1.0e-9_wp) then
            write (text, '(I0)') nint(position)
            return
        end if

        ! F0.6 drops the leading zero ('.5'), which reads as a typo on an axis.
        write (text, '(F0.6)') position
        text = adjustl(text)
        if (text(1:1) == '.') then
            text = '0'//trim(text)
        else if (len_trim(text) > 1) then
            if (text(1:2) == '-.') text = '-0'//trim(text(2:))
        end if
        last = len_trim(text)
        do while (last > 1)
            if (text(last:last) /= '0') exit
            text(last:last) = ' '
            last = last - 1
        end do
        if (text(last:last) == '.') text(last:last) = ' '
        text = adjustl(text)
    end function position_label

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
