module fortplot_figure_histogram
    !! Figure histogram functionality module
    !!
    !! Single Responsibility: Handle histogram calculation and visualization
    !! Extracted from fortplot_figure_core to improve modularity

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_plots, only: figure_add_plot
    implicit none

    private
    public :: calculate_histogram_bins, create_histogram_line_data, &
              hist_figure
    
contains
    
    subroutine calculate_histogram_bins(data, n_bins, normalize_density, &
                                       bin_edges, bin_counts, range, &
                                       weights, cumulative)
        !! Calculate histogram bin edges and counts from data.
        !!
        !! Supports optional range clipping, per-sample weights, density
        !! normalisation, and cumulative accumulation.
        real(wp), contiguous, intent(in) :: data(:)
        integer, intent(in) :: n_bins
        logical, intent(in) :: normalize_density
        real(wp), allocatable, intent(out) :: bin_edges(:), bin_counts(:)
        real(wp), intent(in), optional :: range(2)
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: cumulative

        integer :: i, bin_index, n_data
        real(wp) :: data_min, data_max, bin_width
        real(wp) :: total_area

        n_data = size(data)

        if (present(weights)) then
            if (size(weights) /= n_data) then
                return
            end if
        end if

        if (present(range)) then
            data_min = range(1)
            data_max = range(2)
        else
            data_min = minval(data)
            data_max = maxval(data)

            ! Handle case where all data points are the same
            if (abs(data_max - data_min) < epsilon(1.0_wp)) then
                data_min = data_min - 0.5_wp
                data_max = data_max + 0.5_wp
            end if
        end if

        if (data_max <= data_min) then
            ! Empty range — produce zero counts without crashing.
            allocate(bin_edges(n_bins + 1), bin_counts(n_bins))
            bin_edges = 0.0_wp
            bin_counts = 0.0_wp
            return
        end if

        ! Create bin edges
        allocate(bin_edges(n_bins + 1))
        allocate(bin_counts(n_bins))

        bin_width = (data_max - data_min) / real(n_bins, wp)

        do i = 1, n_bins + 1
            bin_edges(i) = data_min + real(i - 1, wp) * bin_width
        end do

        ! Count data points in each bin
        bin_counts = 0.0_wp
        do i = 1, n_data
            if (data(i) < data_min .or. data(i) > data_max) cycle
            bin_index = min(n_bins, max(1, int((data(i) - data_min) / bin_width) + 1))
            if (present(weights)) then
                bin_counts(bin_index) = bin_counts(bin_index) + weights(i)
            else
                bin_counts(bin_index) = bin_counts(bin_index) + 1.0_wp
            end if
        end do

        ! Normalize for density if requested
        if (normalize_density) then
            total_area = sum(bin_counts) * bin_width
            if (total_area > 0.0_wp) then
                bin_counts = bin_counts / total_area
            end if
        end if

        ! Cumulative accumulation
        if (present(cumulative)) then
            if (cumulative) then
                do i = 2, n_bins
                    bin_counts(i) = bin_counts(i) + bin_counts(i - 1)
                end do
            end if
        end if

    end subroutine calculate_histogram_bins
    
    subroutine create_histogram_line_data(bin_edges, bin_counts, x_data, y_data, &
                                          horizontal)
        !! Create line data for histogram visualization as connected rectangles.
        !!
        !! When horizontal=.true., x and y are swapped so bars extend along
        !! the x-axis instead of the y-axis.
        real(wp), contiguous, intent(in) :: bin_edges(:), bin_counts(:)
        real(wp), allocatable, intent(out) :: x_data(:), y_data(:)
        logical, intent(in), optional :: horizontal

        integer :: i, n_bins

        n_bins = size(bin_counts)
        allocate(x_data(4 * n_bins + 1), y_data(4 * n_bins + 1))

        ! Create line segments for each bar
        do i = 1, n_bins
            if (present(horizontal) .and. horizontal) then
                ! Horizontal: bars extend along x-axis
                x_data(4*(i-1) + 1) = 0.0_wp
                y_data(4*(i-1) + 1) = bin_edges(i)

                x_data(4*(i-1) + 2) = bin_counts(i)
                y_data(4*(i-1) + 2) = bin_edges(i)

                x_data(4*(i-1) + 3) = bin_counts(i)
                y_data(4*(i-1) + 3) = bin_edges(i + 1)

                x_data(4*(i-1) + 4) = 0.0_wp
                y_data(4*(i-1) + 4) = bin_edges(i + 1)
            else
                ! Vertical (default): bars extend along y-axis
                x_data(4*(i-1) + 1) = bin_edges(i)
                y_data(4*(i-1) + 1) = 0.0_wp

                x_data(4*(i-1) + 2) = bin_edges(i)
                y_data(4*(i-1) + 2) = bin_counts(i)

                x_data(4*(i-1) + 3) = bin_edges(i + 1)
                y_data(4*(i-1) + 3) = bin_counts(i)

                x_data(4*(i-1) + 4) = bin_edges(i + 1)
                y_data(4*(i-1) + 4) = 0.0_wp
            end if
        end do

        ! Close the path back to origin
        if (present(horizontal) .and. horizontal) then
            x_data(4 * n_bins + 1) = 0.0_wp
            y_data(4 * n_bins + 1) = bin_edges(1)
        else
            x_data(4 * n_bins + 1) = bin_edges(1)
            y_data(4 * n_bins + 1) = 0.0_wp
        end if

    end subroutine create_histogram_line_data

    subroutine hist_figure(plots, state, plot_count, data, bins, density, label, &
                           color, range, weights, cumulative, orientation, alpha)
        !! Add histogram to figure plots array (matplotlib-compatible).
        !!
        !! Accepts the full set of matplotlib hist kwargs so that both the
        !! pyplot facade and the stateful figure method share the same
        !! behaviour.
        type(plot_data_t), intent(inout) :: plots(:)
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

        integer :: n_bins
        logical :: normalize_density, is_horizontal
        real(wp), allocatable :: bin_edges(:), bin_counts(:), x_data(:), y_data(:)

        ! Set defaults
        n_bins = 10
        if (present(bins)) n_bins = bins

        ! Guard against invalid inputs (graceful no-op)
        if (size(data) == 0) then
            return
        end if
        if (n_bins <= 0) then
            return
        end if

        normalize_density = .false.
        if (present(density)) normalize_density = density

        is_horizontal = .false.
        if (present(orientation)) then
            if (trim(orientation) == 'horizontal' .or. &
                trim(orientation) == 'Horizontal' .or. &
                trim(orientation) == 'HORIZONTAL') then
                is_horizontal = .true.
            end if
        end if

        ! Calculate histogram (validated inputs)
        call calculate_histogram_bins(data, n_bins, normalize_density, &
                                      bin_edges, bin_counts, range=range, &
                                      weights=weights, cumulative=cumulative)

        if (.not. allocated(bin_edges)) return

        ! Create line data for visualization
        call create_histogram_line_data(bin_edges, bin_counts, x_data, y_data, &
                                        horizontal=is_horizontal)

        ! Add as line plot
        call figure_add_plot(plots, state, x_data, y_data, label, color=color)
        plot_count = plot_count + 1

        ! Apply alpha to the last-added plot
        if (present(alpha)) then
            if (plot_count >= 1 .and. plot_count <= size(plots)) then
                plots(plot_count)%fill_alpha = max(0.0_wp, min(1.0_wp, alpha))
                plots(plot_count)%marker_face_alpha = plots(plot_count)%fill_alpha
            end if
        end if
    end subroutine hist_figure

end module fortplot_figure_histogram
