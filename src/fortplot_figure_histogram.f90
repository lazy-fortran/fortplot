module fortplot_figure_histogram
    !! Figure histogram functionality module
    !! 
    !! Single Responsibility: Handle histogram calculation and visualization
    !! Extracted from fortplot_figure_core to improve modularity
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: calculate_histogram_bins, create_histogram_line_data
    
contains
    
    subroutine calculate_histogram_bins(data, n_bins, normalize_density, &
                                       bin_edges, bin_counts)
        !! Calculate histogram bin edges and counts from data
        real(wp), intent(in) :: data(:)
        integer, intent(in) :: n_bins
        logical, intent(in) :: normalize_density
        real(wp), allocatable, intent(out) :: bin_edges(:), bin_counts(:)
        
        integer :: i, bin_index, n_data
        real(wp) :: data_min, data_max, bin_width
        real(wp) :: total_area
        
        n_data = size(data)
        
        ! Find data range
        data_min = minval(data)
        data_max = maxval(data)
        
        ! Handle case where all data points are the same
        if (abs(data_max - data_min) < epsilon(1.0_wp)) then
            data_min = data_min - 0.5_wp
            data_max = data_max + 0.5_wp
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
            bin_index = min(n_bins, max(1, int((data(i) - data_min) / bin_width) + 1))
            bin_counts(bin_index) = bin_counts(bin_index) + 1.0_wp
        end do
        
        ! Normalize for density if requested
        if (normalize_density) then
            total_area = real(n_data, wp) * bin_width
            bin_counts = bin_counts / total_area
        end if
        
    end subroutine calculate_histogram_bins
    
    subroutine create_histogram_line_data(bin_edges, bin_counts, x_data, y_data)
        !! Create line data for histogram visualization as connected rectangles
        real(wp), intent(in) :: bin_edges(:), bin_counts(:)
        real(wp), allocatable, intent(out) :: x_data(:), y_data(:)
        
        integer :: i, n_bins
        
        n_bins = size(bin_counts)
        allocate(x_data(4 * n_bins + 1), y_data(4 * n_bins + 1))
        
        ! Create line segments for each bar
        do i = 1, n_bins
            ! Bottom left corner
            x_data(4*(i-1) + 1) = bin_edges(i)
            y_data(4*(i-1) + 1) = 0.0_wp
            
            ! Top left corner
            x_data(4*(i-1) + 2) = bin_edges(i)
            y_data(4*(i-1) + 2) = bin_counts(i)
            
            ! Top right corner
            x_data(4*(i-1) + 3) = bin_edges(i + 1)
            y_data(4*(i-1) + 3) = bin_counts(i)
            
            ! Bottom right corner
            x_data(4*(i-1) + 4) = bin_edges(i + 1)
            y_data(4*(i-1) + 4) = 0.0_wp
        end do
        
        ! Close the path back to origin
        x_data(4 * n_bins + 1) = bin_edges(1)
        y_data(4 * n_bins + 1) = 0.0_wp
        
    end subroutine create_histogram_line_data

end module fortplot_figure_histogram