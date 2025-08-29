module fortplot_figure_subplots
    !! Subplot management functionality for figure_t
    !! 
    !! Extracted from fortplot_figure_core to meet QADS size limits.
    !! Provides grid-based subplot creation and management.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, subplot_data_t
    use fortplot_logging, only: log_error, log_warning
    implicit none
    
    private
    public :: create_subplots
    public :: add_subplot_plot
    public :: get_subplot_plot_count
    public :: set_subplot_title
    public :: set_subplot_xlabel
    public :: set_subplot_ylabel
    public :: get_subplot_title
    
contains
    
    subroutine create_subplots(subplots_array, subplot_rows, subplot_cols, &
                               nrows, ncols, subplot_active)
        !! Create a grid of subplots
        type(subplot_data_t), allocatable, intent(inout) :: subplots_array(:,:)
        integer, intent(out) :: subplot_rows, subplot_cols
        integer, intent(in) :: nrows, ncols
        logical, intent(out) :: subplot_active
        
        integer :: i, j
        
        ! Validate input
        if (nrows <= 0 .or. ncols <= 0) then
            call log_error("subplots: Invalid grid dimensions")
            return
        end if
        
        ! Allocate subplot array
        if (allocated(subplots_array)) then
            deallocate(subplots_array)
        end if
        allocate(subplots_array(nrows, ncols))
        
        ! Initialize each subplot
        do i = 1, nrows
            do j = 1, ncols
                allocate(subplots_array(i,j)%plots(100))
                subplots_array(i,j)%plot_count = 0
                subplots_array(i,j)%max_plots = 100
                subplots_array(i,j)%xlim_set = .false.
                subplots_array(i,j)%ylim_set = .false.
            end do
        end do
        
        subplot_rows = nrows
        subplot_cols = ncols
        subplot_active = .true.
    end subroutine create_subplots
    
    subroutine add_subplot_plot(subplots_array, subplot_rows, subplot_cols, &
                                row, col, x, y, label, linestyle, color, &
                                default_colors, max_colors)
        !! Add a plot to a specific subplot
        type(subplot_data_t), intent(inout) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols
        integer, intent(in) :: row, col
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in) :: default_colors(:,:)
        integer, intent(in) :: max_colors
        
        type(plot_data_t), allocatable :: new_plots(:)
        integer :: idx
        real(wp) :: plot_color(3)
        
        ! Validate indices
        if (row <= 0 .or. row > subplot_rows .or. &
            col <= 0 .or. col > subplot_cols) then
            call log_error("subplot_plot: Invalid subplot indices")
            return
        end if
        
        ! Check array dimensions
        if (size(x) /= size(y)) then
            call log_error("subplot_plot: x and y arrays must have same size")
            return
        end if
        
        ! Resize plot array if needed
        if (subplots_array(row, col)%plot_count >= &
            subplots_array(row, col)%max_plots) then
            allocate(new_plots(subplots_array(row, col)%max_plots * 2))
            new_plots(1:subplots_array(row, col)%plot_count) = &
                subplots_array(row, col)%plots(1:subplots_array(row, col)%plot_count)
            call move_alloc(new_plots, subplots_array(row, col)%plots)
            subplots_array(row, col)%max_plots = &
                subplots_array(row, col)%max_plots * 2
        end if
        
        ! Add plot
        idx = subplots_array(row, col)%plot_count + 1
        subplots_array(row, col)%plot_count = idx
        
        allocate(subplots_array(row, col)%plots(idx)%x(size(x)))
        allocate(subplots_array(row, col)%plots(idx)%y(size(y)))
        subplots_array(row, col)%plots(idx)%x = x
        subplots_array(row, col)%plots(idx)%y = y
        
        ! Set plot properties
        if (present(label)) then
            subplots_array(row, col)%plots(idx)%label = label
        else
            subplots_array(row, col)%plots(idx)%label = ''
        end if
        
        if (present(linestyle)) then
            subplots_array(row, col)%plots(idx)%linestyle = linestyle
        else
            subplots_array(row, col)%plots(idx)%linestyle = '-'
        end if
        
        if (present(color)) then
            plot_color = color
        else
            plot_color = default_colors(:, mod(idx - 1, max_colors) + 1)
        end if
        subplots_array(row, col)%plots(idx)%color = plot_color
        
        ! Update data ranges
        call update_subplot_ranges(subplots_array(row, col), x, y)
    end subroutine add_subplot_plot
    
    subroutine update_subplot_ranges(subplot, x, y)
        !! Update data ranges for a subplot
        type(subplot_data_t), intent(inout) :: subplot
        real(wp), intent(in) :: x(:), y(:)
        
        if (subplot%plot_count == 1) then
            ! First plot - initialize ranges
            subplot%x_min = minval(x)
            subplot%x_max = maxval(x)
            subplot%y_min = minval(y)
            subplot%y_max = maxval(y)
        else
            ! Update ranges
            subplot%x_min = min(subplot%x_min, minval(x))
            subplot%x_max = max(subplot%x_max, maxval(x))
            subplot%y_min = min(subplot%y_min, minval(y))
            subplot%y_max = max(subplot%y_max, maxval(y))
        end if
    end subroutine update_subplot_ranges
    
    function get_subplot_plot_count(subplots_array, subplot_rows, &
                                   subplot_cols, row, col) result(count)
        !! Get the number of plots in a specific subplot
        type(subplot_data_t), intent(in) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols
        integer, intent(in) :: row, col
        integer :: count
        
        ! Validate indices
        if (row <= 0 .or. row > subplot_rows .or. &
            col <= 0 .or. col > subplot_cols) then
            count = 0
            return
        end if
        
        count = subplots_array(row, col)%plot_count
    end function get_subplot_plot_count
    
    subroutine set_subplot_title(subplots_array, subplot_rows, &
                                 subplot_cols, row, col, title)
        !! Set the title for a specific subplot
        type(subplot_data_t), intent(inout) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: title
        
        ! Validate indices
        if (row <= 0 .or. row > subplot_rows .or. &
            col <= 0 .or. col > subplot_cols) then
            call log_error("subplot_set_title: Invalid subplot indices")
            return
        end if
        
        subplots_array(row, col)%title = title
    end subroutine set_subplot_title
    
    subroutine set_subplot_xlabel(subplots_array, subplot_rows, &
                                  subplot_cols, row, col, xlabel)
        !! Set the x-axis label for a specific subplot
        type(subplot_data_t), intent(inout) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: xlabel
        
        ! Validate indices
        if (row <= 0 .or. row > subplot_rows .or. &
            col <= 0 .or. col > subplot_cols) then
            call log_error("subplot_set_xlabel: Invalid subplot indices")
            return
        end if
        
        subplots_array(row, col)%xlabel = xlabel
    end subroutine set_subplot_xlabel
    
    subroutine set_subplot_ylabel(subplots_array, subplot_rows, &
                                  subplot_cols, row, col, ylabel)
        !! Set the y-axis label for a specific subplot
        type(subplot_data_t), intent(inout) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: ylabel
        
        ! Validate indices
        if (row <= 0 .or. row > subplot_rows .or. &
            col <= 0 .or. col > subplot_cols) then
            call log_error("subplot_set_ylabel: Invalid subplot indices")
            return
        end if
        
        subplots_array(row, col)%ylabel = ylabel
    end subroutine set_subplot_ylabel
    
    function get_subplot_title(subplots_array, subplot_rows, &
                               subplot_cols, row, col) result(title)
        !! Get the title for a specific subplot
        type(subplot_data_t), intent(in) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols
        integer, intent(in) :: row, col
        character(len=:), allocatable :: title
        
        ! Validate indices
        if (row <= 0 .or. row > subplot_rows .or. &
            col <= 0 .or. col > subplot_cols) then
            title = ""
            return
        end if
        
        if (allocated(subplots_array(row, col)%title)) then
            title = subplots_array(row, col)%title
        else
            title = ""
        end if
    end function get_subplot_title
    
end module fortplot_figure_subplots