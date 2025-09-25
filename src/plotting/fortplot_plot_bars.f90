module fortplot_plot_bars
    !! Bar chart plotting functionality
    !! 
    !! Provides:
    !! - Vertical bar charts (bar)
    !! - Horizontal bar charts (barh)
    !! - Automatic bar width calculation
    !! - Color management for bar plots
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_BAR
    implicit none
    
    private
    public :: bar_impl, barh_impl
    
contains
    
    subroutine bar_impl(self, x, heights, width, label, color)
        !! Add vertical bar plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), heights(:)
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        call add_bar_plot_data(self, x, heights, width, label, color, horizontal=.false.)
    end subroutine bar_impl
    
    subroutine barh_impl(self, y, widths, height, label, color)
        !! Add horizontal bar plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y(:), widths(:)
        real(wp), intent(in), optional :: height
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        call add_bar_plot_data(self, y, widths, height, label, color, horizontal=.true.)
    end subroutine barh_impl
    
    ! Private helper subroutines
    
    subroutine add_bar_plot_data(self, positions, values, bar_size, label, color, horizontal)
        !! Add bar plot data (handles both vertical and horizontal)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: positions(:), values(:)
        real(wp), intent(in), optional :: bar_size
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        logical, intent(in) :: horizontal
        
        integer :: plot_idx, color_idx
        
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_BAR
        
        allocate(self%plots(plot_idx)%bar_x(size(positions)))
        allocate(self%plots(plot_idx)%bar_heights(size(values)))
        
        self%plots(plot_idx)%bar_x = positions
        self%plots(plot_idx)%bar_heights = values
        
        if (present(bar_size)) then
            self%plots(plot_idx)%bar_width = bar_size
        else
            ! Default bar width calculation
            if (size(positions) > 1) then
                self%plots(plot_idx)%bar_width = 0.8_wp * minval(positions(2:) - positions(:size(positions)-1))
            else
                self%plots(plot_idx)%bar_width = 0.8_wp
            end if
        end if
        
        self%plots(plot_idx)%bar_horizontal = horizontal
        
        if (present(color)) then
            self%plots(plot_idx)%color = color
        else
            ! Default color cycling
            color_idx = mod(plot_idx - 1, 6) + 1
            self%plots(plot_idx)%color = self%state%colors(:, color_idx)
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if

        self%state%plot_count = self%plot_count
    end subroutine add_bar_plot_data

end module fortplot_plot_bars
