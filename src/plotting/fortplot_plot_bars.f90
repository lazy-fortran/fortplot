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
    use fortplot_figure_core_ranges, only: update_data_ranges_figure
    implicit none
    
    private
    public :: bar_impl, barh_impl
    
contains
    
    subroutine bar_impl(self, x, heights, width, bottom, label, color, edgecolor)
        !! Add vertical bar plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), heights(:)
        real(wp), intent(in), optional :: width
        real(wp), intent(in), optional :: bottom(:)
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: edgecolor(3)

        call add_bar_plot_data(self, x, heights, width, bottom, label, color, edgecolor, &
                               horizontal=.false.)
    end subroutine bar_impl
    
    subroutine barh_impl(self, y, widths, height, left, label, color, edgecolor)
        !! Add horizontal bar plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y(:), widths(:)
        real(wp), intent(in), optional :: height
        real(wp), intent(in), optional :: left(:)
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: edgecolor(3)

        call add_bar_plot_data(self, y, widths, height, left, label, color, edgecolor, &
                               horizontal=.true.)
    end subroutine barh_impl
    
    ! Private helper subroutines
    
    subroutine add_bar_plot_data(self, positions, values, bar_size, bottom, label, color, &
                                 edgecolor, horizontal)
        !! Add bar plot data (handles both vertical and horizontal)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: positions(:), values(:)
        real(wp), intent(in), optional :: bar_size
        real(wp), intent(in), optional :: bottom(:)
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: edgecolor(3)
        logical, intent(in) :: horizontal

        integer :: plot_idx, color_idx, i, n
        real(wp), parameter :: DEFAULT_BAR_WIDTH = 0.8_wp
        real(wp), parameter :: HUGE_SPACING = huge(1.0_wp)
        real(wp) :: min_spacing

        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count

        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if

        self%plots(plot_idx)%plot_type = PLOT_TYPE_BAR
        n = size(positions)

        allocate(self%plots(plot_idx)%bar_x(n))
        allocate(self%plots(plot_idx)%bar_heights(size(values)))
        allocate(self%plots(plot_idx)%bar_bottom(n))

        self%plots(plot_idx)%bar_x = positions
        self%plots(plot_idx)%bar_heights = values

        ! Set bottom offset (for vertical bars) or left offset (for horizontal bars)
        if (present(bottom)) then
            if (size(bottom) == n) then
                self%plots(plot_idx)%bar_bottom = bottom
            else if (size(bottom) == 1) then
                self%plots(plot_idx)%bar_bottom = bottom(1)
            else
                self%plots(plot_idx)%bar_bottom = 0.0_wp
            end if
        else
            self%plots(plot_idx)%bar_bottom = 0.0_wp
        end if
        
        if (present(bar_size)) then
            self%plots(plot_idx)%bar_width = abs(bar_size)
            if (self%plots(plot_idx)%bar_width <= 0.0_wp) then
                self%plots(plot_idx)%bar_width = DEFAULT_BAR_WIDTH
            end if
        else
            ! Default bar width calculation
            if (size(positions) > 1) then
                min_spacing = HUGE_SPACING
                do i = 1, size(positions) - 1
                    min_spacing = min(min_spacing, abs(positions(i + 1) - positions(i)))
                end do
                if (min_spacing <= 0.0_wp .or. min_spacing == HUGE_SPACING) then
                    self%plots(plot_idx)%bar_width = DEFAULT_BAR_WIDTH
                else
                    self%plots(plot_idx)%bar_width = 0.8_wp * min_spacing
                end if
            else
                self%plots(plot_idx)%bar_width = DEFAULT_BAR_WIDTH
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

        if (present(edgecolor)) then
            self%plots(plot_idx)%bar_edgecolor = edgecolor
            self%plots(plot_idx)%bar_edgecolor_set = .true.
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if

        self%state%plot_count = self%plot_count
        call update_data_ranges_figure(self%plots, self%state, self%plot_count)
    end subroutine add_bar_plot_data

end module fortplot_plot_bars
