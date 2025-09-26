module fortplot_errorbar_plots
    !! Error bar plot operations module
    !! 
    !! This module handles error bar plotting functionality including
    !! symmetric and asymmetric error bars for both X and Y directions.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: PLOT_TYPE_ERRORBAR

    implicit none

    private
    public :: errorbar_impl

    interface errorbar
        module procedure errorbar_impl
    end interface
    public :: errorbar

contains

    subroutine errorbar_impl(self, x, y, xerr, yerr, xerr_lower, xerr_upper, &
                            yerr_lower, yerr_upper, label, marker, markersize, &
                            ecolor, elinewidth, capsize, capthick, color)
        !! Add error bar plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        real(wp), intent(in), optional :: xerr_lower(:), xerr_upper(:)
        real(wp), intent(in), optional :: yerr_lower(:), yerr_upper(:)
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: ecolor(3)
        real(wp), intent(in), optional :: elinewidth, capsize, capthick
        real(wp), intent(in), optional :: color(3)
        
        integer :: plot_idx, color_idx
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_ERRORBAR
        
        allocate(self%plots(plot_idx)%x(size(x)))
        allocate(self%plots(plot_idx)%y(size(y)))
        self%plots(plot_idx)%x = x
        self%plots(plot_idx)%y = y
        
        ! Handle symmetric error bars
        if (present(xerr)) then
            allocate(self%plots(plot_idx)%xerr(size(xerr)))
            self%plots(plot_idx)%xerr = xerr
            self%plots(plot_idx)%has_xerr = .true.
        end if
        
        if (present(yerr)) then
            allocate(self%plots(plot_idx)%yerr(size(yerr)))
            self%plots(plot_idx)%yerr = yerr
            self%plots(plot_idx)%has_yerr = .true.
        end if
        
        ! Handle asymmetric error bars
        if (present(xerr_lower) .and. present(xerr_upper)) then
            allocate(self%plots(plot_idx)%xerr_lower(size(xerr_lower)))
            allocate(self%plots(plot_idx)%xerr_upper(size(xerr_upper)))
            self%plots(plot_idx)%xerr_lower = xerr_lower
            self%plots(plot_idx)%xerr_upper = xerr_upper
            self%plots(plot_idx)%has_xerr = .true.
            self%plots(plot_idx)%asymmetric_xerr = .true.
        end if
        
        if (present(yerr_lower) .and. present(yerr_upper)) then
            allocate(self%plots(plot_idx)%yerr_lower(size(yerr_lower)))
            allocate(self%plots(plot_idx)%yerr_upper(size(yerr_upper)))
            self%plots(plot_idx)%yerr_lower = yerr_lower
            self%plots(plot_idx)%yerr_upper = yerr_upper
            self%plots(plot_idx)%has_yerr = .true.
            self%plots(plot_idx)%asymmetric_yerr = .true.
        end if
        
        if (present(capsize)) then
            self%plots(plot_idx)%capsize = capsize
        end if
        
        if (present(elinewidth)) then
            self%plots(plot_idx)%elinewidth = elinewidth
        end if

        if (present(capthick)) then
            self%plots(plot_idx)%capthick = capthick
        else
            self%plots(plot_idx)%capthick = self%plots(plot_idx)%elinewidth
        end if
        
        if (present(marker)) then
            self%plots(plot_idx)%marker = marker
        else
            self%plots(plot_idx)%marker = 'o'  ! Default to circle marker
        end if
        
        if (present(color)) then
            self%plots(plot_idx)%color = color
        else if (present(ecolor)) then
            self%plots(plot_idx)%color = ecolor
        else
            color_idx = mod(plot_idx - 1, 6) + 1
            self%plots(plot_idx)%color = self%state%colors(:, color_idx)
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if
    end subroutine errorbar_impl

end module fortplot_errorbar_plots
