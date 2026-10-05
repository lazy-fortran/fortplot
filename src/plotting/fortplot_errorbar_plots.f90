module fortplot_errorbar_plots
    !! Error bar plot operations module
    !! 
    !! This module handles error bar plotting functionality including
    !! symmetric and asymmetric error bars for both X and Y directions.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_ERRORBAR
    use fortplot_format_parser, only: parse_format_string
    use fortplot_colors, only: parse_color

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
                            ecolor, elinewidth, capsize, capthick, color, &
                            linestyle, fmt)
        !! Add error bar plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        real(wp), intent(in), optional :: xerr_lower(:), xerr_upper(:)
        real(wp), intent(in), optional :: yerr_lower(:), yerr_upper(:)
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: ecolor(3)
        real(wp), intent(in), optional :: elinewidth, capsize, capthick
        real(wp), intent(in), optional :: color(3)
        character(len=*), intent(in), optional :: linestyle, fmt
        
        integer :: plot_idx
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        ! Keep the figure-state plot count in sync so legend setup (which reads
        ! state%plot_count) sees this errorbar entry. Mirrors plot()/scatter().
        self%state%plot_count = self%plot_count

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
        
        call configure_errorbar_style(self%plots(plot_idx), &
                                      self%state%colors(:, &
                                      mod(plot_idx - 1, size(self%state%colors, 2)) + 1), &
                                      self%state%current_line_width, label, marker, &
                                      markersize, ecolor, elinewidth, capsize, &
                                      capthick, color, linestyle, fmt)
        call self%relocate_last_plot_to_subplot()
    end subroutine errorbar_impl

    subroutine configure_errorbar_style(plot, default_color, default_linewidth, &
                                        label, marker, markersize, ecolor, elinewidth, &
                                        capsize, capthick, color, linestyle, fmt)
        type(plot_data_t), intent(inout) :: plot
        real(wp), intent(in) :: default_color(3), default_linewidth
        character(len=*), intent(in), optional :: label, marker, linestyle, fmt
        real(wp), intent(in), optional :: markersize, ecolor(3), elinewidth
        real(wp), intent(in), optional :: capsize, capthick, color(3)
        character(len=20) :: parsed_marker, parsed_linestyle, parsed_color
        real(wp) :: format_color(3)
        logical :: valid_color

        plot%capsize = 0.0_wp
        if (present(capsize)) plot%capsize = max(0.0_wp, capsize)
        plot%elinewidth = default_linewidth
        if (present(elinewidth)) plot%elinewidth = max(0.0_wp, elinewidth)
        ! Cap thickness follows lines.markeredgewidth, independently of elinewidth.
        plot%capthick = 1.0_wp
        if (present(capthick)) plot%capthick = max(0.0_wp, capthick)

        plot%linestyle = '-'
        plot%color = default_color
        if (present(fmt)) then
            if (trim(fmt) == 'none' .or. trim(fmt) == 'None') then
                plot%linestyle = 'none'
            else
                call parse_format_string(fmt, parsed_marker, parsed_linestyle, &
                                         parsed_color)
                if (len_trim(parsed_marker) > 0) plot%marker = parsed_marker
                if (len_trim(parsed_linestyle) > 0) plot%linestyle = parsed_linestyle
                if (len_trim(parsed_color) > 0) then
                    call parse_color(parsed_color, format_color, valid_color)
                    if (valid_color) plot%color = format_color
                end if
            end if
        end if
        if (present(marker)) plot%marker = marker
        if (present(markersize)) then
            plot%scatter_size_default = max(0.0_wp, markersize)**2
        end if
        if (present(linestyle)) plot%linestyle = linestyle
        if (present(color)) plot%color = color
        if (present(ecolor)) then
            plot%errorbar_color = ecolor
            plot%errorbar_color_set = .true.
        end if
        if (present(label)) then
            if (len_trim(label) > 0) plot%label = label
        end if
    end subroutine configure_errorbar_style

end module fortplot_errorbar_plots
