module fortplot_matplotlib_axes
    !! Axis operations and annotations for matplotlib-compatible API
    !! Contains axis limits, labels, scales, text, and annotation functions
    
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error, log_warning, log_info
    
    implicit none
    private
    
    ! Export axis and annotation functions
    public :: xlabel, ylabel, title, legend
    public :: xlim, ylim
    public :: set_xscale, set_yscale
    public :: set_line_width, set_ydata
    
contains

    subroutine ensure_fig_init()
        !! Internal helper to ensure global figure is initialized
        if (.not. allocated(fig)) then
            allocate(figure_t :: fig)
        end if
        if (.not. fig%backend_associated()) then
            call fig%initialize()
        end if
    end subroutine ensure_fig_init

    subroutine xlabel(label_text)
        !! Set the x-axis label for the global figure (pyplot-style)
        character(len=*), intent(in) :: label_text
        
        call ensure_fig_init()
        call fig%set_xlabel(label_text)
    end subroutine xlabel

    subroutine ylabel(label_text)
        !! Set the y-axis label for the global figure (pyplot-style)
        character(len=*), intent(in) :: label_text
        
        call ensure_fig_init()
        call fig%set_ylabel(label_text)
    end subroutine ylabel

    subroutine title(title_text)
        !! Set the title for the global figure (pyplot-style)
        character(len=*), intent(in) :: title_text
        
        call ensure_fig_init()
        call fig%set_title(title_text)
    end subroutine title

    subroutine legend(position, box, fontsize)
        !! Add a legend to the global figure (pyplot-style)
        character(len=*), intent(in), optional :: position, box
        real(8), intent(in), optional :: fontsize
        
        call ensure_fig_init()
        if (present(position)) then
            call fig%legend(position)
        else
            call fig%legend()
        end if
    end subroutine legend

    subroutine xlim(xmin, xmax)
        !! Set the x-axis limits for the global figure (pyplot-style)
        real(8), intent(in) :: xmin, xmax
        
        call ensure_fig_init()
        call fig%set_xlim(xmin, xmax)
    end subroutine xlim

    subroutine ylim(ymin, ymax)
        !! Set the y-axis limits for the global figure (pyplot-style)
        real(8), intent(in) :: ymin, ymax
        
        call ensure_fig_init()
        call fig%set_ylim(ymin, ymax)
    end subroutine ylim

    subroutine set_xscale(scale, threshold)
        !! Set the x-axis scale type (linear, log, symlog)
        character(len=*), intent(in) :: scale
        real(8), intent(in), optional :: threshold
        
        real(8) :: thresh_val
        
        call ensure_fig_init()
        
        ! Default threshold for symlog scale
        thresh_val = 1.0d-10
        if (present(threshold)) thresh_val = threshold
        
        select case (trim(adjustl(scale)))
        case ('linear', 'log', 'symlog')
            call fig%set_xscale(scale, threshold=thresh_val)
        case default
            call log_error("set_xscale: Invalid scale type: " // trim(scale))
            call log_info("Valid scales are: 'linear', 'log', 'symlog'")
        end select
    end subroutine set_xscale

    subroutine set_yscale(scale, threshold)
        !! Set the y-axis scale type (linear, log, symlog)
        character(len=*), intent(in) :: scale
        real(8), intent(in), optional :: threshold
        
        real(8) :: thresh_val
        
        call ensure_fig_init()
        
        ! Default threshold for symlog scale
        thresh_val = 1.0d-10
        if (present(threshold)) thresh_val = threshold
        
        select case (trim(adjustl(scale)))
        case ('linear', 'log', 'symlog')
            call fig%set_yscale(scale, threshold=thresh_val)
        case default
            call log_error("set_yscale: Invalid scale type: " // trim(scale))
            call log_info("Valid scales are: 'linear', 'log', 'symlog'")
        end select
    end subroutine set_yscale

    subroutine set_line_width(width)
        !! Set the line width for subsequent plot elements
        !! This affects all plot elements added after this call
        real(8), intent(in) :: width
        
        call ensure_fig_init()
        
        if (width <= 0.0d0) then
            call log_error("set_line_width: Width must be positive")
            return
        end if
        
        if (width > 20.0d0) then
            call log_warning("set_line_width: Very large width specified")
        end if
        
        call fig%set_line_width(width)
    end subroutine set_line_width

    subroutine set_ydata(ydata)
        !! Update y-data for the last plot added to the figure
        !! Useful for animation or updating existing plots
        real(8), dimension(:), intent(in) :: ydata
        
        integer :: plot_count
        
        call ensure_fig_init()
        
        ! Get the number of plots to determine the last index
        plot_count = fig%get_plot_count()
        
        if (plot_count == 0) then
            call log_error("set_ydata: No plots to update")
            return
        end if
        
        ! Update the last plot's y-data
        call fig%set_ydata(plot_count, ydata)
    end subroutine set_ydata

end module fortplot_matplotlib_axes