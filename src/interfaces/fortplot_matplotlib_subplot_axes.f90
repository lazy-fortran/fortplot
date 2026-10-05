module fortplot_matplotlib_subplot_axes
    !! Stateful pyplot axes properties belong to the selected subplot.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    implicit none
    private
    public :: subplot_axes_limits, subplot_axes_legend
contains
    logical function selected_subplot(row, col) result(selected)
        integer, intent(out) :: row, col
        integer :: rows, cols, index
        selected = .false.
        row = 0; col = 0
        rows = fig%subplot_rows; cols = fig%subplot_cols
        index = fig%current_subplot
        if (rows <= 0 .or. cols <= 0) return
        if (index < 1 .or. index > rows*cols) return
        if (.not. allocated(fig%subplots_array)) return
        row = (index - 1)/cols + 1
        col = mod(index - 1, cols) + 1
        selected = .true.
    end function

    logical function subplot_axes_limits(lower, upper, is_x) result(selected)
        real(wp), intent(in) :: lower, upper
        logical, intent(in) :: is_x
        integer :: row, col
        selected = selected_subplot(row, col)
        if (.not. selected) return
        associate (axes => fig%subplots_array(row, col))
            if (is_x) then
                axes%x_min = lower; axes%x_max = upper; axes%xlim_set = .true.
            else
                axes%y_min = lower; axes%y_max = upper; axes%ylim_set = .true.
            end if
        end associate
        fig%state%rendered = .false.
    end function

    logical function subplot_axes_legend(location) result(selected)
        character(len=*), intent(in), optional :: location
        integer :: row, col
        selected = selected_subplot(row, col)
        if (.not. selected) return
        associate (axes => fig%subplots_array(row, col))
            axes%show_legend = .true.
            axes%legend_location = 'best'
            if (present(location)) axes%legend_location = location
        end associate
        fig%state%rendered = .false.
    end function
end module
