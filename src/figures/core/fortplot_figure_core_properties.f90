module fortplot_figure_core_properties
    !! Property management methods for figure_t type
    !! Contains setters and getters for figure properties

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t
    implicit none

    private
    public :: figure_set_xlabel, figure_set_ylabel, figure_set_title
    public :: figure_set_xscale, figure_set_yscale
    public :: figure_set_xlim, figure_set_ylim, figure_set_line_width
    public :: figure_set_ydata, figure_get_width, figure_get_height
    public :: figure_get_rendered, figure_set_rendered, figure_get_plot_count
    public :: figure_get_plots, figure_get_x_min, figure_get_x_max
    public :: figure_get_y_min, figure_get_y_max

contains

    subroutine figure_set_xlabel(xlabel_var, label)
        character(len=:), allocatable, intent(inout) :: xlabel_var
        character(len=*), intent(in) :: label
        xlabel_var = label
    end subroutine figure_set_xlabel

    subroutine figure_set_ylabel(ylabel_var, label)
        character(len=:), allocatable, intent(inout) :: ylabel_var
        character(len=*), intent(in) :: label
        ylabel_var = label
    end subroutine figure_set_ylabel

    subroutine figure_set_title(title_var, title)
        character(len=:), allocatable, intent(inout) :: title_var
        character(len=*), intent(in) :: title
        title_var = title
    end subroutine figure_set_title

    subroutine figure_set_xscale(state, scale, threshold)
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold

        state%xscale = scale
        if (present(threshold)) then
            state%symlog_threshold = threshold
        end if
    end subroutine figure_set_xscale

    subroutine figure_set_yscale(state, scale, threshold)
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold

        state%yscale = scale
        if (present(threshold)) then
            state%symlog_threshold = threshold
        end if
    end subroutine figure_set_yscale

    subroutine figure_set_xlim(state, x_min, x_max)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_min, x_max
        state%x_min = x_min
        state%x_max = x_max
        state%xlim_set = .true.
    end subroutine figure_set_xlim

    subroutine figure_set_ylim(state, y_min, y_max)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: y_min, y_max
        state%y_min = y_min
        state%y_max = y_max
        state%ylim_set = .true.
    end subroutine figure_set_ylim

    subroutine figure_set_line_width(state, width)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: width
        state%current_line_width = width
    end subroutine figure_set_line_width

    subroutine figure_set_ydata(plots, plot_count, plot_index, y_data)
        type(plot_data_t), intent(inout), allocatable :: plots(:)
        integer, intent(in) :: plot_count, plot_index
        real(wp), intent(in) :: y_data(:)

        if (plot_index < 1 .or. plot_index > plot_count) return
        if (.not. allocated(plots)) return

        if (allocated(plots(plot_index)%y)) deallocate(plots(plot_index)%y)
        allocate(plots(plot_index)%y(size(y_data)))
        plots(plot_index)%y = y_data
    end subroutine figure_set_ydata

    function figure_get_width(state) result(width)
        type(figure_state_t), intent(in) :: state
        integer :: width
        width = state%width
    end function figure_get_width

    function figure_get_height(state) result(height)
        type(figure_state_t), intent(in) :: state
        integer :: height
        height = state%height
    end function figure_get_height

    function figure_get_rendered(state) result(rendered)
        type(figure_state_t), intent(in) :: state
        logical :: rendered
        rendered = state%rendered
    end function figure_get_rendered

    subroutine figure_set_rendered(state, rendered)
        type(figure_state_t), intent(inout) :: state
        logical, intent(in) :: rendered
        state%rendered = rendered
    end subroutine figure_set_rendered

    function figure_get_plot_count(plot_count) result(count)
        integer, intent(in) :: plot_count
        integer :: count
        count = plot_count
    end function figure_get_plot_count

    function figure_get_plots(plots) result(plot_array)
        type(plot_data_t), intent(in), allocatable :: plots(:)
        type(plot_data_t), allocatable :: plot_array(:)

        if (allocated(plots)) then
            allocate(plot_array(size(plots)))
            plot_array = plots
        end if
    end function figure_get_plots

    function figure_get_x_min(plots, plot_count) result(x_min)
        type(plot_data_t), intent(in), allocatable :: plots(:)
        integer, intent(in) :: plot_count
        real(wp) :: x_min

        integer :: i
        logical :: first_valid

        x_min = 0.0_wp
        first_valid = .true.

        if (.not. allocated(plots) .or. plot_count == 0) return

        do i = 1, plot_count
            if (allocated(plots(i)%x)) then
                if (first_valid) then
                    x_min = minval(plots(i)%x)
                    first_valid = .false.
                else
                    x_min = min(x_min, minval(plots(i)%x))
                end if
            end if
        end do
    end function figure_get_x_min

    function figure_get_x_max(plots, plot_count) result(x_max)
        type(plot_data_t), intent(in), allocatable :: plots(:)
        integer, intent(in) :: plot_count
        real(wp) :: x_max

        integer :: i
        logical :: first_valid

        x_max = 1.0_wp
        first_valid = .true.

        if (.not. allocated(plots) .or. plot_count == 0) return

        do i = 1, plot_count
            if (allocated(plots(i)%x)) then
                if (first_valid) then
                    x_max = maxval(plots(i)%x)
                    first_valid = .false.
                else
                    x_max = max(x_max, maxval(plots(i)%x))
                end if
            end if
        end do
    end function figure_get_x_max

    function figure_get_y_min(plots, plot_count) result(y_min)
        type(plot_data_t), intent(in), allocatable :: plots(:)
        integer, intent(in) :: plot_count
        real(wp) :: y_min

        integer :: i
        logical :: first_valid

        y_min = 0.0_wp
        first_valid = .true.

        if (.not. allocated(plots) .or. plot_count == 0) return

        do i = 1, plot_count
            if (allocated(plots(i)%y)) then
                if (first_valid) then
                    y_min = minval(plots(i)%y)
                    first_valid = .false.
                else
                    y_min = min(y_min, minval(plots(i)%y))
                end if
            end if
        end do
    end function figure_get_y_min

    function figure_get_y_max(plots, plot_count) result(y_max)
        type(plot_data_t), intent(in), allocatable :: plots(:)
        integer, intent(in) :: plot_count
        real(wp) :: y_max

        integer :: i
        logical :: first_valid

        y_max = 1.0_wp
        first_valid = .true.

        if (.not. allocated(plots) .or. plot_count == 0) return

        do i = 1, plot_count
            if (allocated(plots(i)%y)) then
                if (first_valid) then
                    y_max = maxval(plots(i)%y)
                    first_valid = .false.
                else
                    y_max = max(y_max, maxval(plots(i)%y))
                end if
            end if
        end do
    end function figure_get_y_max

end module fortplot_figure_core_properties