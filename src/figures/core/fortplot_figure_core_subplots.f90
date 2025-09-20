module fortplot_figure_core_subplots
    !! Subplot management methods for figure_t type
    !! Contains all subplot-related operations

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: subplot_data_t
    use fortplot_figure_subplots, only: create_subplots, add_subplot_plot, &
                                       get_subplot_plot_count, &
                                       set_subplot_title, &
                                       set_subplot_xlabel, &
                                       set_subplot_ylabel, &
                                       get_subplot_title
    use fortplot_figure_initialization, only: figure_state_t
    implicit none

    private
    public :: figure_subplots_wrapper, figure_subplot_plot_wrapper
    public :: figure_subplot_plot_count_wrapper, figure_subplot_set_title_wrapper
    public :: figure_subplot_set_xlabel_wrapper, figure_subplot_set_ylabel_wrapper
    public :: figure_subplot_title_wrapper

contains

    subroutine figure_subplots_wrapper(subplots_array, subplot_rows, &
                                      subplot_cols, current_subplot, nrows, ncols)
        type(subplot_data_t), intent(inout), allocatable :: subplots_array(:,:)
        integer, intent(inout) :: subplot_rows, subplot_cols, current_subplot
        integer, intent(in) :: nrows, ncols

        logical :: subplot_active
        call create_subplots(subplots_array, subplot_rows, &
                             subplot_cols, nrows, ncols, subplot_active)
    end subroutine figure_subplots_wrapper

    subroutine figure_subplot_plot_wrapper(subplots_array, subplot_rows, &
                                          subplot_cols, row, col, x, y, label, &
                                          linestyle, color, colors, max_colors)
        type(subplot_data_t), intent(inout), allocatable :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols, row, col, max_colors
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in) :: colors(:,:)

        call add_subplot_plot(subplots_array, subplot_rows, &
                              subplot_cols, row, col, x, y, label, &
                              linestyle, color, colors, max_colors)
    end subroutine figure_subplot_plot_wrapper

    function figure_subplot_plot_count_wrapper(subplots_array, subplot_rows, &
                                              subplot_cols, row, col) result(count)
        type(subplot_data_t), intent(in), allocatable :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols, row, col
        integer :: count

        count = get_subplot_plot_count(subplots_array, subplot_rows, &
                                       subplot_cols, row, col)
    end function figure_subplot_plot_count_wrapper

    subroutine figure_subplot_set_title_wrapper(subplots_array, subplot_rows, &
                                               subplot_cols, row, col, title)
        type(subplot_data_t), intent(inout), allocatable :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols, row, col
        character(len=*), intent(in) :: title

        call set_subplot_title(subplots_array, subplot_rows, &
                               subplot_cols, row, col, title)
    end subroutine figure_subplot_set_title_wrapper

    subroutine figure_subplot_set_xlabel_wrapper(subplots_array, subplot_rows, &
                                                subplot_cols, row, col, xlabel)
        type(subplot_data_t), intent(inout), allocatable :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols, row, col
        character(len=*), intent(in) :: xlabel

        call set_subplot_xlabel(subplots_array, subplot_rows, &
                                subplot_cols, row, col, xlabel)
    end subroutine figure_subplot_set_xlabel_wrapper

    subroutine figure_subplot_set_ylabel_wrapper(subplots_array, subplot_rows, &
                                                subplot_cols, row, col, ylabel)
        type(subplot_data_t), intent(inout), allocatable :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols, row, col
        character(len=*), intent(in) :: ylabel

        call set_subplot_ylabel(subplots_array, subplot_rows, &
                                subplot_cols, row, col, ylabel)
    end subroutine figure_subplot_set_ylabel_wrapper

    function figure_subplot_title_wrapper(subplots_array, subplot_rows, &
                                         subplot_cols, row, col) result(title)
        type(subplot_data_t), intent(in), allocatable :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols, row, col
        character(len=:), allocatable :: title

        title = get_subplot_title(subplots_array, subplot_rows, &
                                  subplot_cols, row, col)
    end function figure_subplot_title_wrapper

end module fortplot_figure_core_subplots