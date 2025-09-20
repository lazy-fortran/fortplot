module fortplot_figure_core_animation
    !! Animation and backend-specific methods for figure_t type
    !! Contains methods for animation support and backend delegation

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t, arrow_data_t
    use fortplot_figure_comprehensive_operations, only: figure_backend_color, &
                                                       figure_backend_associated, &
                                                       figure_backend_line
    implicit none

    private
    public :: figure_setup_png_backend_for_animation
    public :: figure_extract_rgb_data_for_animation, figure_extract_png_data_for_animation
    public :: figure_backend_color_wrapper, figure_backend_line_wrapper
    public :: figure_backend_arrow_wrapper, figure_backend_associated_wrapper
    public :: figure_twinx, figure_twiny

contains

    subroutine figure_setup_png_backend_for_animation(state, plots, plot_count, &
                                                     width, height, title, &
                                                     xlabel, ylabel)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in), allocatable :: plots(:)
        integer, intent(in) :: plot_count
        integer, intent(in) :: width, height
        character(len=*), intent(in) :: title, xlabel, ylabel

        state%width = width
        state%height = height
        ! Note: Backend setup should be done separately
        state%rendered = .false.
    end subroutine figure_setup_png_backend_for_animation

    function figure_extract_rgb_data_for_animation(state, plots, plot_count) result(rgb_data)
        type(figure_state_t), intent(in) :: state
        type(plot_data_t), intent(in), allocatable :: plots(:)
        integer, intent(in) :: plot_count
        integer, allocatable :: rgb_data(:,:,:)

        ! Extract RGB data from rendered figure
        ! This would interface with the actual backend rendering
        allocate(rgb_data(3, state%width, state%height))
        rgb_data = 255  ! White background as placeholder
    end function figure_extract_rgb_data_for_animation

    function figure_extract_png_data_for_animation(state, plots, plot_count) result(png_data)
        type(figure_state_t), intent(in) :: state
        type(plot_data_t), intent(in), allocatable :: plots(:)
        integer, intent(in) :: plot_count
        integer(1), allocatable :: png_data(:)

        ! Extract PNG binary data from rendered figure
        ! This would interface with the actual backend rendering
        allocate(png_data(1024))  ! Placeholder size
        png_data = 0
    end function figure_extract_png_data_for_animation

    subroutine figure_backend_color_wrapper(state, r, g, b)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: r, g, b
        call figure_backend_color(state, r, g, b)
    end subroutine figure_backend_color_wrapper

    subroutine figure_backend_line_wrapper(state, x1, y1, x2, y2)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x1, y1, x2, y2
        call figure_backend_line(state, x1, y1, x2, y2)
    end subroutine figure_backend_line_wrapper

    subroutine figure_backend_arrow_wrapper(state, arrow_data, plot_count)
        type(figure_state_t), intent(inout) :: state
        type(arrow_data_t), intent(in), allocatable :: arrow_data(:)
        integer, intent(in) :: plot_count

        ! Implementation for drawing arrows using backend
        ! This would delegate to the appropriate backend
    end subroutine figure_backend_arrow_wrapper

    function figure_backend_associated_wrapper(state) result(associated)
        type(figure_state_t), intent(in) :: state
        logical :: associated
        associated = figure_backend_associated(state)
    end function figure_backend_associated_wrapper

    subroutine figure_twinx(state, plots, plot_count)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout), allocatable :: plots(:)
        integer, intent(inout) :: plot_count

        ! Create a twin x-axis
        ! Note: Twin axis support not yet implemented in figure_state_t
    end subroutine figure_twinx

    subroutine figure_twiny(state, plots, plot_count)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout), allocatable :: plots(:)
        integer, intent(inout) :: plot_count

        ! Create a twin y-axis
        ! Note: Twin axis support not yet implemented in figure_state_t
    end subroutine figure_twiny

end module fortplot_figure_core_animation