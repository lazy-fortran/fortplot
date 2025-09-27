module fortplot_figure_aspect
    !! Helper routines for enforcing axis aspect ratios in figures

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_PIE
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_scales, only: apply_scale_transform
    implicit none

    private
    public :: contains_pie_plot, enforce_pie_axis_equal, only_pie_plots

contains

    logical function contains_pie_plot(plots, plot_count) result(has_pie)
        !! Detect whether a collection of plots contains at least one pie chart
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        integer :: i, upper

        has_pie = .false.
        upper = min(plot_count, size(plots))
        do i = 1, upper
            if (plots(i)%plot_type == PLOT_TYPE_PIE) then
                has_pie = .true.
                return
            end if
        end do
    end function contains_pie_plot

    logical function only_pie_plots(plots, plot_count) result(all_pie)
        !! Check if every plot in the collection is a pie chart
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        integer :: i, upper

        all_pie = .false.
        if (plot_count <= 0) return
        upper = min(plot_count, size(plots))
        if (upper <= 0) return

        all_pie = .true.
        do i = 1, upper
            if (plots(i)%plot_type /= PLOT_TYPE_PIE) then
                all_pie = .false.
                return
            end if
        end do
    end function only_pie_plots

    subroutine enforce_pie_axis_equal(state, plot_width_px, plot_height_px)
        !! Adjust the figure axis limits so pie charts render with equal scaling
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in), optional :: plot_width_px, plot_height_px

        real(wp), parameter :: EPS = 1.0e-12_wp
        real(wp), parameter :: ASPECT_TOL = 1.0e-9_wp
        real(wp) :: plot_width_local, plot_height_local
        real(wp) :: data_range_x, data_range_y
        real(wp) :: aspect_pixels, range_ratio
        real(wp) :: center_x, center_y
        real(wp) :: adjusted_range

        if (state%xlim_set .or. state%ylim_set) return

        if (present(plot_width_px) .and. present(plot_height_px)) then
            plot_width_local = plot_width_px
            plot_height_local = plot_height_px
        else
            plot_width_local = real(state%width, wp) * &
                               max(0.0_wp, 1.0_wp - state%margin_left - state%margin_right)
            plot_height_local = real(state%height, wp) * &
                                max(0.0_wp, 1.0_wp - state%margin_bottom - state%margin_top)
        end if

        if (plot_width_local <= EPS .or. plot_height_local <= EPS) return

        data_range_x = state%x_max - state%x_min
        data_range_y = state%y_max - state%y_min
        if (data_range_x <= EPS .or. data_range_y <= EPS) return

        aspect_pixels = plot_width_local / plot_height_local
        range_ratio = data_range_x / data_range_y

        if (abs(range_ratio - aspect_pixels) <= ASPECT_TOL) return

        if (range_ratio > aspect_pixels) then
            adjusted_range = data_range_x / aspect_pixels
            center_y = 0.5_wp * (state%y_max + state%y_min)
            state%y_min = center_y - 0.5_wp * adjusted_range
            state%y_max = center_y + 0.5_wp * adjusted_range
        else
            adjusted_range = data_range_y * aspect_pixels
            center_x = 0.5_wp * (state%x_max + state%x_min)
            state%x_min = center_x - 0.5_wp * adjusted_range
            state%x_max = center_x + 0.5_wp * adjusted_range
        end if

        state%x_min_transformed = apply_scale_transform(state%x_min, state%xscale, &
                                                        state%symlog_threshold)
        state%x_max_transformed = apply_scale_transform(state%x_max, state%xscale, &
                                                        state%symlog_threshold)
        state%y_min_transformed = apply_scale_transform(state%y_min, state%yscale, &
                                                        state%symlog_threshold)
        state%y_max_transformed = apply_scale_transform(state%y_max, state%yscale, &
                                                        state%symlog_threshold)
    end subroutine enforce_pie_axis_equal

end module fortplot_figure_aspect
