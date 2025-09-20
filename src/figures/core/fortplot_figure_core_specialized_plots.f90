module fortplot_figure_core_specialized_plots
    !! Specialized plotting methods for figure_t type
    !! Contains implementations for pie charts, polar plots, and other special plot types

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t
    implicit none

    private
    public :: figure_add_pie, figure_add_polar, figure_add_step, figure_add_stem
    public :: figure_add_fill, figure_add_fill_between, figure_add_imshow

    real(wp), parameter :: PI = 4.0_wp * atan(1.0_wp)

contains

    subroutine figure_add_pie(state, plots, plot_count, values, labels, colors, explode, &
                             radius, center_x, center_y, start_angle)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout), allocatable :: plots(:)
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:)
        real(wp), intent(in), optional :: colors(:,:)
        real(wp), intent(in), optional :: explode(:)
        real(wp), intent(in), optional :: radius, center_x, center_y, start_angle

        integer :: i, j, n, seg_count
        real(wp) :: total, angle_start, angle_span, offset, base_angle
        real(wp) :: cx, cy, r
        real(wp), allocatable :: x_pts(:), y_pts(:)

        n = size(values)
        total = sum(values)
        if (total <= 0.0_wp) return

        cx = 0.5_wp; if (present(center_x)) cx = center_x
        cy = 0.5_wp; if (present(center_y)) cy = center_y
        r = 0.3_wp; if (present(radius)) r = radius
        angle_start = 0.0_wp; if (present(start_angle)) angle_start = start_angle * PI / 180.0_wp

        do i = 1, n
            angle_span = 2.0_wp * PI * values(i) / total
            seg_count = max(12, int(abs(angle_span) * 180.0_wp / PI) + 1)
            allocate(x_pts(seg_count + 2), y_pts(seg_count + 2))

            offset = 0.0_wp
            if (present(explode)) then
                if (i <= size(explode)) offset = explode(i)
            end if
            offset = offset * r * 0.1_wp

            base_angle = angle_start + 0.5_wp * angle_span
            x_pts(1) = cx + offset * cos(base_angle)
            y_pts(1) = cy + offset * sin(base_angle)

            do j = 1, seg_count + 1
                x_pts(j + 1) = x_pts(1) + r * cos(angle_start + &
                                 angle_span * real(j - 1, wp) / real(seg_count, wp))
                y_pts(j + 1) = y_pts(1) + r * sin(angle_start + &
                                 angle_span * real(j - 1, wp) / real(seg_count, wp))
            end do

            ! Add the plot data
            if (.not. allocated(plots)) allocate(plots(1000))
            if (plot_count >= size(plots)) then
                call resize_plots_array(plots, plot_count)
            end if

            plot_count = plot_count + 1
            allocate(plots(plot_count)%x(size(x_pts)))
            allocate(plots(plot_count)%y(size(y_pts)))
            plots(plot_count)%x = x_pts
            plots(plot_count)%y = y_pts

            if (present(labels) .and. i <= size(labels)) then
                plots(plot_count)%label = labels(i)
            end if

            deallocate(x_pts, y_pts)
            angle_start = angle_start + angle_span
        end do
    end subroutine figure_add_pie

    subroutine figure_add_polar(state, plots, plot_count, theta, r, label, linestyle, color)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout), allocatable :: plots(:)
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: theta(:), r(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)

        integer :: i, n
        real(wp), allocatable :: x(:), y(:)

        n = min(size(theta), size(r))
        allocate(x(n), y(n))

        do i = 1, n
            x(i) = r(i) * cos(theta(i))
            y(i) = r(i) * sin(theta(i))
        end do

        ! Add the plot data
        if (.not. allocated(plots)) allocate(plots(1000))
        if (plot_count >= size(plots)) then
            call resize_plots_array(plots, plot_count)
        end if

        plot_count = plot_count + 1
        allocate(plots(plot_count)%x(n))
        allocate(plots(plot_count)%y(n))
        plots(plot_count)%x = x
        plots(plot_count)%y = y

        if (present(label)) plots(plot_count)%label = label
        if (present(linestyle)) plots(plot_count)%linestyle = linestyle
        if (present(color)) plots(plot_count)%color = color

        deallocate(x, y)
    end subroutine figure_add_polar

    subroutine figure_add_step(state, plots, plot_count, x, y, where, label, linestyle, color)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout), allocatable :: plots(:)
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: where, label, linestyle
        real(wp), intent(in), optional :: color(3)

        integer :: i, n, new_n
        real(wp), allocatable :: x_step(:), y_step(:)
        character(len=32) :: step_where

        n = min(size(x), size(y))
        step_where = 'pre'
        if (present(where)) step_where = where

        new_n = 2 * n - 1
        allocate(x_step(new_n), y_step(new_n))

        select case(trim(step_where))
        case('pre')
            do i = 1, n
                x_step(2*i-1) = x(i)
                y_step(2*i-1) = y(i)
                if (i < n) then
                    x_step(2*i) = x(i+1)
                    y_step(2*i) = y(i)
                end if
            end do
        case('post')
            do i = 1, n
                x_step(2*i-1) = x(i)
                y_step(2*i-1) = y(i)
                if (i < n) then
                    x_step(2*i) = x(i)
                    y_step(2*i) = y(i+1)
                end if
            end do
        case default
            x_step(1:n) = x
            y_step(1:n) = y
            new_n = n
        end select

        ! Add the plot data
        if (.not. allocated(plots)) allocate(plots(1000))
        if (plot_count >= size(plots)) then
            call resize_plots_array(plots, plot_count)
        end if

        plot_count = plot_count + 1
        allocate(plots(plot_count)%x(new_n))
        allocate(plots(plot_count)%y(new_n))
        plots(plot_count)%x(1:new_n) = x_step(1:new_n)
        plots(plot_count)%y(1:new_n) = y_step(1:new_n)

        if (present(label)) plots(plot_count)%label = label
        if (present(linestyle)) plots(plot_count)%linestyle = linestyle
        if (present(color)) plots(plot_count)%color = color

        deallocate(x_step, y_step)
    end subroutine figure_add_step

    subroutine figure_add_stem(state, plots, plot_count, x, y, linestyle, marker, &
                              color, bottom_level, label)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout), allocatable :: plots(:)
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: linestyle, marker, label
        real(wp), intent(in), optional :: color(3), bottom_level

        integer :: i, n
        real(wp) :: base_level
        real(wp), allocatable :: x_stem(:), y_stem(:)

        n = min(size(x), size(y))
        base_level = 0.0_wp
        if (present(bottom_level)) base_level = bottom_level

        allocate(x_stem(3*n), y_stem(3*n))

        do i = 1, n
            x_stem(3*i-2) = x(i)
            y_stem(3*i-2) = base_level
            x_stem(3*i-1) = x(i)
            y_stem(3*i-1) = y(i)
            x_stem(3*i) = x(i)
            y_stem(3*i) = base_level
        end do

        ! Add the plot data
        if (.not. allocated(plots)) allocate(plots(1000))
        if (plot_count >= size(plots)) then
            call resize_plots_array(plots, plot_count)
        end if

        plot_count = plot_count + 1
        allocate(plots(plot_count)%x(3*n))
        allocate(plots(plot_count)%y(3*n))
        plots(plot_count)%x = x_stem
        plots(plot_count)%y = y_stem

        if (present(label)) plots(plot_count)%label = label
        if (present(linestyle)) plots(plot_count)%linestyle = linestyle
        if (present(color)) plots(plot_count)%color = color

        deallocate(x_stem, y_stem)
    end subroutine figure_add_stem

    subroutine figure_add_fill(state, plots, plot_count, x, y, color, alpha, label)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout), allocatable :: plots(:)
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: color(3), alpha
        character(len=*), intent(in), optional :: label

        ! Add the plot data
        if (.not. allocated(plots)) allocate(plots(1000))
        if (plot_count >= size(plots)) then
            call resize_plots_array(plots, plot_count)
        end if

        plot_count = plot_count + 1
        allocate(plots(plot_count)%x(size(x)))
        allocate(plots(plot_count)%y(size(y)))
        plots(plot_count)%x = x
        plots(plot_count)%y = y

        if (present(label)) plots(plot_count)%label = label
        if (present(color)) plots(plot_count)%color = color
    end subroutine figure_add_fill

    subroutine figure_add_fill_between(state, plots, plot_count, x, y1, y2, &
                                      color, alpha, label)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout), allocatable :: plots(:)
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y1(:), y2(:)
        real(wp), intent(in), optional :: color(3), alpha
        character(len=*), intent(in), optional :: label

        integer :: n
        real(wp), allocatable :: x_fill(:), y_fill(:)

        n = min(size(x), min(size(y1), size(y2)))
        allocate(x_fill(2*n), y_fill(2*n))

        x_fill(1:n) = x(1:n)
        y_fill(1:n) = y1(1:n)
        x_fill(n+1:2*n) = x(n:1:-1)
        y_fill(n+1:2*n) = y2(n:1:-1)

        ! Add the plot data
        if (.not. allocated(plots)) allocate(plots(1000))
        if (plot_count >= size(plots)) then
            call resize_plots_array(plots, plot_count)
        end if

        plot_count = plot_count + 1
        allocate(plots(plot_count)%x(2*n))
        allocate(plots(plot_count)%y(2*n))
        plots(plot_count)%x = x_fill
        plots(plot_count)%y = y_fill

        if (present(label)) plots(plot_count)%label = label
        if (present(color)) plots(plot_count)%color = color

        deallocate(x_fill, y_fill)
    end subroutine figure_add_fill_between

    subroutine figure_add_imshow(state, plots, plot_count, image, extent, &
                                colormap, vmin, vmax, aspect, interpolation)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout), allocatable :: plots(:)
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: image(:,:)
        real(wp), intent(in), optional :: extent(4), vmin, vmax
        character(len=*), intent(in), optional :: colormap, aspect, interpolation

        ! Add the plot data for image display
        if (.not. allocated(plots)) allocate(plots(1000))
        if (plot_count >= size(plots)) then
            call resize_plots_array(plots, plot_count)
        end if

        plot_count = plot_count + 1
        ! Store image data (implementation would depend on specific requirements)
    end subroutine figure_add_imshow

    subroutine resize_plots_array(plots, current_size)
        type(plot_data_t), intent(inout), allocatable :: plots(:)
        integer, intent(in) :: current_size

        type(plot_data_t), allocatable :: temp_plots(:)
        integer :: new_size

        new_size = current_size * 2
        allocate(temp_plots(new_size))
        temp_plots(1:current_size) = plots(1:current_size)
        deallocate(plots)
        call move_alloc(temp_plots, plots)
    end subroutine resize_plots_array

end module fortplot_figure_core_specialized_plots