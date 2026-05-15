module fortplot_pdf_axes_tick_data
    !! PDF axis tick data generation module
    !!
    !! Handles tick position calculation, label formatting, and subsampling.
    !! Pure computation module - no PDF context dependency.

    use iso_fortran_env, only: wp => real64
    use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
    use fortplot_tick_calculation, only: determine_decimals_from_ticks, &
                                         format_tick_value_consistent
    use fortplot_scales, only: apply_scale_transform
    implicit none
    private

    ! Public procedures
    public :: initialize_tick_arrays
    public :: generate_x_axis_ticks
    public :: generate_y_axis_ticks
    public :: apply_custom_axis_ticks
    public :: generate_axis_ticks_internal
    public :: subsample_ticks
    public :: fill_tick_positions_and_labels
    public :: handle_zero_range_ticks

contains

    subroutine initialize_tick_arrays(plot_width, plot_height, num_x_ticks, &
                                      num_y_ticks, &
                                      x_positions, y_positions, x_labels, y_labels)
        !! Initialize tick count and allocate arrays
        real(wp), intent(in) :: plot_width, plot_height
        integer, intent(out) :: num_x_ticks, num_y_ticks
        real(wp), allocatable, intent(out) :: x_positions(:), y_positions(:)
        character(len=50), allocatable, intent(out) :: x_labels(:), y_labels(:)

        integer, parameter :: TARGET_TICKS = 8

        ! Determine number of ticks using plot area dimensions
        num_x_ticks = min(TARGET_TICKS, max(2, int(plot_width/50.0_wp)))
        num_y_ticks = min(TARGET_TICKS, max(2, int(plot_height/40.0_wp)))

        ! Allocate arrays
        allocate (x_positions(num_x_ticks))
        allocate (y_positions(num_y_ticks))
        allocate (x_labels(num_x_ticks))
        allocate (y_labels(num_y_ticks))
    end subroutine initialize_tick_arrays

    subroutine generate_x_axis_ticks(data_min, data_max, num_ticks, plot_left, &
                                     plot_width, &
                                     positions, labels, scale_type, date_format, &
                                     symlog_threshold, custom_xticks, custom_xtick_labels)
        !! Generate X axis tick positions and labels
        real(wp), intent(in) :: data_min, data_max, plot_left, plot_width
        integer, intent(inout) :: num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=50), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale_type
        character(len=*), intent(in), optional :: date_format
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in), optional :: custom_xticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)

        call generate_axis_ticks_internal(data_min, data_max, num_ticks, plot_left, &
                                          plot_width, &
                                          positions, labels, scale_type, date_format, &
                                          symlog_threshold, 'x', &
                                          custom_xticks=custom_xticks, &
                                          custom_xtick_labels=custom_xtick_labels)
    end subroutine generate_x_axis_ticks

    subroutine generate_y_axis_ticks(data_min, data_max, num_ticks, plot_bottom, &
                                     plot_height, &
                                     positions, labels, scale_type, date_format, &
                                     symlog_threshold, custom_yticks, custom_ytick_labels)
        !! Generate Y axis tick positions and labels
        real(wp), intent(in) :: data_min, data_max, plot_bottom, plot_height
        integer, intent(inout) :: num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=50), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale_type
        character(len=*), intent(in), optional :: date_format
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in), optional :: custom_yticks(:)
        character(len=*), intent(in), optional :: custom_ytick_labels(:)

        call generate_axis_ticks_internal(data_min, data_max, num_ticks, plot_bottom, &
                                          plot_height, &
                                          positions, labels, scale_type, date_format, &
                                          symlog_threshold, 'y', &
                                          custom_yticks=custom_yticks, &
                                          custom_ytick_labels=custom_ytick_labels)
    end subroutine generate_y_axis_ticks

    subroutine apply_custom_axis_ticks(axis, custom_xticks, custom_xtick_labels, &
                                       custom_yticks, custom_ytick_labels, &
                                       data_min, data_max, plot_start, &
                                       plot_size, num_ticks, positions, labels, &
                                       scale_type, symlog_threshold)
        !! Apply custom tick positions/labels, converting data coords to plot area coords
        character, intent(in) :: axis
        real(wp), intent(in), optional :: custom_xticks(:), custom_yticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)
        character(len=*), intent(in), optional :: custom_ytick_labels(:)
        real(wp), intent(in) :: data_min, data_max, plot_start, plot_size
        integer, intent(inout) :: num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=50), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale_type
        real(wp), intent(in), optional :: symlog_threshold
        character(len=16) :: scale
        real(wp) :: thr
        integer :: used_ticks
        integer :: i
        logical :: use_custom
        real(wp) :: data_min_t, data_max_t

        use_custom = .false.
        if (axis == 'x') then
            if (present(custom_xticks) .and. present(custom_xtick_labels) .and. &
                size(custom_xticks) > 0 .and. size(custom_xticks) == &
                size(custom_xtick_labels)) use_custom = .true.
        else
            if (present(custom_yticks) .and. present(custom_ytick_labels) .and. &
                size(custom_yticks) > 0 .and. size(custom_yticks) == &
                size(custom_ytick_labels)) use_custom = .true.
        end if
        if (.not. use_custom) return

        scale = 'linear'; if (present(scale_type)) scale = scale_type
        thr = 1.0_wp; if (present(symlog_threshold)) thr = symlog_threshold
        if (axis == 'x') then
            used_ticks = min(num_ticks, size(custom_xticks))
            labels(1:used_ticks) = custom_xtick_labels(1:used_ticks)
        else
            used_ticks = min(num_ticks, size(custom_yticks))
            labels(1:used_ticks) = custom_ytick_labels(1:used_ticks)
        end if
        do i = 1, used_ticks
            if (axis == 'x') then
                positions(i) = custom_xticks(i)
            else
                positions(i) = custom_yticks(i)
            end if
        end do
        data_min_t = apply_scale_transform(data_min, scale, thr)
        data_max_t = apply_scale_transform(data_max, scale, thr)
        do i = 1, used_ticks
            positions(i) = apply_scale_transform(positions(i), scale, thr)
        end do
        if (data_max_t > data_min_t) then
            do i = 1, used_ticks
                positions(i) = plot_start + (positions(i) - data_min_t)/(data_max_t - &
                                      data_min_t)*plot_size
            end do
        else
            do i = 1, used_ticks
                positions(i) = plot_start + 0.5_wp*plot_size
            end do
        end if
        do i = used_ticks + 1, num_ticks
            labels(i) = ''
        end do
        num_ticks = used_ticks
    end subroutine apply_custom_axis_ticks

    subroutine generate_axis_ticks_internal(data_min, data_max, num_ticks, plot_start, &
                                            plot_size, &
                                            positions, labels, scale_type, &
                                            date_format, &
                                            symlog_threshold, axis, &
                                            custom_xticks, custom_xtick_labels, &
                                            custom_yticks, custom_ytick_labels)
        !! Internal helper to generate axis tick positions and labels
        real(wp), intent(in) :: data_min, data_max, plot_start, plot_size
        integer, intent(inout) :: num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=50), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale_type
        character(len=*), intent(in), optional :: date_format
        real(wp), intent(in), optional :: symlog_threshold
        character, intent(in) :: axis  ! 'x' or 'y'
        real(wp), intent(in), optional :: custom_xticks(:), custom_yticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)
        character(len=*), intent(in), optional :: custom_ytick_labels(:)

        real(wp) :: tvals(MAX_TICKS)
        integer :: nt
        character(len=16) :: scale
        real(wp) :: thr
        integer :: used_ticks

        call apply_custom_axis_ticks(axis, custom_xticks, custom_xtick_labels, &
                                     custom_yticks, custom_ytick_labels, &
                                     data_min, data_max, plot_start, plot_size, &
                                     num_ticks, positions, labels, scale_type, &
                                     symlog_threshold)
        if (num_ticks == 0) return

        scale = 'linear'
        if (present(scale_type)) scale = scale_type
        thr = 1.0_wp
        if (present(symlog_threshold)) thr = symlog_threshold

        call compute_scale_ticks(scale, data_min, data_max, thr, tvals, nt)
        if (nt <= 0) then
            num_ticks = min(num_ticks, size(positions))
            if (num_ticks <= 0) then
                num_ticks = 0
                return
            end if
            call handle_zero_range_ticks(data_min, num_ticks, plot_start + &
                                         plot_size*0.5_wp, &
                                         positions, labels, scale, date_format)
            return
        end if

        used_ticks = min(num_ticks, size(positions))
        if (used_ticks <= 0) then
            num_ticks = 0
            return
        end if

        ! Subsample ticks when more are generated than can be drawn,
        ! ensuring the first and last ticks always span the full data range.
        if (nt > used_ticks) then
            call subsample_ticks(tvals, nt, used_ticks, scale, thr)
            nt = used_ticks
        end if

        call fill_tick_positions_and_labels(tvals, nt, data_min, data_max, plot_start, &
                                            plot_size, &
                                            used_ticks, positions, labels, scale, thr, &
                                            date_format)
        num_ticks = used_ticks
    end subroutine generate_axis_ticks_internal

    subroutine subsample_ticks(tvals, nt, max_ticks, scale, threshold)
        !! Subsample ticks in transformed coordinate space for proportional visual spacing.
        real(wp), intent(inout) :: tvals(:)
        integer, intent(in) :: nt, max_ticks
        character(len=*), intent(in) :: scale
        real(wp), intent(in) :: threshold

        integer :: k, idx, best_idx
        real(wp) :: frac, target_t, best_dist, dist
        real(wp), allocatable :: tvals_t(:)

        if (nt <= max_ticks) return

        ! Transform ticks to display space
        allocate (tvals_t(nt))
        do k = 1, nt
            tvals_t(k) = apply_scale_transform(tvals(k), scale, threshold)
        end do

        ! For each subsampled slot, find the tick closest in transformed space
        do k = 2, max_ticks - 1
            frac = real(k - 1, wp) / real(max_ticks - 1, wp)
            target_t = tvals_t(1) + frac * (tvals_t(nt) - tvals_t(1))

            best_idx = 1
            best_dist = abs(tvals_t(1) - target_t)
            do idx = 2, nt
                dist = abs(tvals_t(idx) - target_t)
                if (dist < best_dist) then
                    best_dist = dist
                    best_idx = idx
                end if
            end do
            tvals(k) = tvals(best_idx)
        end do
        tvals(max_ticks) = tvals(nt)
        deallocate (tvals_t)
    end subroutine subsample_ticks

    subroutine fill_tick_positions_and_labels(tvals, nt, data_min, data_max, &
                                              plot_start, plot_size, &
                                              num_ticks, positions, labels, &
                                              scale, threshold, date_format)
        !! Fill tick positions and labels arrays
        real(wp), intent(in) :: tvals(:), data_min, data_max, plot_start, &
                                plot_size, threshold
        integer, intent(in) :: nt, num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=50), intent(out) :: labels(:)
        character(len=*), intent(in) :: scale
        character(len=*), intent(in), optional :: date_format

        real(wp) :: min_t, max_t, tv_t
        integer :: i, limit, decimals

        min_t = apply_scale_transform(data_min, scale, threshold)
        max_t = apply_scale_transform(data_max, scale, threshold)

        decimals = 0
        if (trim(scale) == 'linear' .and. nt >= 2) then
            decimals = determine_decimals_from_ticks(tvals, nt)
        end if

        limit = min(num_ticks, size(positions))
        do i = 1, limit
            if (i > nt) exit
            tv_t = apply_scale_transform(tvals(i), scale, threshold)
            if (max_t > min_t) then
                positions(i) = plot_start + (tv_t - min_t)/(max_t - min_t)*plot_size
            else
                positions(i) = plot_start + 0.5_wp*plot_size
            end if
            if (trim(scale) == 'linear') then
                labels(i) = adjustl(format_tick_value_consistent(tvals(i), decimals))
            else
                labels(i) = adjustl(format_tick_label(tvals(i), scale, &
                                                      date_format=date_format, &
                                                      data_min=data_min, &
                                                      data_max=data_max))
            end if
        end do
        do i = nt + 1, limit
            labels(i) = ''
        end do
    end subroutine fill_tick_positions_and_labels

    subroutine handle_zero_range_ticks(data_value, num_ticks, center_position, &
                                       positions, labels, scale_type, date_format)
        !! Handle ticks for zero or near-zero range data
        real(wp), intent(in) :: data_value, center_position
        integer, intent(in) :: num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=50), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale_type
        character(len=*), intent(in), optional :: date_format

        integer :: i

        do i = 1, num_ticks
            positions(i) = center_position
            if (present(scale_type)) then
                labels(i) = adjustl(format_tick_label(data_value, scale_type, &
                                                      date_format=date_format, &
                                                      data_min=data_value, &
                                                      data_max=data_value))
            else
                labels(i) = adjustl(format_tick_label(data_value, 'linear'))
            end if
        end do
    end subroutine handle_zero_range_ticks

end module fortplot_pdf_axes_tick_data
