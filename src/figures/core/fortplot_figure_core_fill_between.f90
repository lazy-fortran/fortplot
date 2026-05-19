submodule(fortplot_figure_core) fortplot_figure_core_fill_between

    implicit none

contains

    module subroutine add_fill_between(self, x, y1, y2, where, color, alpha, &
                                       interpolate)
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: x(:), y1(:)
        real(wp), intent(in), optional :: y2(:)
        logical, intent(in), optional :: where (:)
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: alpha
        logical, intent(in), optional :: interpolate

        integer :: n
        real(wp), allocatable :: upper_vals(:), lower_vals(:)
        logical, allocatable :: mask_vals(:)
        logical :: has_mask

        n = size(x)
        if (.not. prepare_fill_between_values(n, y1, y2, where, upper_vals, &
                                              lower_vals, mask_vals, has_mask)) then
            call cleanup_fill_between_values(upper_vals, lower_vals, mask_vals)
            return
        end if

        if (present(interpolate)) then
            call log_warning('fill_between: interpolate option ignored')
        end if

        if (has_mask) then
            call add_prepared_fill_between(self, x, upper_vals, lower_vals, &
                                           mask=mask_vals, color=color, alpha=alpha)
        else
            call add_prepared_fill_between(self, x, upper_vals, lower_vals, &
                                           color=color, alpha=alpha)
        end if

        self%plot_count = self%state%plot_count

    end subroutine add_fill_between

    subroutine cleanup_fill_between_values(upper_vals, lower_vals, mask_vals)
        real(wp), allocatable, intent(inout) :: upper_vals(:), lower_vals(:)
        logical, allocatable, intent(inout) :: mask_vals(:)

        if (allocated(upper_vals)) deallocate (upper_vals)
        if (allocated(lower_vals)) deallocate (lower_vals)
        if (allocated(mask_vals)) deallocate (mask_vals)
    end subroutine cleanup_fill_between_values

    logical function prepare_fill_between_values(n, y1, y2, where, upper_vals, &
                                                 lower_vals, mask_vals, has_mask) &
        result(ok)
        integer, intent(in) :: n
        real(wp), contiguous, intent(in) :: y1(:)
        real(wp), intent(in), optional :: y2(:)
        logical, intent(in), optional :: where(:)
        real(wp), allocatable, intent(out) :: upper_vals(:), lower_vals(:)
        logical, allocatable, intent(out) :: mask_vals(:)
        logical, intent(out) :: has_mask

        ok = .false.
        has_mask = .false.
        if (n < 2) then
            call log_error('fill_between: need at least two points to form area')
            return
        end if
        if (size(y1) /= n) then
            call log_error('fill_between: y1 size mismatch')
            return
        end if

        allocate (upper_vals(n), lower_vals(n))
        upper_vals = y1
        if (.not. assign_fill_between_bound(n, y2, lower_vals, 'y2')) return
        if (.not. assign_fill_between_mask(n, where, mask_vals, has_mask)) return
        ok = .true.
    end function prepare_fill_between_values

    logical function assign_fill_between_bound(n, source, target, name) result(ok)
        integer, intent(in) :: n
        real(wp), intent(in), optional :: source(:)
        real(wp), intent(out) :: target(:)
        character(len=*), intent(in) :: name

        ok = .false.
        if (present(source)) then
            if (size(source) /= n) then
                call log_error('fill_between: '//name//' size mismatch')
                return
            end if
            target = source
        else
            target = 0.0_wp
        end if
        ok = .true.
    end function assign_fill_between_bound

    logical function assign_fill_between_mask(n, where, mask_vals, has_mask) &
        result(ok)
        integer, intent(in) :: n
        logical, intent(in), optional :: where(:)
        logical, allocatable, intent(out) :: mask_vals(:)
        logical, intent(out) :: has_mask

        ok = .false.
        has_mask = .false.
        if (.not. present(where)) then
            ok = .true.
            return
        end if
        if (size(where) /= n) then
            call log_error('fill_between: where mask size mismatch')
            return
        end if
        allocate (mask_vals(n))
        mask_vals = where
        if (.not. any(mask_vals)) then
            call log_warning('fill_between: mask excludes all data points')
            return
        end if
        has_mask = .true.
        ok = .true.
    end function assign_fill_between_mask

    subroutine add_prepared_fill_between(self, x, upper_vals, lower_vals, mask, &
                                         color, alpha)
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: x(:), upper_vals(:), lower_vals(:)
        logical, intent(in), optional :: mask(:)
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: alpha

        select case (merge(1, 0, present(mask)) + merge(2, 0, present(color)) + &
                     merge(4, 0, present(alpha)))
        case (0)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, plot_count=self%plot_count)
        case (1)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, mask=mask, &
                                       plot_count=self%plot_count)
        case (2)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, color_string=color, &
                                       plot_count=self%plot_count)
        case (3)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, mask=mask, &
                                       color_string=color, &
                                       plot_count=self%plot_count)
        case (4)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, alpha=alpha, &
                                       plot_count=self%plot_count)
        case (5)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, mask=mask, alpha=alpha, &
                                       plot_count=self%plot_count)
        case (6)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, color_string=color, &
                                       alpha=alpha, plot_count=self%plot_count)
        case default
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, mask=mask, &
                                       color_string=color, &
                                       alpha=alpha, plot_count=self%plot_count)
        end select
    end subroutine add_prepared_fill_between

end submodule fortplot_figure_core_fill_between
