submodule (fortplot_figure_core) fortplot_figure_core_ticks
    !! Custom tick label operations for figure_t
    !!
    !! Single Responsibility: Handle custom tick positions and labels
    !! Extracted to maintain file size compliance

    use fortplot_logging, only: log_error, log_warning
    implicit none

contains

    module subroutine set_xticks(self, positions, labels)
        !! Set custom x-axis tick positions and optionally labels
        !! If only positions provided, numeric labels are auto-generated
        !! If both provided, custom string labels are used
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: positions(:)
        character(len=*), intent(in), optional :: labels(:)

        integer :: n, i

        n = size(positions)
        if (n == 0) then
            call log_warning('set_xticks: empty positions array')
            return
        end if

        if (present(labels)) then
            if (size(labels) /= n) then
                call log_error('set_xticks: labels array size must match positions')
                return
            end if
        end if

        if (allocated(self%state%custom_xtick_positions)) &
            deallocate(self%state%custom_xtick_positions)
        if (allocated(self%state%custom_xtick_labels)) &
            deallocate(self%state%custom_xtick_labels)

        allocate(self%state%custom_xtick_positions(n))
        allocate(self%state%custom_xtick_labels(n))

        self%state%custom_xtick_positions = positions

        if (present(labels)) then
            do i = 1, n
                self%state%custom_xtick_labels(i) = trim(labels(i))
            end do
        else
            do i = 1, n
                write(self%state%custom_xtick_labels(i), '(G12.5)') positions(i)
                self%state%custom_xtick_labels(i) = &
                    adjustl(self%state%custom_xtick_labels(i))
            end do
        end if

        self%state%custom_xticks_set = .true.
        self%state%rendered = .false.
    end subroutine set_xticks

    module subroutine set_yticks(self, positions, labels)
        !! Set custom y-axis tick positions and optionally labels
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: positions(:)
        character(len=*), intent(in), optional :: labels(:)

        integer :: n, i

        n = size(positions)
        if (n == 0) then
            call log_warning('set_yticks: empty positions array')
            return
        end if

        if (present(labels)) then
            if (size(labels) /= n) then
                call log_error('set_yticks: labels array size must match positions')
                return
            end if
        end if

        if (allocated(self%state%custom_ytick_positions)) &
            deallocate(self%state%custom_ytick_positions)
        if (allocated(self%state%custom_ytick_labels)) &
            deallocate(self%state%custom_ytick_labels)

        allocate(self%state%custom_ytick_positions(n))
        allocate(self%state%custom_ytick_labels(n))

        self%state%custom_ytick_positions = positions

        if (present(labels)) then
            do i = 1, n
                self%state%custom_ytick_labels(i) = trim(labels(i))
            end do
        else
            do i = 1, n
                write(self%state%custom_ytick_labels(i), '(G12.5)') positions(i)
                self%state%custom_ytick_labels(i) = &
                    adjustl(self%state%custom_ytick_labels(i))
            end do
        end if

        self%state%custom_yticks_set = .true.
        self%state%rendered = .false.
    end subroutine set_yticks

    module subroutine set_xtick_labels(self, labels)
        !! Set custom x-axis tick labels for current tick positions
        !! Use after bar() or set_xticks() to provide categorical labels
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: labels(:)

        integer :: n, i

        n = size(labels)
        if (n == 0) then
            call log_warning('set_xtick_labels: empty labels array')
            return
        end if

        if (allocated(self%state%custom_xtick_labels)) &
            deallocate(self%state%custom_xtick_labels)

        allocate(self%state%custom_xtick_labels(n))

        do i = 1, n
            self%state%custom_xtick_labels(i) = trim(labels(i))
        end do

        if (.not. allocated(self%state%custom_xtick_positions)) then
            allocate(self%state%custom_xtick_positions(n))
            do i = 1, n
                self%state%custom_xtick_positions(i) = real(i, wp)
            end do
        end if

        self%state%custom_xticks_set = .true.
        self%state%rendered = .false.
    end subroutine set_xtick_labels

    module subroutine set_ytick_labels(self, labels)
        !! Set custom y-axis tick labels for current tick positions
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: labels(:)

        integer :: n, i

        n = size(labels)
        if (n == 0) then
            call log_warning('set_ytick_labels: empty labels array')
            return
        end if

        if (allocated(self%state%custom_ytick_labels)) &
            deallocate(self%state%custom_ytick_labels)

        allocate(self%state%custom_ytick_labels(n))

        do i = 1, n
            self%state%custom_ytick_labels(i) = trim(labels(i))
        end do

        if (.not. allocated(self%state%custom_ytick_positions)) then
            allocate(self%state%custom_ytick_positions(n))
            do i = 1, n
                self%state%custom_ytick_positions(i) = real(i, wp)
            end do
        end if

        self%state%custom_yticks_set = .true.
        self%state%rendered = .false.
    end subroutine set_ytick_labels

end submodule fortplot_figure_core_ticks
