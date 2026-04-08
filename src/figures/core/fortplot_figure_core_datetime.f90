submodule(fortplot_figure_core) fortplot_figure_core_datetime

    use fortplot_spec_frontend_adapters, only: figure_to_spec
    use fortplot_spec_rendering, only: render_spec_to_file, show_spec
    use fortplot_spec_types, only: spec_t
    implicit none

contains

    module subroutine add_plot_datetime(self, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self
        type(datetime_t), intent(in) :: x(:)
        real(wp), intent(in) :: y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)

        real(wp), allocatable :: x_seconds(:)
        integer :: i, n

        call self%set_xscale('date')

        n = size(x)
        allocate (x_seconds(n))
        do i = 1, n
            x_seconds(i) = real(datetime_to_unix_seconds(x(i)), wp)
        end do

        call core_add_plot(self%plots, self%state, x_seconds, y, label, linestyle, &
                           color, self%plot_count)
    end subroutine add_plot_datetime

    module subroutine savefig(self, filename, blocking)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        integer :: status

        call self%savefig_with_status(filename, status, blocking)
    end subroutine savefig

    module subroutine savefig_with_status(self, filename, status, blocking)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        logical, intent(in), optional :: blocking
        type(spec_t) :: spec

        call figure_to_spec(self, spec)
        call render_spec_to_file(spec, filename, status, self%state)
    end subroutine savefig_with_status

    module subroutine show(self, blocking)
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: blocking
        type(spec_t) :: spec

        call figure_to_spec(self, spec)
        call show_spec(spec, trim(self%state%backend_name), blocking, self%state)
    end subroutine show

    module subroutine set_xaxis_date_format(self, format)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: format

        call core_set_xaxis_date_format(self%state, format)
    end subroutine set_xaxis_date_format

    module subroutine set_yaxis_date_format(self, format)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: format

        call core_set_yaxis_date_format(self%state, format)
    end subroutine set_yaxis_date_format

end submodule fortplot_figure_core_datetime
