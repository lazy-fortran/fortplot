module fortplot_fortarray
    use, intrinsic :: iso_fortran_env, only: real64
    use fortarray_core, only: data_array_t
    use fortplot_figure_core, only: figure_t
    implicit none
    private

    public :: plot, contourf

contains

    subroutine plot(figure, array, stat)
        type(figure_t), intent(inout) :: figure
        type(data_array_t), intent(in) :: array
        integer, intent(out), optional :: stat
        real(real64), allocatable :: x(:)

        if (present(stat)) stat = 1
        if (.not. array%valid()) return
        if (array%rank() /= 1) return
        call dimension_values(array, 1, x)
        call figure%add_plot(x, array%values, label=trim(array%name))
        call figure%set_xlabel(trim(array%dims(1)))
        if (present(stat)) stat = 0
    end subroutine plot

    subroutine contourf(figure, array, stat)
        type(figure_t), intent(inout) :: figure
        type(data_array_t), intent(in) :: array
        integer, intent(out), optional :: stat
        real(real64), allocatable :: x(:), y(:), z(:, :)

        if (present(stat)) stat = 1
        if (.not. array%valid()) return
        if (array%rank() /= 2) return
        call dimension_values(array, 1, x)
        call dimension_values(array, 2, y)
        z = reshape(array%values, [array%shape(1), array%shape(2)])
        call figure%add_contourf(x, y, z, label=trim(array%name))
        call figure%set_xlabel(trim(array%dims(1)))
        call figure%set_ylabel(trim(array%dims(2)))
        if (present(stat)) stat = 0
    end subroutine contourf

    subroutine dimension_values(array, axis, values)
        type(data_array_t), intent(in) :: array
        integer, intent(in) :: axis
        real(real64), allocatable, intent(out) :: values(:)
        integer :: i

        if (allocated(array%coords)) then
            do i = 1, size(array%coords)
                if (array%coords(i)%name == array%dims(axis)) then
                    values = array%coords(i)%values
                    return
                end if
            end do
        end if
        allocate(values(array%shape(axis)))
        do i = 1, size(values)
            values(i) = real(i, real64)
        end do
    end subroutine dimension_values

end module fortplot_fortarray
