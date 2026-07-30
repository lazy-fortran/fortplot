program fortplot_fortarray_consumer
    use, intrinsic :: iso_fortran_env, only: real64
    use fortarray, only: data_array_t, data_array
    use fortplot_figure_core, only: figure_t
    use fortplot_fortarray, only: plot
    implicit none

    type(data_array_t) :: field
    type(figure_t) :: figure
    integer :: stat

    field = data_array([1.0_real64, 2.0_real64], ["radius"], name="density")
    call figure%initialize()
    call plot(figure, field, stat)
    if (stat /= 0 .or. figure%plot_count /= 1) then
        error stop "installed fortplot::fortarray adapter failed"
    end if
end program fortplot_fortarray_consumer
