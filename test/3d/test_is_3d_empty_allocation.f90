program test_is_3d_empty_allocation
    !! Ensure empty z allocation does not trigger 3D detection
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: plot_data_t
    implicit none

    type(plot_data_t) :: plot

    allocate(plot%z(0))
    if (plot%is_3d()) then
        error stop 'Empty z array must not be detected as 3D'
    end if

    print *, 'PASS: empty z allocation is not treated as 3D'

end program test_is_3d_empty_allocation

