program test_oo_api_docstring
    !! Test the OO API pattern shown in the fortplot module docstring
    !! Verifies that figure%add_plot, figure%legend, and figure%savefig
    !! are all publicly accessible type-bound procedures.
    !!
    !! Regression test for issue #1751.
    use fortplot
    use iso_fortran_env, only: real64
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none

    real(real64), allocatable :: x(:), y(:)
    type(figure_t) :: fig
    character(len=:), allocatable :: output_dir
    integer :: i

    call ensure_test_output_dir('oo_api_docstring', output_dir)

    allocate(x(50), y(50))
    x = [(real(i-1, real64)*0.2_real64, i=1, 50)]
    y = sin(x)

    ! Pattern from fortplot.f90 module docstring (issue #1751)
    call fig%initialize(800, 600)
    call fig%add_plot(x, y, label="data", linestyle='-')
    call fig%legend()
    call fig%savefig(trim(output_dir)//'test_oo_api.png')

    print *, "PASS: OO API from docstring compiles and runs (fixes #1751)"

end program test_oo_api_docstring
