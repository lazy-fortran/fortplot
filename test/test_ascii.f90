program test_ascii
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    implicit none

    real(wp), dimension(100) :: x, sx, cx
    type(figure_t) :: ascii_fig
    integer :: i

    ! Generate some data:
    x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
    sx = sin(x)
    cx = cos(x)

    ! Create and configure ascii figure
    call ascii_fig%initialize(80, 24, "ascii")
    call ascii_fig%set_title("Sine and Cosine Functions (ascii)")

    ! Plot data
    call ascii_fig%add_plot(x, sx)
    call ascii_fig%add_plot(x, cx)

    ! Display to terminal
    call ascii_fig%show_ascii(x, sx, size(x), 'Sine and Cosine Functions (ascii)')

    print *, ""
    print *, "ascii terminal plot created successfully!"

end program test_ascii
